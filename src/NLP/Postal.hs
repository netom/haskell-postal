{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.Postal where

import Data.Monoid ((<>))
import Foreign.C.Types
import qualified Language.C.Inline as C
import Language.C.Inline.Context as CX

import Foreign.Ptr

import Data.ByteString.Internal (fromForeignPtr)

import Foreign.ForeignPtr
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

C.context (CX.baseCtx <> CX.bsCtx)

C.include "<libpostal/libpostal.h>"
C.include "<string.h>"

data AddressParserOptions
data Options
data AddressParserResponse
data Expansions

foreign import ccall "stdlib.h &free"
    p_free :: FunPtr (Ptr a -> IO ())

-- libpostal_setup()
setup :: IO Int
setup = fromIntegral <$> [C.exp| int { libpostal_setup() } |]

-- libpostal_setup_parser()
setupParser :: IO Int
setupParser = fromIntegral <$> [C.exp| int { libpostal_setup_parser() } |]

-- libpostal_setup_language_classifier()
setupLanguageClassifier :: IO Int
setupLanguageClassifier =  fromIntegral <$> [C.exp| int { libpostal_setup_language_classifier() } |]

-- libpostal_get_address_parser_default_options()
getAddressParserDefaultOptions :: IO (Ptr AddressParserOptions)
getAddressParserDefaultOptions = castPtr <$> [C.block|
    void * {
        libpostal_address_parser_options_t options = libpostal_get_address_parser_default_options();
        void * hoptions = malloc(sizeof(options));
        memcpy(hoptions, &options, sizeof(options));
        return hoptions;
    } |]

-- libpostal_get_default_options()
getDefaultOptions :: IO (Ptr Options)
getDefaultOptions = castPtr <$> [C.block|
    void * {
        libpostal_normalize_options_t options = libpostal_get_default_options();
        void * hoptions = malloc(sizeof(options));
        memcpy(hoptions, &options, sizeof(options));
        return hoptions;
    } |]

-- libpostal_parse_address()
parseAddress :: Ptr AddressParserOptions -> T.Text -> IO [(T.Text, T.Text)]
parseAddress options address = do
        response <- [C.exp| void * { libpostal_parse_address($bs-ptr:bsAddress, * (libpostal_address_parser_options_t *) $(void * castedOptions)) } |]

        when (response == nullPtr) $ fail "libpostal_parse_address returned NULL a pointer"

        numComponents <- [C.exp| int { ((libpostal_address_parser_response_t *) $(void * response))->num_components } |]

        -- TODO: zero length label
        -- TODO: NULL == malloc()
        -- TODO: put C functions in actual C files, call those with C.exp-s
        result <- forM [0..numComponents-1] $ \i -> do
            (labelLen, labelPtr) <- C.withPtr $ \len -> [C.block| char * {
                char * srcbuf = ((libpostal_address_parser_response_t *) $(void * response))->labels[$(int i)];
                *$(size_t * len) = strlen(srcbuf);
                char * dstbuf = malloc(*$(size_t * len));
                memcpy(dstbuf, srcbuf, *$(size_t * len));
                return dstbuf;
            } |]

            labelFPtr <- newForeignPtr p_free labelPtr

            (compLen, compPtr) <- C.withPtr $ \len -> [C.block| char * {
                char * srcbuf = ((libpostal_address_parser_response_t *) $(void * response))->components[$(int i)];
                *$(size_t * len) = strlen(srcbuf);
                char * dstbuf = malloc(*$(size_t * len));
                memcpy(dstbuf, srcbuf, *$(size_t * len));
                return dstbuf;
            } |]

            compFPtr <- newForeignPtr p_free compPtr

            return
                ( TE.decodeUtf8 $ fromForeignPtr (castForeignPtr labelFPtr) 0 (fromIntegral labelLen)
                , TE.decodeUtf8 $ fromForeignPtr (castForeignPtr compFPtr) 0 (fromIntegral compLen)
                )

        [C.exp| void { libpostal_address_parser_response_destroy((libpostal_address_parser_response_t *) $(void * response)) } |]

        return result

        where
            castedOptions = castPtr options
            bsAddress = TE.encodeUtf8 address

-- Returns an expansion
-- withPtr !!!
-- TODO: process expansions (char**) to [Text]
--
expandAddress :: Ptr Options -> Ptr Int -> T.Text -> IO (Ptr Expansions)
expandAddress options numExpansions address = do
    ccptr <- [C.exp| void * { libpostal_expand_address($bs-ptr:bsAddress, * (libpostal_normalize_options_t *) $(void * cOptions), $(size_t * cNumExpansions) ) } |]
    return $ castPtr ccptr
    where
        bsAddress = TE.encodeUtf8 address
        cNumExpansions = castPtr numExpansions
        cOptions = castPtr options

-- libpostal_expansion_array_destroy()
expansionArrayDestroy :: Ptr Expansions -> Int -> IO ()
expansionArrayDestroy expansions numExpansions =
        [C.exp| void { libpostal_expansion_array_destroy((char * *) $(void * cExpansions), $(int cNumExpansions)) } |]
        where
            cExpansions = castPtr expansions
            cNumExpansions = fromIntegral numExpansions

-- libpostal_teardown_parser()
tearDownParser :: IO ()
tearDownParser = [C.exp| void { libpostal_teardown_parser() } |]

-- libpostal_teardown_language_classifier()
tearDownLanguageClassifier :: IO ()
tearDownLanguageClassifier = [C.exp| void { libpostal_teardown_language_classifier() } |]

-- libpostal_teardown()
tearDown :: IO ()
tearDown = [C.exp| void { libpostal_teardown() } |]
