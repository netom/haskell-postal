{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : NLP.Postal
Description : Haskell binding for the libpostal library
Copyright   : 2018 Fábián Tamás László
License     : MIT
Maintainer  : Fábián Tamás László <giganetom@gmail.com>
Stability   : alpha

Provides an interface for the libpostal NLP library
-}
module NLP.Postal
    ( AddressParserOptions
    , NormalizeOptions
    , setup
    , setupParser
    , setupLanguageClassifier
    , getAddressParserDefaultOptions
    , getDefaultNormalizeOptions
    , parseAddress
    , expandAddress
    , tearDownParser
    , tearDownLanguageClassifier
    , tearDown
    ) where

import Data.Monoid ((<>))
import Foreign.C.Types
import qualified Language.C.Inline as C
import Language.C.Inline.Context as CX

import Foreign.Ptr

import Data.ByteString.Internal (fromForeignPtr)

import Foreign.ForeignPtr
import Control.Monad (forM, when)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

C.context (CX.baseCtx <> CX.bsCtx)

C.include "<libpostal/libpostal.h>"
C.include "<string.h>"

-- |Phantom type for a Ptr containing address parser options
data AddressParserOptions

-- |Phantom type for a Ptr containing options for the normalizer
data NormalizeOptions

foreign import ccall "stdlib.h &free" p_free :: FunPtr (Ptr a -> IO ())

-- |Calls libpostal_setup() to set up the library
-- Make sure you call this before anything else
setup :: IO Int
setup = fromIntegral <$> [C.exp| int { libpostal_setup() } |]

-- |Calls libpostal_setup_parser() to set up the address parser
-- Call this before trying to parse addresses
setupParser :: IO Int
setupParser = fromIntegral <$> [C.exp| int { libpostal_setup_parser() } |]

-- |Calls libpostal_setup_language_classifier() to set up the classifier
-- Call this before doing address normaliation
setupLanguageClassifier :: IO Int
setupLanguageClassifier =  fromIntegral <$> [C.exp| int { libpostal_setup_language_classifier() } |]

-- |Returns default parser options
-- returned by libpostal_get_address_parser_default_options()
getAddressParserDefaultOptions :: IO (Ptr AddressParserOptions)
getAddressParserDefaultOptions = castPtr <$> [C.block|
    void * {
        libpostal_address_parser_options_t options = libpostal_get_address_parser_default_options();
        void * hoptions = malloc(sizeof(options));
        memcpy(hoptions, &options, sizeof(options));
        return hoptions;
    } |]

-- |Returns default normalizer options
-- returned by libpostal_get_default_options()
getDefaultNormalizeOptions :: IO (Ptr NormalizeOptions)
getDefaultNormalizeOptions = castPtr <$> [C.block|
    void * {
        libpostal_normalize_options_t options = libpostal_get_default_options();
        void * hoptions = malloc(sizeof(options));
        memcpy(hoptions, &options, sizeof(options));
        return hoptions;
    } |]

-- |Parse an address
-- Calls libpostal_parse_address() to parse an address and returns the parsed
-- parts as a list of key-value tuples (an association list).
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

-- |Returns the expansion of an address
-- Calls libpostal_expand_address() to normalize an address and return the list
-- of normalized addresses.
expandAddress :: Ptr NormalizeOptions -> T.Text -> IO [T.Text]
expandAddress options address = do
    (numExpansions, expansions) <- C.withPtr $ \numExpansions ->
        [C.exp| char * * { libpostal_expand_address($bs-ptr:bsAddress, * (libpostal_normalize_options_t *) $(void * cOptions), $(size_t * numExpansions) ) } |]

    -- TODO: this also turns an array of (char *) to a list of Texts.
    result <- forM [0..numExpansions-1] $ \i -> do
        (xpLen, xpPtr) <- C.withPtr $ \len -> [C.block| char * {
            char * srcbuf = $(char * * expansions)[$(size_t i)];
            *$(size_t * len) = strlen(srcbuf);
            char * dstbuf = malloc(*$(size_t * len));
            memcpy(dstbuf, srcbuf, *$(size_t * len));
            return dstbuf;
        } |]

        xpFPtr <- newForeignPtr p_free xpPtr

        return $ TE.decodeUtf8 $ fromForeignPtr (castForeignPtr xpFPtr) 0 (fromIntegral xpLen)

    [C.exp| void { libpostal_expansion_array_destroy($(char * * expansions), $(size_t numExpansions)) } |]

    return result

    where
        bsAddress = TE.encodeUtf8 address
        cOptions = castPtr options

-- |Calls libpostal_teardown_parser()
tearDownParser :: IO ()
tearDownParser = [C.exp| void { libpostal_teardown_parser() } |]

-- |Calls libpostal_teardown_language_classifier()
tearDownLanguageClassifier :: IO ()
tearDownLanguageClassifier = [C.exp| void { libpostal_teardown_language_classifier() } |]

-- |Calls libpostal_teardown()
tearDown :: IO ()
tearDown = [C.exp| void { libpostal_teardown() } |]
