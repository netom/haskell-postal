{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module NLP.Postal where

import Data.Monoid ((<>))
import Foreign.C.Types
import qualified Language.C.Inline as C
import Language.C.Inline.Context as CX

import Foreign.Ptr (Ptr, castPtr)

import Data.ByteString.Internal (fromForeignPtr)

import Foreign.ForeignPtr

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

C.context (CX.baseCtx <> CX.bsCtx)

C.include "<libpostal/libpostal.h>"
C.include "<string.h>"

data AddressParserOptions
data Options
data AddressParserResponse
data Expansions

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
parseAddress :: Ptr AddressParserOptions -> T.Text -> IO (Ptr AddressParserResponse)
parseAddress options address =
        castPtr <$> [C.exp| void * { libpostal_parse_address($bs-ptr:bsAddress, * (libpostal_address_parser_options_t *) $(void * castedOptions)) } |]
        where
            castedOptions = castPtr options
            bsAddress = TE.encodeUtf8 address

-- Returns the number of components of an address parser response
responseNumComponents :: Ptr AddressParserResponse -> IO Int
responseNumComponents response =
        fromIntegral <$> [C.exp| int { ((libpostal_address_parser_response_t *) $(void * castedResponse))->num_components } |]
        where
            castedResponse = castPtr response

-- Returns a response label of an address parser response
responseLabel :: Ptr AddressParserResponse -> Int -> IO T.Text
responseLabel response i = do
    ccptr <- [C.exp| char * { ((libpostal_address_parser_response_t *) $(void * castedResponse))->labels[$(int ci)] } |]
    strlen <- fromIntegral <$> [C.exp| int { strlen($(char * ccptr)) } |]
    let w8ptr = castPtr ccptr
    fw8ptr <- newForeignPtr_ w8ptr
    return $ TE.decodeUtf8 $ fromForeignPtr fw8ptr 0 strlen
    where
        castedResponse = castPtr response
        ci = fromIntegral i

-- Returns a response component of an address parser response
responseComponent :: Ptr AddressParserResponse -> Int -> IO T.Text
responseComponent response i = do
    ccptr <- [C.exp| char * { ((libpostal_address_parser_response_t *) $(void * castedResponse))->components[$(int ci)] } |]
    strlen <- fromIntegral <$> [C.exp| int { strlen($(char * ccptr)) } |]
    let w8ptr = castPtr ccptr
    fw8ptr <- newForeignPtr_ w8ptr
    return $ TE.decodeUtf8 $ fromForeignPtr fw8ptr 0 strlen
    where
        castedResponse = castPtr response
        ci = fromIntegral i

-- Returns an expansion
-- withPtr !!!
expandAddress :: Ptr Options -> Ptr Int -> T.Text -> IO (Ptr Expansions)
expandAddress options numExpansions address = do
    ccptr <- [C.exp| void * { libpostal_expand_address($bs-ptr:bsAddress, * (libpostal_normalize_options_t *) $(void * cOptions), $(size_t * cNumExpansions) ) } |]
    return $ castPtr ccptr
    where
        bsAddress = TE.encodeUtf8 address
        cNumExpansions = castPtr numExpansions
        cOptions = castPtr options

-- TODO: process expansions (char**) to [Text]
-- TODO: write safet functions (bounds checks, lists of results, etc.)

-- libpostal_address_parser_response_destroy()
responseDestroy :: Ptr AddressParserResponse -> IO ()
responseDestroy response =
        [C.exp| void { libpostal_address_parser_response_destroy((libpostal_address_parser_response_t *) $(void * castedResponse)) } |]
        where
            castedResponse = castPtr response

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
