{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module NLP.Postal where

import Data.Monoid ((<>))
import Foreign.C.Types
import qualified Language.C.Inline as C
--import qualified Language.C.Types as CT
import Language.C.Inline.Context as CX

import Foreign.Ptr (Ptr, castPtr)

import qualified Data.ByteString as BS
import Data.ByteString.Internal (fromForeignPtr)

import Foreign.ForeignPtr

C.context (CX.baseCtx <> CX.bsCtx)

C.include "<libpostal/libpostal.h>"
C.include "<string.h>"

data AddressParserOptions
data AddressParserResponse

setup :: IO C.CInt
setup = [C.exp| int { libpostal_setup() } |]

setupParser :: IO C.CInt
setupParser = [C.exp| int { libpostal_setup_parser() } |]

getAddressParserDefaultOptions :: IO (Ptr AddressParserOptions)
getAddressParserDefaultOptions = castPtr <$> [C.block|
    void * {
        libpostal_address_parser_options_t options = libpostal_get_address_parser_default_options();
        void * hoptions = malloc(sizeof(options));
        memcpy(hoptions, &options, sizeof(options));
        return hoptions;
    } |]

parseAddress :: Ptr AddressParserOptions -> BS.ByteString -> IO (Ptr AddressParserResponse)
parseAddress options address =
        castPtr <$> [C.exp| void * { libpostal_parse_address($bs-ptr:address, * (libpostal_address_parser_options_t *) $(void * castedOptions)) } |]
        where
            castedOptions = castPtr options

responseNumComponents :: Ptr AddressParserResponse -> IO Int
responseNumComponents response =
        fromIntegral <$> [C.exp| int { ((libpostal_address_parser_response_t *) $(void * castedResponse))->num_components } |]
        where
            castedResponse = castPtr response

responseDestroy :: Ptr AddressParserResponse -> IO () -- libpostal_address_parser_response_destroy
responseDestroy response =
        [C.exp| void { libpostal_address_parser_response_destroy((libpostal_address_parser_response_t *) $(void * castedResponse)) } |]
        where
            castedResponse = castPtr response

responseLabels :: Ptr AddressParserResponse -> Int -> IO BS.ByteString
responseLabels response i = do
    ccptr <- [C.exp| char * { ((libpostal_address_parser_response_t *) $(void * castedResponse))->labels[$(int ci)] } |]
    strlen <- fromIntegral <$> [C.exp| int { strlen($(char * ccptr)) } |]
    let w8ptr = castPtr ccptr
    fw8ptr <- newForeignPtr_ w8ptr
    return $ fromForeignPtr fw8ptr 0 strlen
    where
        castedResponse = castPtr response
        ci = fromIntegral i

responseComponents :: Ptr AddressParserResponse -> Int -> IO BS.ByteString
responseComponents response i = do
    ccptr <- [C.exp| char * { ((libpostal_address_parser_response_t *) $(void * castedResponse))->components[$(int ci)] } |]
    strlen <- fromIntegral <$> [C.exp| int { strlen($(char * ccptr)) } |]
    let w8ptr = castPtr ccptr
    fw8ptr <- newForeignPtr_ w8ptr
    return $ fromForeignPtr fw8ptr 0 strlen
    where
        castedResponse = castPtr response
        ci = fromIntegral i

tearDownParser :: IO ()
tearDownParser = [C.exp| void { libpostal_teardown_parser() } |]

tearDown :: IO ()
tearDown = [C.exp| void { libpostal_teardown() } |]
