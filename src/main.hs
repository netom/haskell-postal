{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import NLP.Postal

import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Setup"
    setup >>= print

    putStrLn "Setup parser"
    setupParser >>= print

    defOpts <- getAddressParserDefaultOptions
    rsp <- parseAddress defOpts "1111 Budapest, Kiss Ágoston u. 4/a"
    ncomps <- responseNumComponents rsp

    putStrLn ""
    forM_ [0..ncomps-1] $ \i -> do
        l <- responseLabel rsp i
        c <- responseComponent rsp i
        putStrLn $ T.unpack $ l <> ": " <> c
    putStrLn ""

    putStrLn "Teardown parser"
    tearDownParser

    putStrLn "Teardown"
    tearDown
