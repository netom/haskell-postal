{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import NLP.Postal

import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ die "Usage: haskell-postal <address>"

    let address = T.pack $ head args

    putStrLn "Setting up libpostal..."
    setup >>= \x -> when (x == 0) $ die "FAIL"

    putStrLn "Setting up the parser..."
    setupParser >>= \x -> when (x == 0) $ die "FAIL"

    putStrLn "Setting up the language classifier..."
    setupLanguageClassifier >>= \x -> when (x == 0) $ die "FAIL"

    defParserOpts <- getAddressParserDefaultOptions
    defNormalizeOptions <- getDefaultNormalizeOptions

    parseAddress defParserOpts address >>= print
    expandAddress defNormalizeOptions address >>= print

    tearDownLanguageClassifier
    tearDownParser
    tearDown
