{-# LANGUAGE OverloadedStrings #-}

module Main where

import NLP.Postal

main :: IO ()
main = do
    putStrLn "Setting up libpostal..."
    setup >>= print

    putStrLn "Setting up the parser..."
    setupParser >>= print

    defOpts <- getAddressParserDefaultOptions

    parsedAddress <- parseAddress defOpts "1111 Budapest, Kiss Ágoston u. 4/a"
    print parsedAddress

    tearDownParser
    tearDown
