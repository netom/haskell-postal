module Main where

import NLP.Postal

main :: IO ()
main = do
    putStrLn "Setup"
    setup

    putStrLn "Setup parser"
    setupParser

    putStrLn "Teardown parser"
    tearDownParser

    putStrLn "Teardown"
    tearDown
