{-# LANGUAGE OverloadedStrings #-}

module NLP.Postal.PostalSpec (spec) where

import Test.Hspec
import NLP.Postal
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (lookup)
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "setup" $ do
        it "general setup runs successfully" $ do
            r <- liftIO setup
            r `shouldBe` 1
        it "parser setup runs successfully" $ do
             r <- liftIO setupParser
             r `shouldBe` 1
        it "language classifier setup runs successfully" $ do
            r <- liftIO setupLanguageClassifier
            r `shouldBe` 1

    describe "expansion" $ do
        it "expands an address correctly" $ do
            defNormalizeOptions <- liftIO getDefaultNormalizeOptions
            r <- liftIO $ expandAddress defNormalizeOptions "11 Wall Street New York, NY"
            forM_ r $ \xp -> T.take 14 xp `shouldBe` "11 wall street"

    describe "parser" $ do
        it "parses an address correctly" $ do
            defParserOpts <- liftIO getAddressParserDefaultOptions
            r <- liftIO $ parseAddress defParserOpts "11 Wall Street New York, NY"
            lookup "house_number" r `shouldBe` Just "11"
            lookup "road" r `shouldBe` Just "wall street"
            lookup "city" r `shouldBe` Just "new york"
            lookup "state" r `shouldBe` Just "ny"

    describe "teardown" $ do
        it "language classifier teardown" $ do
            r <- liftIO tearDownLanguageClassifier
            r `shouldBe` ()
        it "parser teardown" $ do
            r <- liftIO tearDownParser
            r `shouldBe` ()
        it "teardown" $ do
            r <- liftIO tearDown
            r `shouldBe` ()
