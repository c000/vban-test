{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UtilSpec
  ( spec
  )
where

import           Import
import           Util
import           Test.Hspec
import           Test.Hspec.QuickCheck

import qualified RIO.Text                      as T

spec :: Spec
spec = do
  describe "parseDatagramAddr" $ do
    it "aaa" $ T.break (== ':') "a:b" `shouldBe` ("a", ":b")
