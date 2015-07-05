{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.XglParserTest where

import Test.Hspec

import Control.Lens
import Linear
import Text.XML.HXT.Core

import Codec.Soten.Data.XglData
import Codec.Soten.Parser.XglParser

xglParserTest :: Spec
xglParserTest = do
  describe "getModel" $ do
    context "Mandatory fields" $ do
      it "BackgroundColor" $ do
        vec <- runX $ readDocument [] "models/xgl/sample_official.xgl" >>> getBackgroud
        vec `shouldBe` [V3 1 1 1]
