{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.XglParserTest where

import Test.Hspec

import Control.Lens
import Linear

import Codec.Soten.Data.XglData
import Codec.Soten.Parser.XglParser

baseModel = "<WORLD><BACKGROUND><BACKCOLOR>1.0,1.0,1.0</BACKCOLOR></BACKGROUND></WORLD>"

xglParserTest :: Spec
xglParserTest = do
  describe "getModel" $ do
    context "Mandatory fields" $ do
      it "BackgroundColor" $ do
        (getModel baseModel ^. modelBackgroundColor) `shouldBe` V3 1 1 1
