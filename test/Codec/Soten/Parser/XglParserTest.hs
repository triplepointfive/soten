{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.XglParserTest where

import Test.Hspec

import Control.Lens
import Linear
import Text.XML.HXT.Core

import Codec.Soten.Data.XglData
import Codec.Soten.Parser.XglParser

loadModelField f = runX $ readDocument [] "models/xgl/sample_official.xgl" >>> f

xglParserTest :: Spec
xglParserTest = do
  describe "getModel" $ do
    context "Mandatory fields" $ do
      it "BackgroundColor" $ do
        vec <- loadModelField getBackgroud
        vec `shouldBe` [V3 1 1 1]
      it  "Lighting" $ do
        vec <- loadModelField getLighting
        vec `shouldBe`
          [ LightingTagAmbient (V3 0.0 0.0 0.0)
          , LightingTagDirectional (V3 0.302 (-0.302) (-0.905)) (V3 1.0 1.0 1.0) (V3 0.1 0.1 0.1)
          ]
