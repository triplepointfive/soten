{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.XglParserTest where

import Test.Hspec

import Linear
import Text.XML.HXT.Core

import Codec.Soten.Data.XglData
import Codec.Soten.Parser.XglParser

loadModelField f = runX $ readDocument [] "models/xgl/sample_official.xgl" >>> f

xglParserTest :: Spec
xglParserTest =
  describe "getModel" $
    context "Mandatory fields" $ do
      it "BackgroundColor" $ do
        vec <- loadModelField getBackgroud
        vec `shouldBe` [V3 1 1 1]
      it  "Lighting" $ do
        vec <- loadModelField getLighting
        vec `shouldBe`
          [ LightingTagAmbient (V3 0.0 0.0 0.0)
          , LightingTagDirectional
            { lightingTagDirectionalDirection = V3 0.302 (-0.302) (-0.905)
            , lightingTagDirectionalDiffuse   = V3 1.0 1.0 1.0
            , lightingTagDirectionalSpecular  = V3 0.1 0.1 0.1
            }
          ]
      it "Material" $ do
        vec <-  loadModelField getMaterial
        vec `shouldBe`
          [ Material
            { materialID = 0
            , materialAmbient  = V3 0.0 1.0 0.0
            , materialDiffuse  = V3 0.0 1.0 0.0
            , materialSpecular = Just (V3 1.0 1.0 1.0)
            , materialEmiss    = Just (V3 0.0 0.0 0.0)
            , materialShine    = Just 64.0
            , materialAlpha    = Just 1.0
            }
          ]
