{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Importer.XglImporterTest where

import Test.Hspec

import Linear

import Codec.Soten.Data.XglData
import Codec.Soten.Importer.XglImporter

import Codec.Soten.Scene.Light

xglImporterTest :: Spec
xglImporterTest = do
  describe "Transformation" $ do
    context "Lights" $ do
      context "Directional" $ do
        let sceneLight = head (transformLights [ LightingTagDirectional (V3 0.5 0.5 0) (V3 1 1 1) (V3 0 1 0.7) ])
        it "Type" $ do
          _lightType sceneLight `shouldBe` LightDirectional
        it "Direction" $ do
          _lightDirection sceneLight `shouldBe` V3 0.5 0.5 0
        it "Diffuse color" $ do
          _lightColorDiffuse sceneLight `shouldBe` V3 1 1 1
        it "Specular color" $ do
          _lightColorSpecular  sceneLight `shouldBe` V3 0 1 0.7
      it "Filters out other types" $ do
        transformLights
          [ LightingTagAmbient (V3 0 1 0)
          , LightingTagSphereMap (V3 1 0 1) 3
          ]
        `shouldBe` []
