{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Importer.XglImporterTest where

import Test.Hspec

import Linear

import Codec.Soten.Data.XglData
import Codec.Soten.Importer.XglImporter

import Codec.Soten.Scene.Light

xglImporterTest :: Spec
xglImporterTest =
  describe "XGL importer" $
    context "Lights" $ do
      context "Directional" $ do
        let sceneLight = head (transformLights [ LightingTagDirectional (V3 0.5 0.5 0) (V3 1 1 1) (V3 0 1 0.7) ])
        it "Type" $
          _lightType sceneLight `shouldBe` LightDirectional
        it "Direction" $
          _lightDirection sceneLight `shouldBe` V3 0.5 0.5 0
        it "Diffuse color" $
          _lightColorDiffuse sceneLight `shouldBe` V3 1 1 1
        it "Specular color" $
          _lightColorSpecular  sceneLight `shouldBe` V3 0 1 0.7
      it "Filters out other types" $
        transformLights
          [ LightingTagAmbient (V3 0 1 0)
          , LightingTagSphereMap (V3 1 0 1) 3
          ]
        `shouldBe` []
