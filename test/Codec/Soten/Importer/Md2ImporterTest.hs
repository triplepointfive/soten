{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Importer.Md2ImporterTest where

import Test.Hspec

import Codec.Soten.Scene.Material
import Codec.Soten.BaseImporter
import Codec.Soten.Importer.Md2Importer
import Codec.Soten.Util

md2ImporterTest :: Spec
md2ImporterTest = do
  describe "MD2 Importer" $ do
    context "CanImport" $ do
      it "Checks extension of file" $ do
        status <- canImport Md2Importer "models/md2/phoenix_ugv.md2" CheckExtension
        status `shouldBe` True
      it "Checks header of file" $ do
        status <- canImport Md2Importer "models/md2/phoenix_ugv.md2" CheckHeader
        status `shouldBe` True
