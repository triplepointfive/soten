{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Importer.StlImporterTest where

import Test.Hspec

import Codec.Soten.BaseImporter
import Codec.Soten.Importer.StlImporter
import Codec.Soten.Util

stlImporterTest :: Spec
stlImporterTest = do
  describe "STL Importer" $ do
    context "CanImport" $ do
      it "Checks extension of ascii file" $ do
        status <- canImport StlImporter "models/stl/block_ascii.stl" CheckExtension
        status `shouldBe` True
      it "Checks header of ascii file" $ do
        status <- canImport StlImporter "models/stl/block_ascii.stl" CheckHeader
        status `shouldBe` True
      it "Checks extension of binary file" $ do
        status <- canImport StlImporter "models/stl/block_bin.stl" CheckExtension
        status `shouldBe` True
      it "Checks header of binary file" $ do
        status <- canImport StlImporter "models/stl/block_bin.stl" CheckHeader
        status `shouldBe` True
