module Main where

import Test.Hspec

import Codec.Soten.BaseImporter
import Codec.Soten.Importer.StlImporter
import Codec.Soten.Util

main :: IO ()
main = hspec $ do
  describe "Importers" $ do
    context "STL" $ do
      it "checks extension of ascii file" $ do
        status <- canImport StlImporter "models/stl/block_ascii.stl" CheckExtension
        status `shouldBe` True
      it "checks header of ascii file" $ do
        status <- canImport StlImporter "models/stl/block_ascii.stl" CheckHeader
        status `shouldBe` True
      it "checks extension of binary file" $ do
        status <- canImport StlImporter "models/stl/block_bin.stl" CheckExtension
        status `shouldBe` True
      it "checks header of binary file" $ do
        status <- canImport StlImporter "models/stl/block_bin.stl" CheckHeader
        status `shouldBe` True

