module Main where

import Data.Either

import Test.Hspec

import Codec.Soten.BaseImporter
import Codec.Soten.Importer
import Codec.Soten.Importer.StlImporter
import Codec.Soten.Util

import Codec.Soten.Parser.XglParserTest
import Codec.Soten.Importer.XglImporterTest

main :: IO ()
main = hspec $ do
  xglParserTest
  xglImporterTest

  describe "Importer" $ do
    context "Errors" $ do
      it "Unknown format" $ do
        eitherScene <- readModelFile "soten.cabal"
        (isLeft eitherScene) `shouldBe` True
      it "Non exist file" $ do
        eitherScene <- readModelFile "bla-bla-bla"
        (isLeft eitherScene) `shouldBe` True
    context "Loads formats" $ do
      it "STL" $ do
        eitherScene <- readModelFile "models/stl/block_ascii.stl"
        (isRight eitherScene) `shouldBe` True

  describe "Importers" $ do
    context "STL" $ do
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
