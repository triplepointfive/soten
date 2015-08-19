module Main where

import Data.Either

import Test.Hspec

import Codec.Soten.BaseImporter
import Codec.Soten.Importer
import Codec.Soten.Util

import Codec.Soten.Importer.Md2ImporterTest
import Codec.Soten.Importer.StlImporterTest
import Codec.Soten.Importer.XglImporterTest
import Codec.Soten.Parser.Md2ParserTest
import Codec.Soten.Parser.XglParserTest
import Codec.Soten.PostProcess.FixInfacingNormalsTest
import Codec.Soten.PostProcess.GenNormalsTest

main :: IO ()
main = hspec $ do
  xglParserTest
  md2ParserTest

  md2ImporterTest
  stlImporterTest
  xglImporterTest

  fixInfacingNormalsTest
  genNormalsTest

  describe "Importer" $ do
    context "Errors" $ do
      -- it "Unknown format" $ do
        -- eitherScene <- readModelFile "soten.cabal"
        -- (isLeft eitherScene) `shouldBe` True
      it "Non exist file" $ do
        eitherScene <- readModelFile "bla-bla-bla"
        (isLeft eitherScene) `shouldBe` True
    context "Loads formats" $ do
      it "STL" $ do
        eitherScene <- readModelFile "models/stl/block_ascii.stl"
        (isRight eitherScene) `shouldBe` True
