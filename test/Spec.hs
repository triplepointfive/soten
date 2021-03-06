module Main where

import Data.Either

import Test.Hspec

import Codec.Soten.Importer

import Codec.Soten.Importer.Md2ImporterTest
import Codec.Soten.Importer.StlImporterTest
import Codec.Soten.Importer.XglImporterTest
import Codec.Soten.Parser.Md2ParserTest
import Codec.Soten.Parser.ObjParserTest
import Codec.Soten.Parser.XglParserTest
import Codec.Soten.PostProcess.ConvertToLHTest
import Codec.Soten.PostProcess.FixInfacingNormalsTest
import Codec.Soten.PostProcess.FlipUVsTest
import Codec.Soten.PostProcess.FlipWindingOrderTest
import Codec.Soten.PostProcess.GenFaceNormalsTest
import Codec.Soten.PostProcess.TriangulateTest

main :: IO ()
main = hspec $ do
  md2ParserTest
  objParserTest
  xglParserTest

  md2ImporterTest
  stlImporterTest
  xglImporterTest

  convertToLHTest
  fixInfacingNormalsTest
  flipUVsTest
  flipWindingOrderTest
  genFaceNormalsTest
  triangulateTest

  describe "Importer" $ do
    context "Errors" $
      -- it "Unknown format" $ do
        -- eitherScene <- readModelFile "soten.cabal"
        -- (isLeft eitherScene) `shouldBe` True
      it "Non exist file" $ do
        eitherScene <- readModelFile "bla-bla-bla"
        isLeft eitherScene `shouldBe` True
    context "Loads formats" $
      it "STL" $ do
        eitherScene <- readModelFile "models/stl/block_ascii.stl"
        isRight eitherScene `shouldBe` True
