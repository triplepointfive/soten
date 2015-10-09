{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.Md2ParserTest where

import Test.Hspec

import qualified Data.ByteString as BS

import Codec.Soten.Data.Md2Data
import Codec.Soten.Parser.Md2Parser

md2ParserTest :: Spec
md2ParserTest =
  describe "MD2 parser" $ do
    file <- runIO $ BS.readFile "models/md2/phoenix_ugv.md2"
    context "Header" $ do
      let h = header $ load file
      it "Version" $
        version h `shouldBe` 8
      it "Ident" $
        ident h `shouldBe` 844121161
    context "Skins" $
      it "Name" $
        texture (head $ skins $ load file) `shouldBe` "phoenix.pcx"
