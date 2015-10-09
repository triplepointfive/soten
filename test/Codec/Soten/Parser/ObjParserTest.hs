{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.ObjParserTest where

import Test.Hspec

import Codec.Soten.Data.ObjData
import Codec.Soten.Parser.ObjParser

file, cube :: String
file = unlines
    [ "# Blender v2.57 (sub 0) OBJ File: ''"
    , "# www.blender.org"
    , "o Cube_Cube.001"
    , "v -1.000000 1.000000 0.000000  "
    , "s off"
    , "v 1.000000 1.000000 0.000000   "
    , "v -1.000000 -1.000000 0.000000 "
    , "v 1.000000 -1.000000 0.000000  "
    , "vt 1.000000 1.000000           "
    , "vt 0.000000 1.000000           "
    , "vt 0.000000 0.000000           "
    , "vt 1.000000 0.000000           "
    , "usemtl (null)"
    , "f 2/1 4/4 1/2"
    , "f 4/4 3/3 1/2"
    ]

cube = unlines
    [ "v 0.000000 2.000000 2.000000"
    , "v 0.000000 0.000000 2.000000"
    , "v 2.000000 0.000000 2.000000"
    , "v 2.000000 2.000000 2.000000"
    , "v 0.000000 2.000000 0.000000"
    , "v 0.000000 0.000000 0.000000"
    , "v 2.000000 0.000000 0.000000"
    , "v 2.000000 2.000000 0.000000"
    , "f 1 2 3 4"
    , "f 8 7 6 5"
    , "f 4 3 7 8"
    , "f 5 1 4 8"
    , "f 5 6 2 1"
    , "f 2 6 7 3"
    ]

objParserTest :: Spec
objParserTest =
  describe "OBJ parser" $
    context "Cube" $ do
      let model = getModel cube
      it "Tokens number" $
        length model `shouldBe` 14
      it "Vertex" $
        head model `shouldBe` Vertex 0 2 2
      it "Face" $
        last model `shouldBe` Face [2, 6, 7, 3]
