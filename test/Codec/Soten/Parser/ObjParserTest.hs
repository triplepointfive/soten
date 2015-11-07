{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.Parser.ObjParserTest where

import Test.Hspec

import Codec.Soten.Data.ObjData
import Codec.Soten.Parser.ObjParser

quad, cube :: String
quad = unlines
    [ "# Blender v2.57 (sub 0) OBJ File: ''"
    , "# www.blender.org"
    , "o Cube_Cube.001"
    , "v -1.000000 1.000000 0.000000  "
    , "v 1.000000 1.000000 0.000000   "
    , "v -1.000000 -1.000000 0.000000 "
    , "v 1.000000 -1.000000 0.000000  "
    , "vt 1.000000 1.000000           "
    , "vt 0.000000 1.000000           "
    , "vt 0.000000 0.000000           "
    , "vt 1.000000 0.000000           "
    , "usemtl (null)"
    , "s off"
    , "f 4/4 3/3 1/2"
    , "f 2/1 4/4 1/2"
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

sphere = unlines
    [ "mtllib sphere.mtl"
    , "vn 0.098012 -0.995138 -0.009653"
    , "vn 0.290166 -0.956550 -0.028578"
    , "vn 0.470890 -0.880972 -0.046377"
    , "f 32//1 2//1 482//1"
    , "f 482//2 2//2 3//2"
    , "f 482//2 3//2 481//2"
    ]

objParserTest :: Spec
objParserTest =
  describe "OBJ parser" $ do
    context "Verts & faces only" $ do
      let Right tokens = tokenize cube
      it "Tokens number" $
        length tokens `shouldBe` 14
      it "Vertex" $
        head tokens `shouldBe` Vertex 0 2 2
      it "Face" $
        last tokens `shouldBe` Face [2, 6, 7, 3] [] []
    context "Verts & faces & textures" $ do
      let Right tokens = tokenize quad
      it "Tokens number" $
        length tokens `shouldBe` 11
      it "Object tag" $
        head tokens `shouldBe` Object "Cube_Cube.001"
      it "Face with versts and texture coords" $
        last tokens `shouldBe` Face [2, 4, 1] [1, 4, 2] []
    context "Normals & faces" $ do
      let Right tokens = tokenize sphere
      it "Tokens number" $
        length tokens `shouldBe` 6
      it "Vertex normal tag" $
        head tokens `shouldBe` Normal 0.098012 (-0.995138) (-0.009653)
      it "Face tag without texture coordinates" $
        last tokens `shouldBe` Face [482, 3, 481] [] [2, 2, 2]
