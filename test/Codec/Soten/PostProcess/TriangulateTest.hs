{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.TriangulateTest where

import Test.Hspec

import Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.Triangulate
import Codec.Soten.Scene
import Codec.Soten.Scene.Mesh

faces = V.fromList [Face (V.fromList [0, 1, 2, 3])]

validMesh = newMesh
    & meshFaces    .~ faces

triangulateTest :: Spec
triangulateTest = do
  describe "Triangulate post process" $ do
    context "Changes primitive types" $ do
      let scene = newScene & sceneMeshes  .~ V.singleton validMesh
          fixedSceneMesh = V.head $ (apply scene) ^. sceneMeshes
          primitiveTypes = fixedSceneMesh ^. meshPrimitiveTypes
      it "Removes primitive type polygon" $ do
        (PrimitiveTriangle `V.elem` primitiveTypes ) `shouldBe` True
      it "Adds primitive type triange" $ do
        (PrimitivePolygone `V.elem` primitiveTypes ) `shouldBe` False
