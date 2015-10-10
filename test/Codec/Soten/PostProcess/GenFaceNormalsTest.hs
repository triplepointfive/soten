{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.GenFaceNormalsTest where

import Test.Hspec

import Control.Lens ((&), (^.), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.GenFaceNormals
import Codec.Soten.Scene
import Codec.Soten.Scene.Mesh

scene :: Mesh -> Scene
scene mesh = newScene & sceneMeshes .~ V.singleton mesh

isDummyNormal :: V3 Float -> Bool
isDummyNormal (V3 x y z) = all isNaN [x, y, z]

genFaceNormalsTest :: Spec
genFaceNormalsTest = do
  describe "Generate normals post process" $ do
    context "Ignores mesh" $ do
      it "With normals" $ do
        let mesh = newMesh
              & meshNormals        .~ V.replicate 3 (V3 1 1 1)
              & meshVertices       .~ V.fromList [V3 1 0 1, V3 0 1 1, V3 1 1 0]
              & meshFaces          .~ V.singleton (Face (V.fromList [0, 1, 2]))
              & meshPrimitiveTypes .~ V.singleton PrimitiveTriangle
            fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
        fixedSceneMesh `shouldBe` mesh
      it "With points only" $ do
        let mesh = newMesh
              & meshNormals        .~ V.empty
              & meshVertices       .~ V.singleton (V3 1 1 1)
              & meshFaces          .~ V.singleton (Face (V.singleton 0))
              & meshPrimitiveTypes .~ V.singleton PrimitivePoint
            fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
        fixedSceneMesh `shouldBe` mesh
      it "With lines only" $ do
        let mesh = newMesh
              & meshNormals        .~ V.empty
              & meshVertices       .~ V.fromList [V3 1 0 1, V3 0 1 1]
              & meshFaces          .~ V.singleton (Face (V.fromList [0, 1]))
              & meshPrimitiveTypes .~ V.singleton PrimitiveLine
            fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
        fixedSceneMesh `shouldBe` mesh
    context "For mesh with trianles" $ do
      it "Same normal for each vertex" $ do
        let mesh = newMesh
              & meshNormals        .~ V.empty
              & meshVertices       .~ V.fromList [V3 1 0 0, V3 0 1 0, V3 0 0 1]
              & meshFaces          .~ V.singleton (Face (V.fromList [0, 1, 2]))
              & meshPrimitiveTypes .~ V.singleton PrimitiveTriangle
            fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
        (fixedSceneMesh ^. meshNormals) `shouldBe` V.replicate 3 (normalize $ V3 1 1 1)
    context "For mesh with polygons" $ do
      it "Ignores intermediate vertices" $ do
        let mesh = newMesh
              & meshNormals        .~ V.empty
              & meshVertices       .~ V.fromList [V3 1 0 0, V3 0 1 0, V3 0 0 6, V3 0 5 0, V3 4 0 0, V3 0 0 1]
              & meshFaces          .~ V.singleton (Face (V.fromList [0, 1, 2, 3, 4, 5]))
              & meshPrimitiveTypes .~ V.singleton PrimitivePolygone
            fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
        (fixedSceneMesh ^. meshNormals) `shouldBe` V.replicate 6 (normalize $ V3 1 1 1)
    context "Generates dummy normals for points and lines" $ do
      let mesh = newMesh
            & meshNormals        .~ V.empty
            & meshVertices       .~ V.fromList [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 0 0 1, V3 0 0 0, V3 0 0 0]
            & meshFaces          .~ V.fromList [Face (V.singleton 0), Face (V.fromList [1, 2, 3]), Face (V.fromList [4, 5])]
            & meshPrimitiveTypes .~ V.fromList [PrimitivePoint, PrimitiveTriangle, PrimitiveLine]
          fixedSceneMesh = V.head $ (apply (scene mesh)) ^. sceneMeshes
      it "Single normal for a point" $ do
        (isDummyNormal $ V.head $ fixedSceneMesh ^. meshNormals) `shouldBe` True
      it "Two normals for a line" $ do
        (V.map isDummyNormal $ V.drop 4 $ fixedSceneMesh ^. meshNormals) `shouldBe` V.replicate 2 True
