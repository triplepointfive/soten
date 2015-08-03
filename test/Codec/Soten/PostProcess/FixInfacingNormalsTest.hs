{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.FixInfacingNormalsTest where

import Test.Hspec

import Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.FixInfacingNormals
import Codec.Soten.Scene
import Codec.Soten.Scene.Mesh

vertices = V.fromList [V3 1 0 0, V3 0 1 0, V3 0 0 1, V3 (-1) 0 0, V3 0 (-1) 0, V3 0 0 (-1)]
normals  = (V.replicate 3 (V3 1 1 1)) V.++ (V.replicate 3 (V3 (-1) (-1) (-1)))
faces    = V.fromList [Face (V.fromList [0, 1, 2]), Face (V.fromList [3, 4, 5])]

validMesh = newMesh
    & meshVertices .~ vertices
    & meshNormals  .~ normals
    & meshFaces    .~ faces

invalidMesh = newMesh
    & meshVertices .~ vertices
    & meshNormals  .~ V.map (*(-1)) normals
    & meshFaces    .~ faces

fixInfacingNormalsTest :: Spec
fixInfacingNormalsTest = do
  describe "Fix infacing normals post process" $ do
    context "Valid mesh" $ do
      let scene = newScene & sceneMeshes  .~ V.singleton validMesh
          fixedSceneMesh = V.head $ (apply scene) ^. sceneMeshes
      it "Left normals unchanged" $ do
        (fixedSceneMesh ^. meshNormals) `shouldBe` normals
      it "Left faces unchanged" $ do
        (fixedSceneMesh ^. meshFaces) `shouldBe` faces
    context "Invalid mesh" $ do
      let scene = newScene & sceneMeshes  .~ V.singleton invalidMesh
          fixedSceneMesh = V.head $ (apply scene) ^. sceneMeshes
      it "Inverts normals" $ do
        (fixedSceneMesh ^. meshNormals) `shouldBe` normals
      it "Flips faces" $ do
        (fixedSceneMesh ^. meshFaces) `shouldBe`
          (V.fromList [Face (V.fromList [2, 1, 0]), Face (V.fromList [5, 4, 3])])
