{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.ConvertToLHTest where

import Test.Hspec

import Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.ConvertToLH
import Codec.Soten.Scene
import Codec.Soten.Scene.Anim
import Codec.Soten.Scene.Mesh
import Codec.Soten.Scene.Material

transformMatrix, flipedTransMatrix :: M44 Float
transformMatrix = V4 (V4 0 1 1 1) (V4 0 1 1 1) (V4 0 1 1 1) (V4 0 1 1 1)
flipedTransMatrix = V4
    (V4 0    1  (-1)  1)
    (V4 0    1  (-1)  1)
    (V4 0  (-1)   1 (-1))
    (V4 0    1  (-1)  1)

rootNode, nodeWithTransform :: Node
nodeWithTransform = newNode & nodeTransformation .~ Just transformMatrix
rootNode = nodeWithTransform & nodeChildren .~ V.singleton nodeWithTransform

mesh :: Mesh
mesh = newMesh
    & meshVertices   .~ V.singleton (V3 1 2 3)
    & meshNormals    .~ V.singleton (V3 4 5 6)
    & meshTangents   .~ V.singleton (V3 7 8 9)
    & meshBitangents .~ V.singleton (V3 1 3 6)
    & meshBones      .~ V.singleton bone

bone = newBone & boneOffsetMatrix .~ transformMatrix

originScene, scene :: Scene
originScene = newScene
    & sceneRootNode .~ rootNode
    & sceneMeshes   .~ V.singleton mesh
scene = apply originScene

convertToLHTest :: Spec
convertToLHTest =
  describe "Converts to Left Handed" $ do
    context "Converts a node" $ do
      let fixedRootNode = scene ^. sceneRootNode
      it "Itself" $
        (fixedRootNode ^. nodeTransformation) `shouldBe` Just flipedTransMatrix
      it "And its children" $ do
        let nodeChild = V.head (fixedRootNode ^. nodeChildren)
        (nodeChild ^. nodeTransformation) `shouldBe` Just flipedTransMatrix
    context "Converts a mesh" $ do
      let fixedMesh = V.head (scene ^. sceneMeshes)
      it "Vertices coordicates" $
        V.head (fixedMesh ^. meshVertices) `shouldBe` V3 1 2 (-3)
      it "Normals coordicates" $
        V.head (fixedMesh ^. meshNormals) `shouldBe` V3 4 5 (-6)
      it "Tangents" $
        V.head (fixedMesh ^. meshTangents) `shouldBe` V3 7 8 (-9)
      it "Bitangents" $
        V.head (fixedMesh ^. meshBitangents) `shouldBe` V3 (-1) (-3) 6
      it "Bones" $ do
        let fixedBone = V.head (fixedMesh ^. meshBones)
        (fixedBone ^. boneOffsetMatrix) `shouldBe` flipedTransMatrix
    -- context "Materials" $ do
      -- let fixedMaterial = V.head (scene ^. sceneMaterials)

