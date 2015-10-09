{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.ConvertToLHTest where

import Test.Hspec

import Control.Lens ((&), (^.), (.~))
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

bone :: Bone
bone = newBone & boneOffsetMatrix .~ transformMatrix

material :: Material
material = addProperty newMaterial property
  where
    property = MaterialTexMapAxis TextureTypeNone (V3 (-1) (-2) (-3))

animation :: Animation
animation = newAnimation & animationChannels .~ V.singleton animNode
  where
    animNode :: NodeAnim
    animNode = newNodeAnim
        & nodeAnimPositionKeys .~ V.singleton vecKey
        & nodeAnimRotationKeys .~ V.singleton quatKey
    vecKey = VectorKey 0.1 (V3 (-1) (-2) 3)
    quatKey = QuatKey 0.2 (Quaternion 1 (V3 (-1) (-2) 3))

originScene, scene :: Scene
originScene = newScene
    & sceneRootNode   .~ rootNode
    & sceneMeshes     .~ V.singleton mesh
    & sceneMaterials  .~ V.singleton material
    & sceneAnimations .~ V.singleton animation
scene = apply originScene

convertToLHTest :: Spec
convertToLHTest =
  describe "Convert to Left Handed post process" $ do
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
    context "Materials" $ do
      let fixedMaterial = V.head (scene ^. sceneMaterials)
      it "Texture map axis" $
        V.head (fixedMaterial ^. materialProperties) `shouldBe`
          MaterialTexMapAxis TextureTypeNone (V3 1 2 3)
    context "Animation nodes" $ do
      let fixedAnimation = V.head (scene ^. sceneAnimations)
          fixedNodeAnim = V.head (fixedAnimation ^. animationChannels)
      it "Position keys" $
        V.head (fixedNodeAnim ^. nodeAnimPositionKeys) `shouldBe`
          VectorKey 0.1 (V3 1 2 3)
      it "Rotation keys" $
        V.head (fixedNodeAnim ^. nodeAnimRotationKeys) `shouldBe`
          QuatKey 0.2 (Quaternion 1 (V3 1 2 3))
