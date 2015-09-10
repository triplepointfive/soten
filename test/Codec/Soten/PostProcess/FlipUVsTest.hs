{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.FlipUVsTest where

import Test.Hspec

import Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.FlipUVs
import Codec.Soten.Scene
import Codec.Soten.Scene.Mesh
import Codec.Soten.Scene.Material

validMesh = newMesh & meshTextureCoords .~ V.singleton (V3 1 1 1)
material transform = addProperty newMaterial (MaterialUVTransform transform)

originTransform = newUVTransform
    & uvTransformTranslation .~ V2 1 1
    & uvRotation .~ 0.5

flippedTransform = newUVTransform
    & uvTransformTranslation .~ V2 (-1) (-1)
    & uvRotation .~ -0.5

flipUVsTest :: Spec
flipUVsTest =
  describe "Flip UVs" $
    context "Flips" $ do
      let scene = newScene
            & sceneMeshes  .~ V.singleton validMesh
            & sceneMaterials .~ V.singleton (material originTransform)
          fixedSceneMesh = V.head $ apply scene ^. sceneMeshes
          fixedSceneMaterial = V.head $ apply scene ^. sceneMaterials
      it "Material" $
        (fixedSceneMesh ^. meshTextureCoords) `shouldBe` V.singleton (V3 1 0 1)
      it "Adds primitive type triange" $
        fixedSceneMaterial `shouldBe` material flippedTransform
