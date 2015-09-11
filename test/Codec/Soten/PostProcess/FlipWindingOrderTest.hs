{-# LANGUAGE OverloadedStrings #-}
module Codec.Soten.PostProcess.FlipWindingOrderTest where

import Test.Hspec

import Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import Linear

import Codec.Soten.PostProcess.FlipWindingOrder
import Codec.Soten.Scene
import Codec.Soten.Scene.Mesh
import Codec.Soten.Scene.Material

mesh = newMesh & meshFaces .~ V.singleton (Face (V.fromList [1, 2, 3]))

flipWindingOrderTest :: Spec
flipWindingOrderTest =
  describe "Flip winding order" $
    context "Flips" $ do
      let scene = newScene & sceneMeshes  .~ V.singleton mesh
          fixedSceneMesh = V.head $ apply scene ^. sceneMeshes
      it "Faces indices" $
        (fixedSceneMesh ^. meshFaces) `shouldBe` V.singleton (Face (V.fromList [3, 2, 1]))
