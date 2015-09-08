--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.FlipUVs (
    apply
) where

import           Control.Lens ((&), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene
    & sceneMeshes    %~ V.map flipMesh
    & sceneMaterials %~ V.map flipMaterial

-- | Converts a single mesh.
flipMesh :: Mesh -> Mesh
flipMesh mesh = mesh & meshTextureCoords %~ V.map flipY
  where
    flipY :: V3 Float -> V3 Float
    flipY (V3 x y z) = V3 x (1 - y) z

-- | Converts a single material.
flipMaterial :: Material -> Material
flipMaterial = undefined
