--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Flips UV channels of scene meshes and materials.
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.FlipUVs (
    apply
) where

import           Control.Lens ((&), (%~))
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh
import           Codec.Soten.Scene.Material

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene
    & sceneMeshes    %~ V.map flipMesh
    & sceneMaterials %~ V.map flipMaterial

-- | Converts a single mesh.
flipMesh :: Mesh -> Mesh
flipMesh = meshTextureCoords %~ V.map flipY
  where
    flipY :: V3 Float -> V3 Float
    flipY (V3 x y z) = V3 x (1 - y) z

-- | Converts a single material.
flipMaterial :: Material -> Material
flipMaterial = materialProperties %~ V.map flipUV
  where
    flipUV :: MaterialProperty -> MaterialProperty
    flipUV (MaterialUVTransform uv) = MaterialUVTransform $
        uv & uvTransformTranslation %~ ((-1)*) & uvRotation %~ ((-1)*)
    flipUV property = property
