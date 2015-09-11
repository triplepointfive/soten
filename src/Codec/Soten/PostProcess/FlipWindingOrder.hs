--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This step adjusts the output face winding order to be CW.
-- The default face winding order is counter clockwise (CCW).
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.FlipWindingOrder (
    apply
) where

import           Control.Lens ((%~))
import qualified Data.Vector as V

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply = sceneMeshes %~ V.map flipMesh

-- | Converts a single mesh.
flipMesh :: Mesh -> Mesh
flipMesh = meshFaces %~ V.map (faceIndices %~ V.reverse)
