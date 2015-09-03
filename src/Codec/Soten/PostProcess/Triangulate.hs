{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- The Triangulate splits up all faces with more than three indices
-- into triangles. You usually want this to happen because the graphics cards
-- need their data as triangles.
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.Triangulate (
    apply
) where

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), normalize, cross)

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene & sceneMeshes %~ V.map triangulateIfNeeded

-- | Checks if mesh needs a triangulation and perform it if needed.
triangulateIfNeeded :: Mesh -> Mesh
triangulateIfNeeded mesh@Mesh{..} = if needToProceed
    then triangulateMesh mesh
    else mesh
  where
    needToProceed = if V.null _meshPrimitiveTypes
        then V.any ( \ x -> V.length (_faceIndices x ) > 3 ) _meshFaces
        else PrimitivePolygone `V.elem` _meshPrimitiveTypes

-- | Triangulates mesh.
triangulateMesh :: Mesh -> Mesh
triangulateMesh mesh@Mesh{..} = mesh
    & meshPrimitiveTypes .~ primitiveTypes
  where
    primitiveTypes = V.filter (/=PrimitivePolygone) $
        if PrimitiveTriangle `V.elem` _meshPrimitiveTypes
        then _meshPrimitiveTypes
        else V.cons PrimitiveTriangle _meshPrimitiveTypes
