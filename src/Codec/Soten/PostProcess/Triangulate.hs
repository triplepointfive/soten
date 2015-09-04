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

import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad.State

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), normalize, dot)
import           Safe (headMay)

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

-- | Triangulates a face if needed.
triangulateFace :: Face -> State Mesh (V.Vector Face)
triangulateFace face
    | numIndices <= 3 = faceWith3Indices face
    | numIndices == 4 = faceWith4Indices face
    | otherwise       = faceWithNIndices face
  where
    numIndices = V.length (_faceIndices face)

-- | If it's a simple point, line or triangle: just copy it.
faceWith3Indices :: Face -> State Mesh (V.Vector Face)
faceWith3Indices = return . V.singleton

-- | Optimized code for quadrilaterals.
faceWith4Indices :: Face -> State Mesh (V.Vector Face)
faceWith4Indices Face{..} = undefined
  where
    -- Quads can have at maximum one concave vertex. Determine
    -- this vertex (if it exists) and start tri-fanning from it.
    concaveVertexIndex :: V.Vector (V3 Float) -> Int
    concaveVertexIndex verts = fromMaybe 0 concave
      where
        concave :: Maybe Int
        concave = foldl concaveIndex Nothing [0..3]

        concaveIndex :: Maybe Int -> Int -> Maybe Int
        concaveIndex (Just i) _ = Just i
        concaveIndex Nothing  i = if angle > pi then Just i else Nothing
          where
            v0 = verts V.! (_faceIndices V.! ((i + 3) `mod` 4))
            v1 = verts V.! (_faceIndices V.! ((i + 2) `mod` 4))
            v2 = verts V.! (_faceIndices V.! ((i + 1) `mod` 4))
            v  = verts V.! (_faceIndices V.! i)

            left  = normalize (v0 - v)
            diag  = normalize (v1 - v)
            right = normalize (v2 - v)

            angle = acos (left `dot` diag) + acos (right `dot` diag)

faceWithNIndices :: Face -> State Mesh (V.Vector Face)
faceWithNIndices = undefined

