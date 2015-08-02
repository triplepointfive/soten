-- | Implementation of the post processing step to invert
-- all normals in meshes with infacing normals.
module Codec.Soten.PostProcess.FixInfacingNormals (
    apply
) where

import           Control.Lens ((&), (^.), (%~))
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene & sceneMeshes %~ V.map processMesh

-- | Fixes a mesh.
processMesh :: Mesh -> Mesh
processMesh mesh
    -- Nothing to do if there are no model normals.
    | not (hasNormals mesh)         = mesh
    -- The boxes are overlapping.
    | (d0x > 0) /= (d1x > 0)        = mesh
    | (d0y > 0) /= (d1y > 0)        = mesh
    | (d0z > 0) /= (d1z > 0)        = mesh
    -- This is a planar surface.
    | d1x < 0.05 * sqrt (d1y * d1z) = mesh
    | d1y < 0.05 * sqrt (d1z * d1x) = mesh
    | d1z < 0.05 * sqrt (d1y * d1x) = mesh
    -- Compare the volumes of the bounding boxes.
    | abs d1x < abs d0x             = mesh
    | otherwise                     = flipFaces $ invertNormals mesh
  where
    -- Compute the bounding box of both the model vertices + normals and
    -- the umodified model vertices.
    (min1, max1) = minMaxVec (mesh ^. meshVertices)
    (min0, max0) = minMaxVec $
        V.zipWith (+) (mesh ^. meshVertices) (mesh ^. meshNormals)
    (V3 d0x d0y d0z) = max0 - min0
    (V3 d1x d1y d1z) = max1 - min1

-- | Returns the bounding box.
minMaxVec :: V.Vector (V3 Float) -> (V3 Float, V3 Float)
minMaxVec = V.foldl minMaxVecIter
    (V3 1e10 1e10 1e10, V3 (-1e10) (-1e10) (-1e10))
  where
    minMaxVecIter :: (V3 Float, V3 Float)
                  -> V3 Float
                  -> (V3 Float, V3 Float)
    minMaxVecIter (V3 minX minY minZ, V3 maxX maxY maxZ) (V3 x y z) =
        ( V3 (min minX x) (min minY y) (min minZ z)
        , V3 (max maxX x) (max maxY y) (max maxZ z)
        )

-- | Inverts normals of a mesh.
invertNormals :: Mesh -> Mesh
invertNormals mesh = mesh & meshNormals %~ V.map (*(-1))

-- | Reverses the order of indices for each face.
flipFaces :: Mesh -> Mesh
flipFaces mesh = mesh & meshFaces %~ V.map reverseIndices
  where
    reverseIndices :: Face -> Face
    reverseIndices face = face & faceIndices %~ V.reverse

