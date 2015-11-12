-- | Implementation of the post processing step to calculate
-- tangents and bitangents for all imported meshes.
module Codec.Soten.PostProcess.CalcTangents (
    apply
) where

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), normalize, cross)

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh
import           Codec.Soten.Util (degToRad)

type VecVec = V.Vector (V3 Float)

maxAngle = degToRad 45
sourceUV = 0
angleEpsilon = 0.9999

getNaN :: Float
getNaN = read "NaN"

vecNaN = V3 getNaN getNaN getNaN

-- | Executes the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene & sceneMeshes %~ V.map processMesh

-- | Calculates tangents and bitangents for the given mesh.
processMesh :: Mesh -> Mesh
processMesh mesh
    | hasTangentsAndBitangents mesh = mesh
    -- Tangents are undefined for line and point meshes.
    | PrimitiveTriangle `V.notElem` (mesh ^. meshPrimitiveTypes)
      && PrimitivePolygone `V.notElem` (mesh ^. meshPrimitiveTypes) = mesh
    -- Mesh must have normals.
    | not (hasNormals mesh) = mesh
    | otherwise = mesh
        & meshTangents   .~ V.map normalize tangents
        & meshBitangents .~ V.map normalize bitangents
  where
    meshPos = mesh ^. meshVertices
    meshNorm = mesh ^. meshNormals
    meshTex = mesh ^. meshTextureCoords

    emptyVec = V.replicate (V.length meshPos) (V3 0 0 0)
    (tangents, bitangents) =
        V.foldl updateGets (emptyVec, emptyVec) (mesh ^. meshFaces)

    updateGets :: (VecVec, VecVec) -> Face -> (VecVec, VecVec)
    updateGets gets (Face indices) = undefined
      where
         -- | Returns a vector of index and related tangent + bitangent.
         faceData :: V.Vector (Int, V3 Float, V3 Float)
         faceData
            | numIndices < 3 = V.map (\ i -> (i, vecNaN, vecNaN)) indices
            | otherwise      = V.map (\ i -> let (t, bt) = vertexTangents i in (i, t, bt)) indices
          where
            numIndices = V.length indices
            p0 = indices V.! 0
            p1 = indices V.! 1
            p2 = indices V.! 2
            -- Position differences p1->p2 and p1->p3.
            (V3 vx vy vz) = (meshPos V.! p1) - (meshPos V.! p0)
            (V3 wx wy wz) = (meshPos V.! p2) - (meshPos V.! p0)
            -- Texture offset p1->p2 and p1->p3.
            (V3 sx sy _) = (meshTex V.! p1) - (meshTex V.! p0)
            (V3 tx ty _) = (meshTex V.! p2) - (meshTex V.! p0)
            dirCorrection = if tx * sy - ty * sx < 0 then -1 else 1
            -- When t1, t2, t3 in same position in UV space, just use default UV
            -- direction.
            -- undefined
            -- Tangent points in the direction where to positive X axis of the
            -- texture coord's would point in model space bitangent's points
            -- along the positive Y axis of the texture coord's, respectively.
            tangent = (*dirCorrection) <$> V3
                (wx * sy - vx * ty)
                (wy * sy - vy * ty)
                (wz * sy - vz * ty)
            bitangent = (*dirCorrection) <$> V3
                (wx * sx - vx * tx)
                (wy * sx - vy * tx)
                (wz * sx - vz * tx)

            vertexTangents :: Int -> (V3 Float, V3 Float)
            vertexTangents idx
                -- Reconstruct tangent/bitangent according to normal and
                -- bitangent/tangent when it's infinite or NaN.
                | validVector localTangent && not (validVector localBitangent) =
                    (localTangent, localTangent `cross` norm)
                | validVector localBitangent && not (validVector localTangent) =
                    (norm `cross` localBitangent, localBitangent)
                | otherwise = (localTangent, localBitangent)
              where
                norm = meshNorm V.! idx
                -- Project tangent and bitangent into the plane formed by the
                -- vertex' normal.
                localTangent = normalize $ tangent -
                    (norm `cross` (tangent `cross` norm))
                localBitangent = normalize $ bitangent -
                    (norm `cross` (bitangent `cross` norm))

validVector :: V3 Float -> Bool
validVector v = v == v
