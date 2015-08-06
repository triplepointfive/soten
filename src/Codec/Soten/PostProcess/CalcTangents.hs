-- | Implementation of the post processing step to calculate
-- tangents and bitangents for all imported meshes.
module Codec.Soten.PostProcess.CalcTangents (
    apply
) where

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh
import           Codec.Soten.Util (degToRad)

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
  where
    meshPos = mesh ^. meshVertices
    meshNorm = mesh ^. meshNormals
    meshTex = mesh ^. meshTextureCoords

    calcForFace :: Face -> (V.Vector (V3 Float), V.Vector (V3 Float))
    calcForFace (Face indices)
        | numIndices < 3 = ( V.replicate numIndices vecNaN
                           , V.replicate numIndices vecNaN
                           )
        | otherwise      =
      where
        numIndices = V.length indices
        p0 = indices V.! 0
        p1 = indices V.! 1
        p2 = indices V.! 2
        -- Position differences p1->p2 and p1->p3.
        (V3 vx vy vz)  = (meshPos V! p1) - (meshPos V! p0)
        (V3 wx wy wz)  = (meshPos V! p2) - (meshPos V! p0)
        -- Texture offset p1->p2 and p1->p3.
        (V3 sx sy _) = (meshTex V.! p1) - (meshTex V.! p0)
        (V3 tx ty _) = (meshTex V.! p2) - (meshTex V.! p0)
        dirCorrection = if tx * sy - ty * sx < 0 then -1 else 1
        -- When t1, t2, t3 in same position in UV space, just use default UV
        -- direction.
        -- undefined
        -- Tangent points in the direction where to positive X axis of the
        -- texture coord's would point in model space bitangent's points along
        -- the positive Y axis of the texture coord's, respectively.
        tangent = (*dirCorrection) <$> V3
            (wx * sy - vx * ty)
            (wy * sy - vy * ty)
            (wz * sy - vz * ty)
        bitangent = (*dirCorrection) <$> V3
            (wx * sx - vx * tx)
            (wy * sx - vy * tx)
            (wz * sx - vz * tx)


