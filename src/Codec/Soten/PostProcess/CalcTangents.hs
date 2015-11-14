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

type VecVec = V.Vector (V3 Float)

getNaN :: Float
getNaN = read "NaN"

vecNaN :: V3 Float
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
    updateGets (tans, bitans) (Face indices) =
        ( tans   `V.update` V.map (\(i, tas, _) -> (i, tas + tans   V.! i)) faceData
        , bitans `V.update` V.map (\(i, _, bis) -> (i, bis + bitans V.! i)) faceData
        )
      where
         -- | Returns a vector of index and related tangent + bitangent.
         faceData :: V.Vector (Int, V3 Float, V3 Float)
         faceData
            | numIndices == 3 = V.map (\ i -> (i, tang,   bitang)) indices
            | otherwise       = V.map (\ i -> (i, vecNaN, vecNaN)) indices
          where
            numIndices = V.length indices
            p0 = indices V.! 0
            p1 = indices V.! 1
            p2 = indices V.! 2
            v0 = meshPos V.! p0
            v1 = meshPos V.! p1
            v2 = meshPos V.! p2
            v0tex = meshTex V.! p0
            v1tex = meshTex V.! p1
            v2tex = meshTex V.! p2

            -- | Calculates tangent and bitangent vectors for 3 given vertices.
            -- (tang, bitang) :: (V3 Float, V3 Float)
            (tang, bitang) = (V3 tx ty tz, V3 btx bty btz)
              where
                (V3 edge1x edge1y edge1z) = v1 - v0
                (V3 edge2x edge2y edge2z) = v2 - v0

                (V3 deltaU1 deltaV1 _) = v1tex - v0tex
                (V3 deltaU2 deltaV2 _) = v2tex - v0tex

                f = 1.0 / (deltaU1 * deltaV2 - deltaU2 * deltaV1)

                tx = f * (deltaV2 * edge1x - deltaV1 * edge2x)
                ty = f * (deltaV2 * edge1y - deltaV1 * edge2y)
                tz = f * (deltaV2 * edge1z - deltaV1 * edge2z)

                btx = f * (-deltaU2 * edge1x - deltaU1 * edge2x)
                bty = f * (-deltaU2 * edge1y - deltaU1 * edge2y)
                btz = f * (-deltaU2 * edge1z - deltaU1 * edge2z)

validVector :: V3 Float -> Bool
validVector v = v == v
