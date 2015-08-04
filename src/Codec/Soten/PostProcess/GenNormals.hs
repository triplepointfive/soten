-- | Implementation of the post processing step to generate face
-- normals for all imported faces.
module Codec.Soten.PostProcess.GenNormals (
    apply
) where

import           Control.Lens ((&), (^.), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), normalize, cross)

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- Hacky way to retrieve a NaN value.
getNaN :: Float
getNaN = read "NaN"

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene & sceneMeshes %~ V.map genMeshFaceNormals

-- | Generates normals for each face of a mesh.
genMeshFaceNormals :: Mesh -> Mesh
genMeshFaceNormals mesh
    | hasNormals mesh = mesh
    -- | Mesh consists of points and lines only.
    | PrimitiveTriangle `V.notElem` (mesh ^. meshPrimitiveTypes)
      && PrimitivePolygone `V.notElem` (mesh ^. meshPrimitiveTypes) = mesh
    | otherwise = mesh & meshNormals .~ calculatedNormals
  where
    calculatedNormals :: V.Vector (V3 Float)
    calculatedNormals = V.concatMap faceNormals (mesh ^. meshFaces)

    faceNormals :: Face -> V.Vector (V3 Float)
    faceNormals (Face indices) = case verticesCount of
        0 -> V.empty
        1 -> V.singleton vecNaN
        2 -> V.replicate 2 vecNaN
        _ -> V.replicate verticesCount $ calcNormals
            (V.head indices)
            (indices V.! 1)
            (V.last indices)
      where
        verticesCount = V.length indices
        vecNaN = V3 getNaN getNaN getNaN

    calcNormals :: Int -> Int -> Int -> V3 Float
    calcNormals i1 i2 i3 = normalize $ (v2 - v1) `cross` (v3 - v1)
      where
        v1 = (mesh ^. meshVertices) V.! i1
        v2 = (mesh ^. meshVertices) V.! i2
        v3 = (mesh ^. meshVertices) V.! i3
