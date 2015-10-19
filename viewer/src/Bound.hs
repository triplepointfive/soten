{-# LANGUAGE RecordWildCards #-}
module Bound where

import qualified Data.Vector as V

import Codec.Soten as Soten

data BoundingBox =
    BoundingBox
    { sceneMin    :: !(V3 Float)
    , sceneMax    :: !(V3 Float)
    , sceneCenter :: !(V3 Float)
    } deriving Show

boundingBox :: Scene -> BoundingBox
boundingBox Scene{..} =
    BoundingBox
    { sceneMin    = minBound
    , sceneMax    = maxBound
    , sceneCenter = (minBound + maxBound) / 2
    }
  where
    initBounds = (V3 1e10 1e10 1e10, V3 (-1e10) (-1e10) (-1e10))
    (minBound, maxBound) = nodeBound identity initBounds _sceneRootNode

    nodeBound :: M44 Float -> (V3 Float, V3 Float) -> Node
              -> (V3 Float, V3 Float) -- ^ Min and max bounds.
    nodeBound trafo (minVec, maxVec) Node{..} =
        V.foldl (nodeBound mTrafo) (minV, maxV) _nodeChildren
      where
        mTrafo = maybe trafo (trafo !*!) _nodeTransformation
        (minV, maxV) = V.foldl meshBound (minVec, maxVec) nodeMeshes
        nodeMeshes = V.map (_sceneMeshes V.!) _nodeMeshes

        meshBound :: (V3 Float, V3 Float) -> Mesh -> (V3 Float, V3 Float)
        meshBound bounds Mesh{..} = V.foldl compareV bounds _meshVertices

    compareV :: (V3 Float, V3 Float) -> V3 Float -> (V3 Float, V3 Float)
    compareV (V3 minX minY minZ, V3 maxX maxY maxZ) (V3 tX tY tZ) =
        ( V3 (min minX tX) (min minY tY) (min minZ tZ)
        , V3 (max maxX tX) (max maxY tY) (max maxZ tZ)
        )
