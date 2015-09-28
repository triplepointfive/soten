--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Implementation of the post processing step to convert all
-- imported data to a left-handed coordinate system.
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.ConvertToLH (
    apply
) where

import           Control.Lens ((&), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), identity)

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene
    & sceneRootNode  %~ processNode identity
    & sceneMeshes    %~ V.map processMesh
    & sceneMaterials %~ V.map processMaterial
    & sceneAnimation %~ V.map processAnimation

-- | Recursively converts a node, all of its children and all of its meshes.
processNode :: Node -> Node
processNode node = node & nodeTransformation .~
    V4 (V4    a1    a2 (-a3)    a4)
       (V4    b1    b2 (-b3)    b4)
       (V4 (-c1) (-c1)   c3  (-c4))
       (V4    d1    d2 (-d3)    d4)
  where
    (V4 (V4 a1 a2 a3 a4) (V4 b1 b2 b3 b4) (V4 c1 c2 c3 c4) (V4 d1 d2 d3 d4)) =
      _nodeTransformation node

-- | Process a mesh.
processMesh :: Mesh -> Mesh
processMesh = undefined

-- | Process a material.
processMaterial :: Material -> Material
processMaterial = materialProperties %~ V.map convertMat
  where
    convertMat :: MaterialProperty -> MaterialProperty
    convertMat (MaterialTexMapAxis t vec) = MaterialTexMapAxis t ((-1*) <$> vec)
    convertMat property = property

-- | Transform all animation channels.
processAnimation :: NodeAnim -> NodeAnim
processAnimation = animationChannels %~ V.map processNodeAnim

-- | Transform a single animation node.
processNodeAnim :: NodeAnim -> NodeAnim
processNodeAnim nodeAmin = nodeAnim
    & nodeAnimPositionKeys %~ V.map flipVectorKey
    & nodeAnimRotationKeys %~ V.map flipQuatKey
  where
    flipVectorKey :: VectorKey -> VectorKey
    flipVectorKey = vectorValue %~ V.map flipV3

    flipV3 :: V3 Float -> V3 Float
    flipV3 (V3 x y z) = V3 x y (-z)

    flipQuatKey :: QuatKey -> QuatKey
    flipQuatKey = quatKeyTime %~ V.map flipQuaternion

    flipQuaternion :: Quaternion -> Quaternion
    flipQuaternion (Quaternion q (V3 x y z)) = Quaternion q (V3 (-x) (-y) z)
