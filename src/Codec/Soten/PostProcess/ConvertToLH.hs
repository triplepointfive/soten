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

import           Control.Lens ((&), (%~))
import qualified Data.Vector as V
import           Linear

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh
import           Codec.Soten.Scene.Anim
import           Codec.Soten.Scene.Material

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply scene = scene
    & sceneRootNode   %~ processNode
    & sceneMeshes     %~ V.map processMesh
    & sceneMaterials  %~ V.map processMaterial
    & sceneAnimations %~ V.map processAnimation

-- | Recursively converts a node, all of its children and all of its meshes.
processNode :: Node -> Node
processNode node = node
    & nodeTransformation %~ fmap flipMatrix
    & nodeChildren       %~ V.map processNode

-- | Process a mesh.
processMesh :: Mesh -> Mesh
processMesh mesh = mesh
    -- Mirror bitangents as well as they're derived from the texture coords.
    & meshBitangents %~ V.map (*(-1))
    -- Mirror offset matrices of all bones.
    & meshBones      %~ V.map processBone
    -- Mirror positions, normals and stuff along the Z axis.
    & meshVertices   %~ V.map reverseFlipV3
    & meshNormals    %~ V.map reverseFlipV3
    & meshTangents   %~ V.map reverseFlipV3
    -- Why? I don't know why...
    & meshBitangents %~ V.map reverseFlipV3

-- | Process a bone.
processBone :: Bone -> Bone
processBone = boneOffsetMatrix %~ flipMatrix

-- | Process a material.
processMaterial :: Material -> Material
processMaterial = materialProperties %~ V.map convertMat
  where
    convertMat :: MaterialProperty -> MaterialProperty
    convertMat (MaterialTexMapAxis t vec) =
        MaterialTexMapAxis t (((-1)*) <$> vec)
    convertMat property = property

-- | Transform all animation channels.
processAnimation :: Animation -> Animation
processAnimation = animationChannels %~ V.map processNodeAnim

-- | Transform a single animation node.
processNodeAnim :: NodeAnim -> NodeAnim
processNodeAnim nodeAnim = nodeAnim
    & nodeAnimPositionKeys %~ V.map flipVectorKey
    & nodeAnimRotationKeys %~ V.map flipQuatKey
  where
    flipVectorKey :: VectorKey -> VectorKey
    flipVectorKey = vectorValue %~ flipV3

    flipQuatKey :: QuatKey -> QuatKey
    flipQuatKey = quatValue %~ flipQuaternion

    flipQuaternion :: Num a => Quaternion a -> Quaternion a
    flipQuaternion (Quaternion q (V3 x y z)) = Quaternion q (V3 (-x) (-y) z)

-- | Flips a vector of 3 elements.
flipV3 :: Num a => V3 a -> V3 a
flipV3 (V3 x y z) = V3 (-x) (-y) z

-- | Flips the last coordinate of a vector.
reverseFlipV3 :: V3 Float -> V3 Float
reverseFlipV3 (V3 x y z) = V3 x y (-z)

-- | Flips a matrix to LH.
flipMatrix :: M44 Float -> M44 Float
flipMatrix (V4 a b c d) =
    V4 (V4    a1    a2 (-a3)    a4)
       (V4    b1    b2 (-b3)    b4)
       (V4 (-c1) (-c2)   c3  (-c4))
       (V4    d1    d2 (-d3)    d4)
  where
    (V4 a1 a2 a3 a4) = a
    (V4 b1 b2 b3 b4) = b
    (V4 c1 c2 c3 c4) = c
    (V4 d1 d2 d3 d4) = d
