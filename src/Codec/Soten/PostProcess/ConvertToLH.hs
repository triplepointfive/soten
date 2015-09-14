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
processNode = undefined

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
processAnimation :: Animation -> Animation
processAnimation = undefined
