-- | Defines the interface to import models.
module Codec.Soten (
    module Linear

  , module Codec.Soten.Scene
  , module Codec.Soten.Scene.Anim
  , module Codec.Soten.Scene.Camera
  , module Codec.Soten.Scene.Light
  , module Codec.Soten.Scene.Material
  , module Codec.Soten.Scene.Mesh
  , module Codec.Soten.Scene.MetaData
  , module Codec.Soten.Scene.Texture
  , module Codec.Soten.Importer
) where

import Linear

import Codec.Soten.Importer (readModelFile)
import Codec.Soten.Scene
import Codec.Soten.Scene.Anim
import Codec.Soten.Scene.Camera
import Codec.Soten.Scene.Light
import Codec.Soten.Scene.Material
import Codec.Soten.Scene.Mesh
import Codec.Soten.Scene.MetaData
import Codec.Soten.Scene.Texture
