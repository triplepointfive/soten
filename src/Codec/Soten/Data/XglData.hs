{-# LANGUAGE TemplateHaskell #-}
-- | Contains internal data structures which represents original datum.
module Codec.Soten.Data.XglData (
    Model(..)
  , modelBackgroundColor
  , Author(..)
  , LightingTag(..)
) where

import           Control.Lens
                 ( makeLenses
                 )
import           Linear
                 ( V3(..)
                 )

-- | Describes the lighting in an environment.
data LightingTag
    -- | Describes light that has equal intensity in all directions.
    = LightingTagAmbient
    { -- | Ambient light color.
      _lightingTagAmbientColor :: !(V3 Float)
    }
    -- | Describes a light that comes from a particular direction, as if there
    -- was a light source an infinite distance away.
    | LightingTagDirectional
    { -- | Describes the direction that light is shining from.
      _lightingTagDirectionalDirection :: !(V3 Float)
      -- | The components of lighting that are reflected diffusely by materials.
    , _lightingTagDirectionalDiffuse   :: !(V3 Float)
      -- | The components of lighting that are reflected spectrally by
      -- materials.
    , _lightingTagDirectionalSpecular  :: !(V3 Float)
    }
    -- | Describes an environmental sphere of light that illuminates the object.
    | LightingTagSphereMap
    {
      -- | Specifies the center of the sphere.
      _lightingTagSphereMapCenter :: !(V3 Float)
      -- | A scalar that specifies the radius of the sphere map.
    , _lightingTagSphereMapRadius :: !Float
    }
    deriving Show

-- | Determines the original author of the file.
data Author =
    Author
    { -- | Represents the author name.
      _authorName    :: !String
      -- | The version of the model file. Contains "." as a delimiter.
    , _authorVersion :: !String
    } deriving Show

-- | Data structure to store all stl-specific model datum.
data Model =
    Model
    { -- | Represents the color that the object in the world should be
      -- displayed on.
      _modelBackgroundColor :: !(V3 Float)
      -- | Describes the lighting in an environment.
    , _modelLightingTags    :: ![LightingTag]
      -- | Displayable name for an object or world.
    , _modelName            :: !(Maybe String)
      -- | Original author
    , _modelAuthor          :: !(Maybe Author)
    } deriving Show
makeLenses ''Model
