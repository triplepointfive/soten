-- | Contains internal data structures which represents original datum.
module Codec.Soten.Data.XglData (
    Model(..)
  , Author(..)
  , LightingTag(..)
  , Mesh(..)
) where

import           Linear
                 ( V3(..)
                 )

-- | Describes the lighting in an environment.
data LightingTag
    -- | Describes light that has equal intensity in all directions.
    = LightingTagAmbient
    { -- | Ambient light color.
      lightingTagAmbientColor :: !(V3 Float)
    }
    -- | Describes a light that comes from a particular direction, as if there
    -- was a light source an infinite distance away.
    | LightingTagDirectional
    { -- | Describes the direction that light is shining from.
      lightingTagDirectionalDirection :: !(V3 Float)
      -- | The components of lighting that are reflected diffusely by materials.
    , lightingTagDirectionalDiffuse   :: !(V3 Float)
      -- | The components of lighting that are reflected spectrally by
      -- materials.
    , lightingTagDirectionalSpecular  :: !(V3 Float)
    }
    -- | Describes an environmental sphere of light that illuminates the object.
    | LightingTagSphereMap
    {
      -- | Specifies the center of the sphere.
      lightingTagSphereMapCenter :: !(V3 Float)
      -- | A scalar that specifies the radius of the sphere map.
    , lightingTagSphereMapRadius :: !Float
    }
    deriving Show

-- | Determines the original author of the file.
data Author =
    Author
    { -- | Represents the author name.
      authorName    :: !String
      -- | The version of the model file. Contains "." as a delimiter.
    , authorVersion :: !String
    } deriving Show

-- | A collection of related triangles, lines and points that approximate a
-- 3D object.
data Mesh =
    Mesh
    { -- | Defined in the current scope.
      meshID       :: !Int
      -- | Represents a position in a 3D coordinate space.
    , meshVertices :: ![V3 Float]
      -- | Represents a position in a 3D coordinate space.
    , meshNormals  :: ![V3 Float]
    } deriving Show

-- | Data structure to store all stl-specific model datum.
data Model =
    Model
    { -- | Represents the color that the object in the world should be
      -- displayed on.
      modelBackgroundColor :: !(V3 Float)
      -- | Describes the lighting in an environment.
    , modelLightingTags    :: ![LightingTag]
      -- | Displayable name for an object or world.
    , modelName            :: !(Maybe String)
      -- | Original author
    , modelAuthor          :: !(Maybe Author)
    } deriving Show
