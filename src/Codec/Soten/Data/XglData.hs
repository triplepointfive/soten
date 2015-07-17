-- | Contains internal data structures which represents original datum.
module Codec.Soten.Data.XglData (
    Model(..)
  , Author(..)
  , LightingTag(..)
  , Mesh(..)
  , Material(..)
  , Face(..)
  , Vertex(..)
  , Transform(..)
  , Object(..)
) where

import           Linear
                 ( V3(..)
                 , V2(..)
                 )

import           Codec.Soten.Types
                 ( Index
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
    deriving (Show, Eq)

-- | Determines the original author of the file.
data Author =
    Author
    { -- | Represents the author name.
      authorName    :: !String
      -- | The version of the model file. Contains "." as a delimiter.
    , authorVersion :: !String
    } deriving (Show, Eq)

-- | A collection of related triangles, lines and points that approximate a
-- 3D object.
data Mesh =
    Mesh
    { -- | Defined in the current scope.
      meshID                 :: !Int
      -- | Represents a position in a 3D coordinate space.
    , meshPositions          :: ![V3 Float]
      -- | Represents a position in a 3D coordinate space.
    , meshNormals            :: ![V3 Float]
      -- | Represents a position on the 2D space of a texture.
    , meshTextureCoordinates :: ![V2 Float]
      -- | Faces list.
    , meshFaces              :: ![Face]
      -- | Materials list.
    , meshMaterials          :: ![Material]
    } deriving (Show, Eq)

-- | Represents the way that a surface interacts with the light that hits it.
data Material =
    Material
    { -- | Defined in the current scope.
      materialID       :: !Int
      -- | The fraction of ambient red, green, and blue light that the object
      -- reflects.
    , materialAmbient  :: !(V3 Float)
      -- | The fraction of diffuse red, green, and blue light that the object
      -- reflects.
    , materialDiffuse  :: !(V3 Float)
      -- | The fraction of specular red, green, and blue light that the object
      -- reflects.
    , materialSpecular :: !(Maybe (V3 Float))
      -- | The amount of red, green, and blue light that the object emits.
    , materialEmiss    :: !(Maybe (V3 Float))
      -- | Indicates how opaque the material is.
    , materialShine    :: !(Maybe Float)
      -- | Indicates how a specular reflection from the material drops off.
    , materialAlpha    :: !(Maybe Float)
    } deriving (Show, Eq)

-- | Represents a vertex with required position and optional normal and texture
-- coordinates.
data Vertex =
    Vertex
    { -- | Position tag reference.
      vertexPosition :: !Index
      -- | Normal tag reference.
    , vertexNormal   :: !(Maybe Index)
      -- | Texture tag reference.
    , vertexTexture  :: !(Maybe Index)
    } deriving (Show, Eq)

-- | Represents a triangular face that is part of a 3D object.
data Face =
    Face
    { -- | Material tag.
      faceMaterial :: !Index
      -- | Vertex 1.
    , faceVertex1  :: !Vertex
      -- | Vertex 2.
    , faceVertex2  :: !Vertex
      -- | Vertex 3.
    , faceVertex3  :: !Vertex
    } deriving (Show, Eq)

-- | Represents a non-skewing three-dimensional transform.
data Transform =
    Transform
    { -- | The positive Z-axis of transformed points will point in the direction
      -- of this vector.
      transForward  :: !(V3 Float)
      -- | The positive Y-axis of transformed points will point as closely as
      -- possible to this vector.
    , transUp       :: !(V3 Float)
      -- | Points are translated so that the origin in the original space is
      -- moved to specified position.
    , transPosition :: !(V3 Float)
      -- | Points are scaled toward the origin of their original space by the
      -- specified scaling amount.
    , transScale    :: !(Maybe (V3 Float))
    } deriving (Show, Eq)

-- | Represents a 3D object at a specific location in the world.
data Object =
    Object
    { -- | This transform is used to map the mesh into the space of the parent
      -- tag.
      objectTransform :: !(Maybe Transform)
      -- | Mesh tag reference.
    , objectMesh      :: !(Maybe Index)
    } deriving (Show, Eq)

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
      -- | Original author.
    , modelAuthor          :: !(Maybe Author)
      -- | List of model meshes.
    , modelMeshes          :: ![Mesh]
      -- | List of model objects.
    , modelObjects         :: ![Object]
    } deriving (Show, Eq)

