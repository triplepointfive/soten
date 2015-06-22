{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Data.ObjData where

import qualified Data.Map as Map

import           Control.Lens (makeLenses)
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene.Mesh (PrimitiveType(..))
import           Codec.Soten.Types (Color3D, Index)

data TextureType
    = TextureDiffuseType
    | TextureSpecularType
    | TextureAmbientType
    | TextureEmissiveType
    | TextureBumpType
    | TextureNormalType
    | TextureSpecularityType
    | TextureOpacityType
    | TextureDispType
    deriving (Show, Enum, Bounded, Ord, Eq)

type GroupMap = Map.Map String (V.Vector Index)

defaultMaterial = "DefaultMaterial"

-- | Data structure to store all material specific data
data Material =
    Material
    { -- | Name of material description
      _materialName               :: !String
      -- | Texture names
    , _materialTexture            :: !String
    , _materialTextureSpecular    :: !String
    , _materialTextureAmbient     :: !String
    , _materialTextureEmissive    :: !String
    , _materialTextureBump        :: !String
    , _materialTextureNormal      :: !String
    , _materialTextureSpecularity :: !String
    , _materialTextureOpacity     :: !String
    , _materialTextureDisp        :: !String
    , _meterialClamp              :: !(Map.Map TextureType Bool)
      -- | Ambient color
    , _meterialAmbient            :: !Color3D
      -- | Diffuse color
    , _meterialDiffuse            :: !Color3D
      -- | Specular color
    , _meterialSpecular           :: !Color3D
      -- | Emissive color
    , _meterialEmissive           :: !Color3D
      -- | Alpha value
    , _meterialAlpha              :: !Float
      -- | Shineness factor
    , _meterialShineness          :: !Float
      -- | Illumination model
    , _meterialIlluminationModel  :: !Int
      -- | Index of refraction
    , _meterialIor                :: !Float
    } deriving (Show)
makeLenses ''Material

-- | Data structure for a simple obj-face, describes
-- discreditation and materials
data Face =
    Face
    { -- | Primitive Type
      -- TODO: Use constructors instead
      _facePrimitiveType :: !PrimitiveType
      -- | Vertex indices
    , _faceVertices      :: ![Index]
      -- | Texture coordinates indices
    , _faceTextureCoord  :: ![Index]
      -- | Normal indices
    , _faceNormals       :: ![Index]
      -- | Assigned material
    , _faceMaterial      :: !(Maybe String)
    } deriving (Show)
makeLenses ''Face

-- | Data structure to store a mesh
data Mesh =
    Mesh
    { -- | All stored faces
      _meshFaces           :: !(V.Vector Face)
      -- | Assigned material
    , _meshMaterial        :: !(Maybe String)
      -- | Number of stored indices
    , _meshuiNumIndices    :: !Int
      -- | Number of UV
      -- TODO: Rethink of this field
    , _meshuiUVCoordinates :: !Int
      -- | True if normals are stored
    , _meshHasNormals      :: !Bool
    } deriving (Show)
makeLenses ''Mesh

-- | Stores all objects of an objfile object definition
data Object =
    Object
    { -- | Object name
      _objectName   :: !String
      -- | Assigned meshes
    , _objectMeshes :: ![Index]
    } deriving (Show)
makeLenses ''Object

-- | Data structure to store all obj-specific model datum
data Model =
    Model
    { -- | Model name
      _modelName            :: !String
      -- | List ob assigned objects
    , _modelObjects         :: !(V.Vector Object)
      -- | Current Object
    , _modelCurrentObject   :: !(Maybe Index)
      -- | Current Material
    , _modelCurrentMaterial :: !String
      -- | Current Mesh
    , _modelCurrentMesh     :: !(Maybe Index)
      -- | All generated groups
    , _modelGroupLib        :: ![String]
      -- | All generated vertices
    , _modelVertices        :: ![V3 Float]
      -- | All generated normals
    , _modelNormals         :: ![V3 Float]
      -- | Group map
    , _modelGroups          :: !GroupMap
      -- | Group to face id assignment
      -- TODO: Rethink of this field
    , _modelGroupFaceIDs    :: ![Int]
      -- | Active group
      -- TODO: Rename to current
    , _modelActiveGroup     :: !String
      -- | Generated texture coordinates
    , _modelTextureCoord    :: ![V3 Float]
      -- | Stored Meshes
    , _modelMeshes          :: !(V.Vector Mesh)
      -- | Material map
    , _modelMaterialMap     :: !(Map.Map String Material)
    } deriving (Show)
makeLenses ''Model

newObject :: Object
newObject = Object "" []

newFace :: [Index] -> [Index] -> [Index] -> PrimitiveType -> Face
newFace vertices textures normals primType =
    Face
    { _facePrimitiveType = primType
    , _faceVertices      = vertices
    , _faceTextureCoord  = textures
    , _faceNormals       = normals
    , _faceMaterial      = Nothing
    }

newMesh :: Mesh
newMesh = 
    Mesh
    { _meshFaces           = V.empty
    , _meshMaterial        = Nothing
    , _meshuiNumIndices    = 0
    , _meshuiUVCoordinates = 12
    , _meshHasNormals      = False
    }

newModel :: String -> Model
newModel name =
    Model
    { _modelName            = name
    , _modelObjects         = V.empty
    , _modelCurrentObject   = Nothing
    , _modelCurrentMaterial = defaultMaterial
    , _modelGroupLib        = []
    , _modelVertices        = []
    , _modelNormals         = []
    , _modelGroups          = Map.empty
    , _modelGroupFaceIDs    = []
    , _modelActiveGroup     = ""
    , _modelTextureCoord    = []
    , _modelCurrentMesh     = Nothing
    , _modelMeshes          = V.empty
    , _modelMaterialMap     = Map.fromList [(defaultMaterial, newMaterial)]
    }

newMaterial :: Material
newMaterial =
    Material
    { _materialName               = defaultMaterial
    , _materialTexture            = ""
    , _materialTextureSpecular    = ""
    , _materialTextureAmbient     = ""
    , _materialTextureEmissive    = ""
    , _materialTextureBump        = ""
    , _materialTextureNormal      = ""
    , _materialTextureSpecularity = ""
    , _materialTextureOpacity     = ""
    , _materialTextureDisp        = ""
    , _meterialClamp              = clamp
    , _meterialAmbient            = V3 0 0 0
    , _meterialDiffuse            = V3 0.6 0.6 0.6
    , _meterialSpecular           = V3 0 0 0
    , _meterialEmissive           = V3 0 0 0
    , _meterialAlpha              = 1
    , _meterialShineness          = 0
    , _meterialIlluminationModel  = 1
    , _meterialIor                = 1
    }
  where
    clamp = Map.fromList (zip [minBound ..] (repeat False))
