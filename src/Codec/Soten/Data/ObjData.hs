{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Data.ObjData where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Codec.Soten.Primitive (PrimitiveType(..))
import Codec.Soten.Types (Color3D(..))

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
    | TextureTypeCount
    deriving Show

data Material =
    Material
    { -- Name of material description
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
    , _meterialClamp              :: ![TextureType]
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

data Face = Face
            { _facePrimitiveType :: !PrimitiveType
            , _faceVertices      :: ![Int]
            , _faceTextureCoord  :: ![Int]
            , _faceNormals       :: ![Int]
            , _faceMaterial      :: !Material
            } deriving (Show)
makeLenses ''Face

data Mesh = Mesh
            { _meshFaces           :: ![Face]
            , _meshMaterial        :: !Material
              -- | Number of stored indices
            , _meshuiNumIndices    :: !Int
              -- | Number of UV
            , _meshuiUVCoordinates :: !Int
            , _meshMaterialIndex   :: !(Maybe Int)
            , _meshHasNormals      :: !Bool
            } deriving (Show)
makeLenses ''Mesh

data Object = Object
              { _objectName   :: !String
              , _objectMeshes :: ![Int]
              } deriving (Show)
makeLenses ''Object

data Model = Model
             { _modelObjects      :: ![Object]
             , _modelVertices     :: ![V3 Float]
             , _modelTextureCoord :: ![V3 Float]
             , _modelNormals      :: ![V3 Float]
             } deriving (Show)
makeLenses ''Model

newObject :: String -> Object
newObject name = Object name []

--newMesh :: Mesh
--newMesh = Mesh []

newModel :: Model
newModel = Model [] [] [] []

modelCreateNewMesh :: Model -> Model
modelCreateNewMesh model = undefined

newMaterial :: Material
newMaterial =
    Material
    { _materialName               = ""
    , _materialTexture            = ""
    , _materialTextureSpecular    = ""
    , _materialTextureAmbient     = ""
    , _materialTextureEmissive    = ""
    , _materialTextureBump        = ""
    , _materialTextureNormal      = ""
    , _materialTextureSpecularity = ""
    , _materialTextureOpacity     = ""
    , _materialTextureDisp        = ""
    , _meterialClamp              = []
    , _meterialAmbient            = V3 0 0 0
    , _meterialDiffuse            = V3 0.6 0.6 0.6
    , _meterialSpecular           = V3 0 0 0
    , _meterialEmissive           = V3 0 0 0
    , _meterialAlpha              = 1
    , _meterialShineness          = 0
    , _meterialIlluminationModel  = 1
    , _meterialIor                = 1
    }
