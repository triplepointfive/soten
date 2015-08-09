{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Codec.Soten.Data.Md2Data where

import           GHC.Generics
import           Data.Int

import           Linear (V3(..))

import           Data.Serialize

-- | The header is a structure which comes at the beginning of the file.
data Header
    = Header
    { -- | Magic number: "IDP2".
      ident        :: !Int32
      -- | Version: must be 8.
    , version      :: !Int32
      -- | Texture width.
    , skinWidth    :: !Int32
      -- | Texture height.
    , skinHeight   :: !Int32
      -- | Size in bytes of a frame.
    , frameSize    :: !Int32
      -- | Number of skins.
    , numSkins     :: !Int32
      -- | Number of vertices per frame.
    , numVertices  :: !Int32
      -- | Number of texture coordinates.
    , numSt        :: !Int32
      -- | Number of triangles.
    , numTris      :: !Int32
      -- | Number of opengl commands.
    , numGLCmds    :: !Int32
      -- | Number of frames.
    , numFrames    :: !Int32
      -- | Offset skin data.
    , offsetSkins  :: !Int32
      -- | Offset texture coordinate data.
    , offsetSt     :: !Int32
      -- | Offset triangle data.
    , offsetTris   :: !Int32
      -- | Offset frame data.
    , offsetFrames :: !Int32
      -- | Offset OpenGL command data.
    , offsetGLCmds :: !Int32
      -- | Offset end of file.
    , offsetEnd    :: !Int32
    } deriving (Show, Eq, Generic)
instance Serialize Header
sizeOfHeader = 68

-- | The vector, composed of three floating coordinates.
type Vector = V3 Float
sizeOfVector = 12

-- | The texture name associated to the model.
data Skin
    = Skin
    { -- | Texture file name.
      texture :: ![Char] -- 64
    } deriving (Show, Eq, Generic)
instance Serialize Skin
sizeOfSkin = 64

-- | Texture coordinates.
data TexCoord
    = TexCoord
    { s :: !Int16
    , t :: !Int16
    } deriving (Show, Eq, Generic)
instance Serialize TexCoord
sizeOfTexCoord = 4

-- | Triangle info.
data Triangle
    = Triangle
    { -- | Vertex indices of a triangle.
      vertex :: ![Int16] -- 3
      -- | Tex coord indices.
    , st     :: ![Int16] -- 3
    } deriving (Show, Eq, Generic)
instance Serialize Triangle
sizeOfTriangle = 8

-- | Composed of "compressed" 3D coordinates and a normal vector index.
data Vertex
    = Vertex
    { -- | Position.
      v           :: ![Char]
      -- | Normal vector index.
    , normalIndex :: !Char
    } deriving (Show, Eq, Generic)
instance Serialize Vertex
sizeOfVertex = 4

-- | Holding all model's data.
data Model
    = Model
    { header    :: !Header
    , skins     :: ![Skin]
    , texCoords :: ![TexCoord]
    , triangles :: ![Triangle]
    , frames    :: ![Frame]
    , glCmds    :: ![Int32]
    } deriving (Show, Eq, Generic)
instance Serialize Model

-- | Have specific informations for itself and the vertex list of the frame.
data Frame
    = Frame
    { -- | Scale factor.
      scale     :: !Vector
      -- | Translation vector.
    , translate :: !Vector
      -- | Frame name.
    , name      :: ![Char] -- 16
      -- | List of frame's vertices.
    , verts     :: ![Vertex]
    } deriving (Show, Eq, Generic)
instance Serialize Frame
sizeOfFrame = 40

