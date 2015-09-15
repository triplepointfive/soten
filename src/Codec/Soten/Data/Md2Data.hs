{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Codec.Soten.Data.Md2Data where

import           Control.Monad
                 ( replicateM
                 )
import           GHC.Generics
import           Data.Int
import           Data.Word
                 ( Word8
                 )

import           Data.ByteString.Internal
                 ( w2c
                 )
import           Linear (V3(..))
import           Data.Serialize

import           Data.Serialize.Int

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

instance Serialize Header where
    get = do
        _ident        <- getInt32le
        _version      <- getInt32le
        _skinWidth    <- getInt32le
        _skinHeight   <- getInt32le
        _frameSize    <- getInt32le
        _numSkins     <- getInt32le
        _numVertices  <- getInt32le
        _numSt        <- getInt32le
        _numTris      <- getInt32le
        _numGLCmds    <- getInt32le
        _numFrames    <- getInt32le
        _offsetSkins  <- getInt32le
        _offsetSt     <- getInt32le
        _offsetTris   <- getInt32le
        _offsetFrames <- getInt32le
        _offsetGLCmds <- getInt32le
        _offsetEnd    <- getInt32le

        return Header
            { ident        = _ident
            , version      = _version
            , skinWidth    = _skinWidth
            , skinHeight   = _skinHeight
            , frameSize    = _frameSize
            , numSkins     = _numSkins
            , numVertices  = _numVertices
            , numSt        = _numSt
            , numTris      = _numTris
            , numGLCmds    = _numGLCmds
            , numFrames    = _numFrames
            , offsetSkins  = _offsetSkins
            , offsetSt     = _offsetSt
            , offsetTris   = _offsetTris
            , offsetFrames = _offsetFrames
            , offsetGLCmds = _offsetGLCmds
            , offsetEnd    = _offsetEnd
            }

sizeOfHeader :: Int
sizeOfHeader = 68

-- | The vector, composed of three floating coordinates.
type Vector = V3 Float

getVector :: Get Vector
getVector = do
    x <- getFloat32le
    y <- getFloat32le
    z <- getFloat32le
    return $ V3 x y z

sizeOfVector :: Int32
sizeOfVector = 12

-- | The texture name associated to the model.
data Skin
    = Skin
    { -- | Texture file name.
      texture :: !String
    } deriving (Show, Eq)

sizeOfSkin :: Int
sizeOfSkin = 64

-- | Texture coordinates.
data TexCoord
    = TexCoord
    { s :: !Int16
    , t :: !Int16
    } deriving (Show, Eq, Generic)

instance Serialize TexCoord where
    get = do
        _s <- getInt16le
        _t <- getInt16le
        return $ TexCoord _s _t

sizeOfTexCoord :: Int
sizeOfTexCoord = 4

-- | Triangle info.
data Triangle
    = Triangle
    { -- | Vertex indices of a triangle.
      vertex :: ![Int16]
      -- | Tex coord indices.
    , st     :: ![Int16]
    } deriving (Show, Eq, Generic)

instance Serialize Triangle where
    get = do
        v1  <- getInt16le
        v2  <- getInt16le
        v3  <- getInt16le
        st1 <- getInt16le
        st2 <- getInt16le
        st3 <- getInt16le
        return $ Triangle [v1, v2, v3] [st1, st2, st3]

sizeOfTriangle :: Int
sizeOfTriangle = 12

-- | Composed of "compressed" 3D coordinates and a normal vector index.
data Vertex
    = Vertex
    { -- | Position.
      v           :: ![Word8]
      -- | Normal vector index.
    , normalIndex :: !Word8
    } deriving (Show, Eq, Generic)

instance Serialize Vertex where
    get = do
        x  <- getWord8
        y  <- getWord8
        z  <- getWord8
        ni <- getWord8
        return $ Vertex [x, y, z] ni

sizeOfVertex :: Int32
sizeOfVertex = 4

-- | Have specific informations for itself and the vertex list of the frame.
data Frame
    = Frame
    { -- | Scale factor.
      scale     :: !Vector
      -- | Translation vector.
    , translate :: !Vector
      -- | Frame name.
    , name      :: !String
      -- | List of frame's vertices.
    , verts     :: ![Vertex]
    } deriving (Show, Eq, Generic)

instance Serialize Frame where
    get = do
        _scale <- getVector
        _translate <- getVector
        wordName <- replicateM 16 getWord8
        return $ Frame _scale _translate (map w2c $ filter (/=0) wordName) []

sizeOfFrame :: Int
sizeOfFrame = 40

-- | Holding all model's data.
data Model
    = Model
    { header    :: !Header
    , skins     :: ![Skin]
    , texCoords :: ![TexCoord]
    , triangles :: ![Triangle]
    , frames    :: ![Frame]
    , glCmds    :: ![Int32]
    } deriving (Show, Eq)

sizeOfGLCommand :: Int
sizeOfGLCommand = 4
