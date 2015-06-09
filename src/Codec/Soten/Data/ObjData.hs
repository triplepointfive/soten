{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Data.ObjData where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Codec.Soten.Primitive (PrimitiveType(..))

data Face = Face
            { _facePrimitiveType :: !PrimitiveType
            , _faceVertices      :: ![Int]
            , _faceNormals       :: ![Int]
            , _faceTextureCoord  :: ![Int]
--            , _faceMaterial      :: !Material
            } deriving (Show)
makeLenses ''Face

data Object = Object
              { _objectName :: !String
              } deriving (Show)
makeLenses ''Object

data Model = Model
             { _modelObjects      :: ![Object]
             , _modelVertices     :: ![V3 Float]
             , _modelTextureCoord :: ![V3 Float]
             , _modelNormals      :: ![V3 Float]
             } deriving (Show)
makeLenses ''Model
