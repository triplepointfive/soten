{-# LANGUAGE TemplateHaskell #-}
-- | Defines texture helper structures for the library.
module Codec.Soten.Scene.Texture (
    Texel(..)
  , texelA
  , texelR
  , texelG
  , texelB

  , Texture(..)
  , textureWidth
  , textureHeight
  , textureTexels
  , textureFormatHint
  , newTexture
  , checkFormat
) where

import           Data.Int (Int8)

import           Control.Lens (makeLenses, (^.))
import qualified Data.Vector as V

-- | Helper structure to represent a texel in a ARGB8888 format.
data Texel =
    Texel
    { -- | Alpha component.
      _texelA :: !Int8
      -- | Red component.
    , _texelR :: !Int8
      -- | Green component.
    , _texelG :: !Int8
      -- | Blue component.
    , _texelB :: !Int8
    } deriving (Show, Eq)
makeLenses ''Texel

-- | Helper structure to describe an embedded texture.
data Texture =
    Texture
    { -- | Width of the texture, in pixels.
      _textureWidth  :: !Int
      -- | Height of the texture, in pixels.
    , _textureHeight :: !Int
      -- | Data of the texture.
    , _textureTexels :: !(V.Vector Texel)
      -- | A hint from the loader to make it easier for applications
      -- to determine the type of embedded compressed textures.
    , _textureFormatHint :: !String
    } deriving Show
makeLenses ''Texture

-- | Initializes new 'Texture'.
newTexture :: Texture
newTexture =
    Texture
    { _textureWidth      = 0
    , _textureHeight     = 0
    , _textureTexels     = V.empty
    , _textureFormatHint = "\NUL\NUL\NUL"
    }

-- | Compare the format hint against a given string.
checkFormat :: Texture -> String -> Bool
checkFormat texture s = s == texture ^. textureFormatHint
