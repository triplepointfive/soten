{-# LANGUAGE TemplateHaskell #-}
-- | Contains internal data structures which represents original datum.
module Codec.Soten.Data.XglData (
    Model(..)
  , modelBackgroundColor
) where

import           Control.Lens
                 ( makeLenses
                 )
import           Linear
                 ( V3(..)
                 )

-- | Data structure to store all stl-specific model datum.
data Model =
    Model
    { -- | Represents the color that the object in the world should be
      -- displayed on.
      _modelBackgroundColor :: !(V3 Float)
    } deriving Show
makeLenses ''Model
