{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
-- | Contains internal data structures which represents original datum.
module Codec.Soten.Data.StlData (
    Facet(..)
  , facetNormal
  , facetVertices
  , facetAttribute
  , newFacet
  , Model(..)
  , modelName
  , modelFacets
  , newModel
) where

import           GHC.Generics

import           Control.Lens (makeLenses)
import           Data.Serialize
import qualified Data.Vector as V
import           Linear (V3(..))

-- | The facet data, represents a single triangle.
data Facet =
    Facet
    { -- | A unit normal.
      _facetNormal    :: !(V3 Float)
      -- | Three vertices (corners).
    , _facetVertices  :: !(V.Vector (V3 Float))
      -- | Attribute byte count.
    , _facetAttribute :: !Int
    } deriving (Show, Generic)
makeLenses ''Facet

instance Serialize Facet where
    put = error "put is not implemented for Stl.Facet"
    get = do
        n1 <- getFloat32le
        n2 <- getFloat32le
        n3 <- getFloat32le

        v1x <- getFloat32le
        v1y <- getFloat32le
        v1z <- getFloat32le

        v2x <- getFloat32le
        v2y <- getFloat32le
        v2z <- getFloat32le

        v3x <- getFloat32le
        v3y <- getFloat32le
        v3z <- getFloat32le

        return $ Facet (V3 n1 n2 n3)
            (V.fromList [V3 v1x v1y v1z, V3 v2x v2y v2z, V3 v3x v3y v3z]) 0

-- | Initializes new model instance.
newFacet :: Facet
newFacet =
    Facet
    { _facetNormal    = V3 0 0 0
    , _facetVertices  = V.empty
    , _facetAttribute = 0
    }

-- | Data structure to store all stl-specific model datum.
data Model =
    Model
    { -- | Model name.
      _modelName   :: !String
      -- | Model facets.
    , _modelFacets :: !(V.Vector Facet)
    } deriving Show
makeLenses ''Model

-- | Initializes new model instance.
newModel :: Model
newModel =
    Model
    { _modelName   = ""
    , _modelFacets = V.empty
    }
