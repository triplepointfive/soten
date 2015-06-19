{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Scene (
    Node(..)
  , findNode
) where

import           Data.Maybe (isJust)
import           Control.Monad (join)

import           Control.Lens (makeLenses, (&), (^.))
import qualified Data.Vector as V
import           Linear.Matrix (M44)

import           Codec.Soten.MetaData (MetaData)
import           Codec.Soten.Types (Index)

-- | A node in the imported hierarchy.
data Node =
    Node
    { -- | The name of the node.
      _nodeName           :: !String
      -- | The transformation relative to the node's parent.
    , _nodeTransformation :: !(Maybe (M44 Float))
      -- | The child nodes of this node.
    , _nodeChildren       :: !(V.Vector Node)
      -- | The meshes of this node. Each entry is an index into the mesh.
    , _nodeMeshes         :: !(V.Vector Index)
      -- | Metadata associated with this node.
    , _nodeMetaData       :: !(Maybe MetaData)
    }
makeLenses ''Node

newNode :: Node
newNode =
    Node
    { _nodeName           = ""
    , _nodeTransformation = Nothing
    , _nodeChildren       = V.empty
    , _nodeMeshes         = V.empty
    , _nodeMetaData       = Nothing
    }

-- | Searches for a node with a specific name, beginning at this nodes.
findNode :: String -> Node -> Maybe Node
findNode name node
    | node ^. nodeName == name      = Just node
    | V.null (node ^. nodeChildren) = Nothing
    | otherwise                     =
        join (V.find isJust searchedChildren)
  where
    searchedChildren :: V.Vector (Maybe Node)
    searchedChildren = V.map (findNode name) (node ^. nodeChildren)
