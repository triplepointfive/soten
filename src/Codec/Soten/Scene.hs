{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Scene (
    Node(..)
  , nodeName
  , nodeTransformation
  , nodeChildren
  , nodeMeshes
  , nodeMetaData
  , newNode
  , findNode

  , SceneFlags(..)

  , Scene(..)
  , sceneFlags
  , sceneRootNode
  , sceneMeshes
  , sceneMaterials
  , sceneAnimations
  , sceneTextures
  , sceneLights
  , sceneCamera
  , newScene
  , hasMeshes
  , hasMaterials
  , hasLights
  , hasTextures
  , hasCameras
  , hasAnimations

  , Animation(..)
  , Camera(..)
  , Light(..)
  , LightSource(..)
  , Material(..)
  , Mesh(..)
  , MetaData(..)
  , Texture(..)
) where

import           Data.Maybe (isJust)
import           Control.Monad (join)

import           Control.Lens (makeLenses, (^.))
import qualified Data.Vector as V
import           Linear.Matrix (M44)

import           Codec.Soten.Scene.Anim (Animation(..))
import           Codec.Soten.Scene.Camera (Camera(..))
import           Codec.Soten.Scene.Light (Light(..), LightSource(..))
import           Codec.Soten.Scene.Material (Material(..))
import           Codec.Soten.Scene.Mesh (Mesh(..))
import           Codec.Soten.Scene.MetaData (MetaData(..))
import           Codec.Soten.Scene.Texture (Texture(..))
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
    } deriving Show
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

-- | Sets scene's state.
data SceneFlags
      -- | Imported data structure is not complete.
    = SceneIncomplete
      -- | The validation is successful.
    | SceneValidated
      -- | The validation is successful but some issues have been found.
    | SceneValidationWarning
      -- | The vertices of the output meshes aren't in the internal
      -- verbose format anymore
    | SceneNonVerboseFormat
      -- | Denotes pure height-map terrain data.
    | SceneTerrain
    deriving Show

-- | The root structure of the imported data.
data Scene =
    Scene
    { -- | The array of the SceneFlags.
      _sceneFlags      :: !(V.Vector SceneFlags)
      -- | The root node of the hierarchy.
    , _sceneRootNode   :: !Node
      -- | The array of meshes.
    , _sceneMeshes     :: !(V.Vector Mesh)
      -- | The array of materials.
    , _sceneMaterials  :: !(V.Vector Material)
      -- | The array of animations.
    , _sceneAnimations :: !(V.Vector Animation)
      -- | The array of embedded textures.
    , _sceneTextures   :: !(V.Vector Texture)
      -- | The array of light sources.
    , _sceneLights     :: !(V.Vector Light)
      -- | The array of cameras.
    , _sceneCamera     :: !(V.Vector Camera)
    } deriving Show
makeLenses ''Scene

newScene :: Scene
newScene =
    Scene
    { _sceneFlags      = V.empty
    , _sceneRootNode   = newNode
    , _sceneMeshes     = V.empty
    , _sceneMaterials  = V.empty
    , _sceneAnimations = V.empty
    , _sceneTextures   = V.empty
    , _sceneLights     = V.empty
    , _sceneCamera     = V.empty
    }

-- | Check whether the scene contains meshes
hasMeshes :: Scene -> Bool
hasMeshes scene = not (V.null (scene ^. sceneMeshes))

-- | Check whether the scene contains materials
hasMaterials :: Scene -> Bool
hasMaterials scene = not (V.null (scene ^. sceneMaterials))

-- | Check whether the scene contains lights
hasLights :: Scene -> Bool
hasLights scene = not (V.null (scene ^. sceneLights))

-- | Check whether the scene contains textures
hasTextures :: Scene -> Bool
hasTextures scene = not (V.null (scene ^. sceneTextures))

-- | Check whether the scene contains cameras
hasCameras :: Scene -> Bool
hasCameras scene = not (V.null (scene ^. sceneCamera))

-- | Check whether the scene contains animations
hasAnimations :: Scene -> Bool
hasAnimations scene = not (V.null (scene ^. sceneCamera))
