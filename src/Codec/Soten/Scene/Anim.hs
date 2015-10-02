{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Scene.Anim (
    VectorKey(..)
  , vectorKeyTime
  , vectorValue

  , QuatKey(..)
  , quatKeyTime
  , quatValue

  , MeshKey
  , meshKeyTime
  , meshValue

  , AnimBehaviour(..)

  , NodeAnim(..)
  , nodeAnimName
  , nodeAnimPositionKeys
  , nodeAnimRotationKeys
  , nodeAnimScalingKeys
  , nodeAnimPreState
  , nodeAnimPostState
  , newNodeAnim

  , MeshAnim
  , meshAnimName
  , meshAnimKeys

  , Animation(..)
  , animationName
  , animationDuration
  , animationTicksPerSecond
  , animationChannels
  , animationMeshChannels
  , newAnimation
) where

import           Control.Lens (makeLenses, (^.))
import qualified Data.Vector as V
import           Linear (V3(..), Quaternion(..))

import           Codec.Soten.Types (Index)

-- | A time-value pair specifying a certain 3D vector for the given time.
data VectorKey =
    VectorKey
    { -- | The time of this key.
      _vectorKeyTime :: !Double
      -- | The value of this key.
    , _vectorValue   :: !(V3 Float)
    } deriving Show
makeLenses ''VectorKey

instance Eq VectorKey where
    (==) v1 v2 = (v1 ^. vectorValue) == (v2 ^. vectorValue)

instance Ord VectorKey where
    (<=) v1 v2 = (v1 ^. vectorKeyTime) <= (v2 ^. vectorKeyTime)

-- | A time-value pair specifying a rotation for the given time.
data QuatKey =
    QuatKey
    { -- | The time of this key.
      _quatKeyTime :: !Double
      -- | The value of this key.
    , _quatValue   :: !(Quaternion Float)
    } deriving Show
makeLenses ''QuatKey

instance Eq QuatKey where
    (==) q1 q2 = (q1 ^. quatValue) == (q2 ^. quatValue)

instance Ord QuatKey where
    (<=) q1 q2 = (q1 ^. quatKeyTime) <= (q2 ^. quatKeyTime)

-- | Binds a anim mesh to a specific point in time.
data MeshKey =
    MeshKey
    { -- | The time of this key.
      _meshKeyTime :: !Double
      -- | Index into the array of coresponding mesh.
    , _meshValue   :: !Index
    } deriving Show
makeLenses ''MeshKey

instance Eq MeshKey where
    (==) m1 m2 = (m1 ^. meshValue) == (m2 ^. meshValue)

instance Ord MeshKey where
    (<=) m1 m2 = (m1 ^. meshKeyTime) <= (m2 ^. meshKeyTime)

-- | Defines how an animation channel behaves outside the defined time range.
data AnimBehaviour
    -- | The value from the default node transformation is taken.
    = AnimBehaviourDefault
    -- | The nearest key value is used without interpolation.
    | AnimBehaviourConstant
    -- | The value of the nearest two keys is linearly extrapolated for the
    -- current time value.
    | AnimBehaviourLinear
    -- | The animation is repeated.
    | AnimBehaviourRepeat
    deriving (Show)

-- | Describes the animation of a single node.
data NodeAnim =
    NodeAnim
    { -- | The name of the node affected by this animation.
      _nodeAnimName         :: !String
      -- | The position keys of this animation channel.
    , _nodeAnimPositionKeys :: !(V.Vector VectorKey)
      -- | The rotation keys of this animation channel.
    , _nodeAnimRotationKeys :: !(V.Vector QuatKey)
      -- | The scaling keys of this animation channel.
    , _nodeAnimScalingKeys  :: !(V.Vector VectorKey)
      -- | Defines how the animation behaves before the first key is
      -- encountered.
    , _nodeAnimPreState     :: !AnimBehaviour
      -- | Defines how the animation behaves after the last key was processed.
    , _nodeAnimPostState    :: !AnimBehaviour
    } deriving Show
makeLenses ''NodeAnim

newNodeAnim :: NodeAnim
newNodeAnim =
    NodeAnim
    { _nodeAnimName         = ""
    , _nodeAnimPositionKeys = V.empty
    , _nodeAnimRotationKeys = V.empty
    , _nodeAnimScalingKeys  = V.empty
    , _nodeAnimPreState     = AnimBehaviourDefault
    , _nodeAnimPostState    = AnimBehaviourDefault
    }

-- | Describes vertex-based animations for a single mesh or a group of meshes.
data MeshAnim =
    MeshAnim
    { -- | Name of the mesh to be animated.
      _meshAnimName :: !String
      -- | Key frames of the animation.
    , _meshAnimKeys :: !(V.Vector MeshKey)
    } deriving Show
makeLenses ''MeshAnim

-- | An animation consists of keyframe data for a number of nodes.
data Animation =
    Animation
    { -- | The name of the animation.
      _animationName           :: !String
      -- | Duration of the animation in ticks.
    , _animationDuration       :: !Double
      -- | Ticks per second.
    , _animationTicksPerSecond :: !(Maybe Double)
      -- | The node animation channels.
    , _animationChannels       :: !(V.Vector NodeAnim)
      -- | The mesh animation channels.
    , _animationMeshChannels   :: !(V.Vector MeshAnim)
    } deriving Show
makeLenses ''Animation

newAnimation :: Animation
newAnimation =
    Animation
    { _animationName           = ""
    , _animationDuration       = -1
    , _animationTicksPerSecond = Nothing
    , _animationChannels       = V.empty
    , _animationMeshChannels   = V.empty
    }
