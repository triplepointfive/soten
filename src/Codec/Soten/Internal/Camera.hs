{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Internal.Camera (
    Camera(..)
  , cameraName
  , cameraPosition
  , cameraUp
  , cameraLookAt
  , cameraHorizontalFOV
  , cameraClipPlaneNear
  , cameraClipPlaneFar
  , cameraAspect
  , newCamera
) where

import           Control.Lens (makeLenses)
import           Linear (V3(..))

-- | Helper structure to describe a virtual camera.
data Camera =
    Camera
    { -- | The name of the camera.
      _cameraName          :: !String
      -- | Position of the camera relative to the coordinate space
      -- defined by the corresponding node.
    , _cameraPosition      :: !(V3 Float)
      -- | Up vector
    , _cameraUp            :: !(V3 Float)
      -- | A vector of the camera coordinate system relative to
      -- the coordinate space defined by the corresponding node.
    , _cameraLookAt        :: !(V3 Float)
      -- | Half horizontal field of view angle, in radians.
    , _cameraHorizontalFOV :: !Float
      -- | Distance of the near clipping plane from the camera.
    , _cameraClipPlaneNear :: !Float
      -- | Distance of the far clipping plane from the camera.
    , _cameraClipPlaneFar  :: !Float
      -- | Screen aspect ratio.
    , _cameraAspect        :: !Float
    } deriving Show
makeLenses ''Camera

newCamera :: Camera
newCamera =
    Camera
    { _cameraName          = ""
    , _cameraPosition      = V3 0 0 0
    , _cameraUp            = V3 0 1 0
    , _cameraLookAt        = V3 0 0 1
    , _cameraHorizontalFOV = 0.25 * pi
    , _cameraClipPlaneNear = 0.1
    , _cameraClipPlaneFar  = 1000
    , _cameraAspect        = 0
    }
