{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Internal.Light (
    LightSource(..)
    
  , Light(..)
  , lightName
  , lightType
  , lightPosition
  , lightDirection
  , lightAttenuationConstant
  , lightAttenuationLinear
  , lightAttenuationQuadratic
  , lightColorDiffuse
  , lightColorSpecular
  , lightColorAmbient
  , lightAngleInnerCone
  , lightAngleOuterCone
  , newLight
) where

import           Control.Lens (makeLenses)
import           Linear (V3(..))

import           Codec.Soten.Types (Color3D)

-- | All supported types of light sources.
data LightSource
    = LightUndefined
    | LightDirectional
    | LightPoint
    | LightSpot
    | LightAmbient

-- | Helper structure to describe a light source.
data Light =
    Light
    { -- | The name of the light source.
      _lightName                 :: !String
      -- | The type of the light source.
    , _lightType                 :: !LightSource
      -- | Position of the light source in space.
    , _lightPosition             :: !(V3 Float)
      -- | Direction of the light source in space.
    , _lightDirection            :: !(V3 Float)
      -- | Constant light attenuation factor.
    , _lightAttenuationConstant  :: !Float
      -- | Linear light attenuation factor.
    , _lightAttenuationLinear    :: !Float
      -- | Quadratic light attenuation factor.
    , _lightAttenuationQuadratic :: !Float
      -- | Diffuse color of the light source.
    , _lightColorDiffuse         :: !Color3D
      -- | Specular color of the light source.
    , _lightColorSpecular        :: !Color3D
      -- | Ambient color of the light source.
    , _lightColorAmbient         :: !Color3D
      -- | Inner angle of a spot light's light cone.
    , _lightAngleInnerCone       :: !Float
      -- | Outer angle of a spot light's light cone.
    , _lightAngleOuterCone       :: !Float
    }
makeLenses ''Light

newLight :: Light
newLight =
    Light
    { _lightName                 = ""
    , _lightType                 = LightUndefined
    , _lightPosition             = V3 0 0 0
    , _lightDirection            = V3 0 0 0
    , _lightAttenuationConstant  = 0
    , _lightAttenuationLinear    = 1
    , _lightAttenuationQuadratic = 0
    , _lightColorDiffuse         = V3 0 0 0
    , _lightColorSpecular        = V3 0 0 0
    , _lightColorAmbient         = V3 0 0 0
    , _lightAngleInnerCone       = 2 * pi
    , _lightAngleOuterCone       = 2 * pi
    }
