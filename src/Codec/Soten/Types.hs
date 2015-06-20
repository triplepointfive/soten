module Codec.Soten.Types (
    Color3D(..)
  , Color4D(..)
  , Index(..)
) where

import Linear (V3(..), V4(..))

type Color3D = V3 Float
type Color4D = V4 Float

type Index = Int
