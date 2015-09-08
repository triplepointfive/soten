--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2015 Smelkov Ilya
-- License   :  MIT
-- Maintainer:  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Implementation of the post processing step to convert all
-- imported data to a left-handed coordinate system.
--
--------------------------------------------------------------------
module Codec.Soten.PostProcess.ConvertToLH  (
    apply
) where

import           Control.Lens ((&), (%~), (.~))
import qualified Data.Vector as V
import           Linear (V3(..), normalize, dot)

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

-- | Applies the post processing step on the given imported data.
apply :: Scene -> Scene
apply = undefined
