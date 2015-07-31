-- | Defines a post processing step to search an importer's output
-- for data that is obviously invalid.
module Codec.Soten.PostProcess.FindInvalidData (
    apply
) where

import Codec.Soten.Scene

apply :: Scene -> Scene
apply = id
