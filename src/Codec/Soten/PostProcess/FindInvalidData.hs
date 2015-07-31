-- | Defines a post processing step to search an importer's output
-- for data that is obviously invalid.
module Codec.Soten.PostProcess.FindInvalidData (
    apply
) where

import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Scene
import           Codec.Soten.Scene.Mesh

data Action = Remove

apply :: Scene -> Scene
apply = id

processMesh :: Mesh -> Maybe Action
processMesh mesh = undefined

processVector :: (V.Vector (V3 Float)) -> Bool
processVector vector = all id
    [ vectorHasDifferentElements vector   -- All vectors are identical
    , V.all vectorFinite vector           -- Is NaN a component of vector
    , not $ V.any vectorZeroLength vector -- Found zero-length vector
    ]
  where
    vectorFinite :: V3 Float -> Bool
    vectorFinite (V3 x y z) = not $ any isNaN [x, y, z]

    vectorZeroLength :: V3 Float -> Bool
    vectorZeroLength (V3 0 0 0) = True
    vectorZeroLength _          = False

vectorHasDifferentElements :: Eq a => V.Vector a -> Bool
vectorHasDifferentElements vec = if V.length vec > 1
    then isDifferentWith (V.head vec) (V.tail vec) else True
  where
    isDifferentWith :: Eq a => a -> V.Vector a -> Bool
    isDifferentWith e vec
        | V.null vec      = False
        | e == V.head vec = isDifferentWith e (V.tail vec)
        | otherwise       = True
