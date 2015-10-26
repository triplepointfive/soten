{-# LANGUAGE RecordWildCards #-}
module Codec.Soten.Data.ObjData where

import Linear
import qualified Data.Vector as V

-- | All parseble tokens.
data Token
    -- | Specifies a geometric vertex and its x y z coordinates.
    = Vertex !Float !Float !Float
    -- | Specifies a texture vertex and its coordinates.
    | VertexTexture !Float !Float
    -- | Specifies a user-defined object name.
    | Object !String
    -- | Specifies a face element and its vertex reference number.
    | Face ![Int] ![Int]
    deriving (Eq, Show)

-- | A parsed model representation.
data Model = Model
    { faces     :: !(V.Vector ([Int], [Int]))
    , vertsAcc  :: !(V.Vector (V3 Float))
    , texstsAcc :: !(V.Vector (V3 Float))
    , objName   :: !String
    } deriving (Eq, Show)

-- | Generates an empty model object.
newModel :: Model
newModel = Model
    { faces     = V.empty
    , vertsAcc  = V.empty
    , texstsAcc = V.empty
    , objName   = ""
    }

-- | Adds a single vertex to the end of vertices list.
addVertex :: Model -> (Float, Float, Float) -> Model
addVertex m@Model{..} (x, y, z) = m { vertsAcc = vertsAcc `V.snoc` V3 x y z }

-- | Adds a single texture coordinate to the end of a list.
addTextureCoord :: Model -> (Float, Float) -> Model
addTextureCoord m@Model{..} (u, v) = m { texstsAcc = texstsAcc `V.snoc` V3 u v 0 }

-- | Adds a single face to the end of faces list.
addFace :: Model -> ([Int], [Int]) -> Model
addFace m@Model{..} (vs, ts) =
    m { faces = faces `V.snoc` (map pred vs, map pred ts) }
