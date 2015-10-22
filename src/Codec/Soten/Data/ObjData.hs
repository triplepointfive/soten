{-# LANGUAGE RecordWildCards #-}
module Codec.Soten.Data.ObjData where

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
    , vertsAcc  :: !(V.Vector (Float, Float, Float))
    , texstsAcc :: !(V.Vector (Float, Float))
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
addVertex m@Model{..} v = m { vertsAcc = vertsAcc `V.snoc` v }

-- | Adds a single texture coordinate to the end of a list.
addTextureCoord :: Model -> (Float, Float) -> Model
addTextureCoord m@Model{..} t = m { texstsAcc = texstsAcc `V.snoc` t }

-- | Adds a single face to the end of faces list.
addFace :: Model -> ([Int], [Int]) -> Model
addFace m@Model{..} f = m { faces = faces `V.snoc` f }
