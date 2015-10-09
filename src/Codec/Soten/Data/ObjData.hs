module Codec.Soten.Data.ObjData where

-- | A parsed model representation.
type Model = [Token]

-- | All parseble tokens.
data Token
    -- | Specifies a geometric vertex and its x y z coordinates.
    = Vertex !Float !Float !Float
    -- | Specifies a texture vertex and its coordinates.
    | VertexTexture !Float !Float
    -- | Specifies a user-defined object name.
    | Object !String
    -- | Specifies a face element and its vertex reference number.
    | Face ![Int]
    deriving (Eq, Show)
