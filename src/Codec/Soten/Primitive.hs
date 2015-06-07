module Codec.Soten.Primitive (
    PrimitiveType(..)
) where

data PrimitiveType
    = PrimitivePoint
    | PrimitiveLine
    | PrimitiveTriangle
    | PrimitivePolygone
    deriving (Show, Eq)
