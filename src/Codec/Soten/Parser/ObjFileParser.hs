module Codec.Soten.Parser.ObjFileParser (
    ObjFileParser(..)
  , Model(..)
  , getModel
) where

data ObjFileParser = ObjFileParser String String
data Model = Model

getModel :: ObjFileParser -> Model
getModel = undefined

