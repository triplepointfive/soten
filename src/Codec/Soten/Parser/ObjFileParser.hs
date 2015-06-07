module Codec.Soten.Parser.ObjFileParser (
    Model(..)
  , getModel
) where

import Control.Monad.State.Lazy

import Codec.Soten.Primitive (PrimitiveType(..))

data ObjFileParser = ObjFileParser
                     { objFileParserContent   :: !String
                     , objFileParserModelName :: !String
                     , objFileParserModel     :: !Model
                     }

data Model = Model

newModel :: Model
newModel = undefined

getModel :: String -> String -> Model
getModel content modelName =
    evalState parseFile (ObjFileParser content modelName newModel)

parseFile :: State ObjFileParser Model
parseFile = do
    obj <- get
    if null (objFileParserContent obj)
    then return (objFileParserModel obj)
    else do
        case (head $ objFileParserContent obj) of
            'v' -> getVertex
            'p' -> getFace PrimitivePoint
            'l' -> getFace PrimitiveLine
            'f' -> getFace PrimitivePolygone
            '#' -> getComment
            'u' -> getMaterialDesc
            'm' -> if 'g' == head (tail (objFileParserContent obj))
                   then getGroupNumberAndResolution
                   else getMaterialLib
            'g' -> getGroupName
            's' -> getGroupNumber
            'o' -> getObjectName
            _   -> skipLine
        parseFile

getVertex :: State ObjFileParser ObjFileParser
getVertex = undefined

getFace :: PrimitiveType -> State ObjFileParser ObjFileParser
getFace = undefined

getComment :: State ObjFileParser ObjFileParser
getComment = undefined

getMaterialDesc :: State ObjFileParser ObjFileParser
getMaterialDesc = undefined

getGroupNumberAndResolution :: State ObjFileParser ObjFileParser
getGroupNumberAndResolution = undefined

getMaterialLib :: State ObjFileParser ObjFileParser
getMaterialLib = undefined

getGroupName :: State ObjFileParser ObjFileParser
getGroupName = undefined

getGroupNumber :: State ObjFileParser ObjFileParser
getGroupNumber = undefined

getObjectName :: State ObjFileParser ObjFileParser
getObjectName = undefined

skipLine :: State ObjFileParser ObjFileParser
skipLine = undefined
