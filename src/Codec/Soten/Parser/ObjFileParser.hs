{-# LANGUAGE RecordWildCards #-}
module Codec.Soten.Parser.ObjFileParser (
    Model(..)
  , getModel
) where

import Data.Char (isSpace)
import Data.List (foldl')

import Codec.Soten.Primitive (PrimitiveType(..))

data ObjFileParser = ObjFileParser
                     {
                       objFileParserModel     :: !Model
                     }

data Model = Model

newModel :: Model
newModel = undefined

getModel :: String -> String -> Model
getModel content modelName = objFileParserModel $
    foldl' (flip parseLine) (ObjFileParser newModel) fileLines
  where
    fileLines = map replaceTabs (lines content)
    replaceTabs = map (\c -> if c == '\t' then ' ' else c)

parseLine :: String -> ObjFileParser -> ObjFileParser
parseLine []               = id
parseLine ('v':' ':xs)     = getVector3
parseLine ('v':'t':' ':xs) = getVector
parseLine ('v':'n':' ':xs) = getVector3
parseLine ('p':' ':xs)     = getFace PrimitivePoint
parseLine ('l':' ':xs)     = getFace PrimitiveLine
parseLine ('f':' ':xs)     = getFace PrimitivePolygone
parseLine ('#':' ':xs)     = getComment
parseLine ('u':' ':xs)     = getMaterialDesc
parseLine ('m':'g':' ':xs) = getGroupNumberAndResolution
parseLine ('m':' ':xs)     = getMaterialLib
parseLine ('g':' ':xs)     = getGroupName
parseLine ('s':' ':xs)     = getGroupNumber
parseLine ('o':' ':xs)     = getObjectName
parseLine _                = id

getVector3 :: ObjFileParser -> ObjFileParser
getVector3 = undefined

getVector :: ObjFileParser -> ObjFileParser
getVector = undefined

getFace :: PrimitiveType -> ObjFileParser -> ObjFileParser
getFace = undefined

getComment :: ObjFileParser -> ObjFileParser
getComment = id

getMaterialDesc :: ObjFileParser -> ObjFileParser
getMaterialDesc = undefined

getGroupNumberAndResolution :: ObjFileParser -> ObjFileParser
getGroupNumberAndResolution = undefined

getMaterialLib :: ObjFileParser -> ObjFileParser
getMaterialLib = undefined

getGroupName :: ObjFileParser -> ObjFileParser
getGroupName = undefined

getGroupNumber :: ObjFileParser -> ObjFileParser
getGroupNumber = undefined

getObjectName :: ObjFileParser -> ObjFileParser
getObjectName = undefined

skipLine :: ObjFileParser -> ObjFileParser
skipLine = undefined
