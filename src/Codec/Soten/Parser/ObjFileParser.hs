{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Parser.ObjFileParser (
    Model(..)
  , getModel
) where

import Data.Char (isSpace)
import Data.List (foldl')

import Control.Lens ((^.), (&), (.~), makeLenses)

import Codec.Soten.Primitive (PrimitiveType(..))
import Codec.Soten.Util (extract, nothing)

data Object = Object
              { _objectName :: !String
              }
makeLenses ''Object

data Model = Model
             { _modelObjects :: ![Object]
             }
makeLenses ''Model

data ObjFileParser = ObjFileParser
                     { _objFileParserModel   :: !Model
                     , _objFileParserCurrent :: !(Maybe Object)
                     }
makeLenses ''ObjFileParser

createObject :: String -> Object
createObject = Object

newModel :: Model
newModel = Model []

newObjFileParser :: ObjFileParser
newObjFileParser = ObjFileParser newModel Nothing

getModel :: String -> String -> Model
getModel content modelName = _objFileParserModel $
    foldl' (flip parseLine) newObjFileParser fileLines
  where
    fileLines = map replaceTabs (lines content)
    replaceTabs = map (\c -> if c == '\t' then ' ' else c)

parseLine :: String -> ObjFileParser -> ObjFileParser
parseLine []               = id
parseLine ('v':' ':xs)     = getVector3 xs
parseLine ('v':'t':' ':xs) = getVector xs
parseLine ('v':'n':' ':xs) = getVector3 xs
parseLine ('p':' ':xs)     = getFace xs PrimitivePoint
parseLine ('l':' ':xs)     = getFace xs PrimitiveLine
parseLine ('f':' ':xs)     = getFace xs PrimitivePolygone
parseLine ('#':_)          = getComment
parseLine ('u':' ':xs)     = getMaterialDesc xs
parseLine ('m':'g':' ':xs) = getGroupNumberAndResolution xs
parseLine ('m':' ':xs)     = getMaterialLib xs
parseLine ('g':' ':xs)     = getGroupName xs
parseLine ('s':' ':xs)     = getGroupNumber xs
parseLine ('o':' ':xs)     = getObjectName xs
parseLine _                = id

getVector3 :: String -> ObjFileParser -> ObjFileParser
getVector3 = undefined

getVector :: String -> ObjFileParser -> ObjFileParser
getVector = undefined

getFace :: String -> PrimitiveType -> ObjFileParser -> ObjFileParser
getFace = undefined

getComment :: ObjFileParser -> ObjFileParser
getComment = id

getMaterialDesc :: String -> ObjFileParser -> ObjFileParser
getMaterialDesc = undefined

getGroupNumberAndResolution :: String -> ObjFileParser -> ObjFileParser
getGroupNumberAndResolution = undefined

getMaterialLib :: String -> ObjFileParser -> ObjFileParser
getMaterialLib = undefined

getGroupName :: String -> ObjFileParser -> ObjFileParser
getGroupName = undefined

getGroupNumber :: String -> ObjFileParser -> ObjFileParser
getGroupNumber = undefined

getObjectName :: String -> ObjFileParser -> ObjFileParser
getObjectName [] obj = obj
getObjectName objName obj =
    obj & objFileParserModel   .~ updModel
        & objFileParserCurrent .~ (nothing newObject foundObject)
  where
    updModel  = (obj^.objFileParserModel) & modelObjects .~ objectsList
    newObject = createObject objName
    (foundObject, objectsList) = extract (\o -> o^.objectName == objName)
        (obj^.objFileParserModel^.modelObjects)

skipLine :: String -> ObjFileParser -> ObjFileParser
skipLine = undefined
