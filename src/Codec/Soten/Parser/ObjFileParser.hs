{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Parser.ObjFileParser (
    Model(..)
  , getModel
) where

import Data.List (foldl')

import Control.Lens ((^.), (&), (%~), (.~), makeLenses)
import Data.Maybe (fromJust)
import Data.String.Utils (split, strip)
import Linear (V3(..))
import Safe (readMay)

import Codec.Soten.Primitive (PrimitiveType(..))
import Codec.Soten.Util (extract, nothing, throw, DeadlyImporterError(..))

data Object = Object
              { _objectName :: !String
              }
makeLenses ''Object

data Model = Model
             { _modelObjects  :: ![Object]
             , _modelVertices :: ![V3 Float]
             , _modelNormals  :: ![V3 Float]
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
newModel = Model [] [] []

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
parseLine ('v':' ':xs)     = getVertex xs
parseLine ('v':'t':' ':xs) = getVector xs
parseLine ('v':'n':' ':xs) = getVertexNormal xs
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

getVertex :: String -> ObjFileParser -> ObjFileParser
getVertex line obj =
    obj & objFileParserModel %~ modelVertices %~ (++[parseVector3 line])

getVector :: String -> ObjFileParser -> ObjFileParser
getVector = undefined

getVertexNormal :: String -> ObjFileParser -> ObjFileParser
getVertexNormal line obj = undefined
    obj & objFileParserModel %~ modelNormals %~ (++[parseVector3 line])

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
    obj & objFileParserModel %~ modelObjects .~ objectsList
        & objFileParserCurrent .~ nothing newObject foundObject
  where
    newObject = createObject objName
    (foundObject, objectsList) = extract ((==objName) . _objectName)
        (obj^.objFileParserModel^.modelObjects)

parseVector3 :: String -> V3 Float
parseVector3 line = V3 x y z
  where
    [x, y, z] = map (maybe parseError fromJust . readMay ) $ take 3
        $ filter (not . null) $ split " " line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "'"
