{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Parser.ObjFileParser (
    Model(..)
  , getModel
) where

import Data.List (foldl')

import Control.Lens ((^.), (&), (%~), (.~), makeLenses)
import Data.Maybe (isJust, fromJust)
import Data.String.Utils (split, strip)
import Linear (V3(..))
import Safe (readMay)

import Codec.Soten.Primitive (PrimitiveType(..))
import Codec.Soten.Data.ObjData
import Codec.Soten.Util (extract, nothing, throw, DeadlyImporterError(..))

data ObjFileParser = ObjFileParser
                     { _objFileParserModel   :: !Model
                     , _objFileParserCurrent :: !(Maybe Object)
                     } deriving (Show)
makeLenses ''ObjFileParser

newObjFileParser :: ObjFileParser
newObjFileParser = ObjFileParser newModel Nothing

getModel :: String -> String -> Model
getModel content modelName = _objFileParserModel $
    foldl' (flip parseLine) (newObjFileParser modelName) fileLines
  where
    fileLines = map replaceTabs (lines content)
    replaceTabs = map (\c -> if c == '\t' then ' ' else c)

parseLine :: String -> ObjFileParser -> ObjFileParser
parseLine []               = id
parseLine ('v':' ':xs)     = getVertex xs
parseLine ('v':'t':' ':xs) = getTextureCoord xs
parseLine ('v':'n':' ':xs) = getVertexNormal xs
parseLine ('p':' ':xs)     = getFace PrimitivePoint xs
parseLine ('l':' ':xs)     = getFace PrimitiveLine xs
parseLine ('f':' ':xs)     = getFace PrimitivePolygone xs
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

getTextureCoord :: String -> ObjFileParser -> ObjFileParser
getTextureCoord line obj =
    obj & objFileParserModel %~ modelTextureCoord %~ (++[newVector coords])
  where
    newVector (x:y:[])  = V3 x y 0
    newVector (x:y:z:_) = V3 x y z
    newVector _         = throw $ DeadlyImporterError $
        "Invalid number of components: '" ++ line ++ "'"
    coords = map (maybe parseError fromJust . readMay ) $ take 3
        $ filter (not . null) $ split " " line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "'"

getVertexNormal :: String -> ObjFileParser -> ObjFileParser
getVertexNormal line obj =
    obj & objFileParserModel %~ modelNormals %~ (++[parseVector3 line])

type FaceIndices = ([Int], [Int], [Int])

-- TODO: Assign material
getFace :: PrimitiveType -> String -> ObjFileParser -> ObjFileParser
getFace _ "" obj = obj
getFace primitiveType line obj =
    if null indices
    then obj
    else
        let dataExample = tightDigits (head indices)
            dataPattern = flip (faceVertexParser dataExample)
            (vertices, texture, normals) = foldl dataPattern ([], [], []) indices
            newFace = Face primitiveType vertices texture normals
            -- TODO: Check # of elements in face
--        in
        in createDefaultCurObject obj
--        if isJust (obj^.objFileParserCurrent)
--        then
--            obj & objFileParserCurrent %~ fmap (objectFaces %~ (++[newFace]))
--        else
--            obj & objFileParserCurrent .~ Just (
--                (newObject "defaultobject") & objectFaces %~ (++[newFace]))
  where
    indices = filter (not . null) $ split " " line

    faceVertexParser :: String
                     -> (String -> FaceIndices -> FaceIndices)
    faceVertexParser (_:'/':_:'/':_:[])
      = \ str (vs, vts, vns) -> let [v, vt, vn] = splitAndParseIndecis str in
        (vs ++ [v], vts ++ [vt], vns ++ [vn])
    faceVertexParser (_:'/':'/':_:[])
      = \ str (vs, vts, vns) -> let [v, vn] = splitAndParseIndecis str in
        (vs ++ [v], vts, vns ++ [vn])
    faceVertexParser (_:'/':_:[])
      = \ str (vs, vts, vns) -> let [v, vt] = splitAndParseIndecis str in
        (vs ++ [v], vts ++ [vt], vns)
    faceVertexParser (_:[])
      = \ str (vs, vts, vns) -> let [v] = splitAndParseIndecis str in
        (vs ++ [v], vts, vns)
    faceVertexParser example = throw $ DeadlyImporterError $
      "Failed to parse faceVertex: '" ++ line ++ "' - invalid pattern '" ++
      example ++ "'"

    splitAndParseIndecis :: String -> [Int]
    splitAndParseIndecis = map (maybe parseError id . readMay) .
        filter (not . null) . split "/"
      where
        parseError = throw $ DeadlyImporterError $
            "Failed to retirve indecis for line: '" ++ line ++ "'"

    tightDigits :: String -> String
    tightDigits = tightDigitsIter False
      where
        tightDigitsIter :: Bool -> String -> String
        tightDigitsIter _ [] = []
        tightDigitsIter False (x:xs) =
            x : tightDigitsIter (x `elem` "0123456789") xs
        tightDigitsIter True (x:xs)
            | x `elem` "0123456789" = tightDigitsIter True xs
            | otherwise             = x : tightDigitsIter False xs

getComment :: ObjFileParser -> ObjFileParser
getComment = id

getMaterialDesc :: String -> ObjFileParser -> ObjFileParser
getMaterialDesc = undefined

-- Not used
getGroupNumberAndResolution :: String -> ObjFileParser -> ObjFileParser
getGroupNumberAndResolution _ = id

getMaterialLib :: String -> ObjFileParser -> ObjFileParser
getMaterialLib = undefined

getGroupName :: String -> ObjFileParser -> ObjFileParser
getGroupName = undefined

-- Not used
getGroupNumber :: String -> ObjFileParser -> ObjFileParser
getGroupNumber _ = id

getObjectName :: String -> ObjFileParser -> ObjFileParser
getObjectName [] obj = obj
getObjectName objName obj =
    obj & objFileParserModel %~ modelObjects .~ objectsList
        & objFileParserCurrent .~ nothing newObject foundObject
  where
    newObject = newObject objName
    (foundObject, objectsList) = extract ((==objName) . _objectName)
        (obj^.objFileParserModel^.modelObjects)

parseVector3 :: String -> V3 Float
parseVector3 line = V3 x y z
  where
    [x, y, z] = map (maybe parseError id . readMay) $ take 3
        $ filter (not . null) $ split " " line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "'"

createDefaultCurObject :: ObjFileParser -> ObjFileParser
createDefaultCurObject obj
    | isJust (obj^.objFileParserCurrent) = obj
    | otherwise = obj & objFileParserCurrent
        .~ Just (newObject "defaultobject")
