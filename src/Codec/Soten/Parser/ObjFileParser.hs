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

getModel :: String -> String -> Model
getModel content modelName =
    foldl' (flip parseLine) (newModel modelName) fileLines
  where
    fileLines = map replaceTabs (lines content)
    replaceTabs = map (\c -> if c == '\t' then ' ' else c)

parseLine :: String -> Model -> Model
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

getVertex :: String -> Model -> Model
getVertex line model = model & modelVertices %~ (++[parseVector3 line])

getTextureCoord :: String -> Model -> Model
getTextureCoord line model = model & modelTextureCoord %~ (++[newVector coords])
  where
    newVector (x:y:[])  = V3 x y 0
    newVector (x:y:z:_) = V3 x y z
    newVector _         = throw $ DeadlyImporterError $
        "Invalid number of components: '" ++ line ++ "'"
    coords = map (maybe parseError fromJust . readMay ) $ take 3
        $ filter (not . null) $ split " " line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "'"

getVertexNormal :: String -> Model -> Model
getVertexNormal line model = model & modelNormals %~ (++[parseVector3 line])

type FaceIndices = ([Int], [Int], [Int])

-- TODO: Assign material
getFace :: PrimitiveType -> String -> Model -> Model
getFace _ "" model = model
getFace primitiveType line model =
    if null indices
    then model
    else
        let dataExample = tightDigits (head indices)
            dataPattern = flip (faceVertexParser dataExample)
            (vertices, texture, normals) = foldl dataPattern ([], [], []) indices
            newFace = Face primitiveType vertices texture normals
            -- TODO: Check # of elements in face
        in createObject $ setActiveMaterial model
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

getComment :: Model -> Model
getComment = id

getMaterialDesc :: String -> Model -> Model
getMaterialDesc = undefined

-- Not used
getGroupNumberAndResolution :: String -> Model -> Model
getGroupNumberAndResolution _ = id

getMaterialLib :: String -> Model -> Model
getMaterialLib = undefined

getGroupName :: String -> Model -> Model
getGroupName = undefined

-- Not used
getGroupNumber :: String -> Model -> Model
getGroupNumber _ = id

getObjectName :: String -> Model -> Model
getObjectName [] model = model
getObjectName objName model =
    model & modelObjects .~ objectsList
        & modelCurrentObject .~ nothing newObject foundObject
  where
    newObject = newObject & objectName .~ objName
    (foundObject, objectsList) = extract ((==objName) . _objectName)
        (model^.modelObjects)

parseVector3 :: String -> V3 Float
parseVector3 line = V3 x y z
  where
    [x, y, z] = map (maybe parseError id . readMay) $ take 3
        $ filter (not . null) $ split " " line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "'"

createObject :: Model -> Model
createObject model
    | isJust (model ^. modelCurrentObject) = model
    | otherwise = model & modelCurrentObject
        .~ Just (newObject & objectName .~ "defaultobject")

setActiveMaterial :: Model -> Model
setActiveMaterial model =
    model & modelCurrentMaterial .~ nothing newMaterial (Just defaultMaterial)
  where defaultMaterial = model ^. modelDefaultMaterial

--setCurrentMesh :: Model -> Model
--setCurrentMesh obj =
--    obj & objFileParserModel %~ modelCurrentMesh

