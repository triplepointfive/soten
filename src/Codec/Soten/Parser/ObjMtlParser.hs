module Codec.Soten.Parser.ObjMtlParser (
    load
) where

import           Data.List (foldl')
import qualified Data.Map as Map

import           Control.Lens ((^.), (&), (%~), (.~), Lens', lens)
import           Data.Maybe (fromJust)
import           Linear (V3(..))
import           Safe (readMay)

import           Codec.Soten.Data.ObjData
import           Codec.Soten.Types (Color3D)

-- | Material specific token
data MaterialToken
    = DiffuseTexture
    | AmbientTexture
    | SpecularTexture
    | OpacityTexture
    | EmmissiveTexture
    | BumpTexture1
    | BumpTexture2
    | BumpTexture3
    | NormalTexture
    | DisplacementTexture
    | SpecularityTexture
    deriving Show

-- | Texture option specific token
data TextureToken
    = BlendUOption
    | BlendVOption
    | BoostOption
    | ModifyMapOption
    | OffsetOption
    | ScaleOption
    | TurbulenceOption
    | ResolutionOption
    | ClampOption
    | BumpOption
    | ChannelOption
    | TypeOption
    deriving Show

-- DiffuseTexture      = "map_Kd"
-- AmbientTexture      = "map_Ka"
-- SpecularTexture     = "map_Ks"
-- OpacityTexture      = "map_d"
-- EmmissiveTexture    = "map_emissive"
-- BumpTexture1        = "map_bump"
-- BumpTexture2        = "map_Bump"
-- BumpTexture3        = "bump"
-- NormalTexture       = "map_Kn"
-- DisplacementTexture = "disp"
-- SpecularityTexture  = "map_ns"

-- BlendUOption		= "-blendu"
-- BlendVOption		= "-blendv"
-- BoostOption		= "-boost"
-- ModifyMapOption	= "-mm"
-- OffsetOption		= "-o"
-- ScaleOption		= "-s"
-- TurbulenceOption	= "-t"
-- ResolutionOption	= "-texres"
-- ClampOption		= "-clamp"
-- BumpOption			= "-bm"
-- ChannelOption		= "-imfchan"
-- TypeOption			= "-type"

load :: String -- ^ File content
     -> String -- ^ Material name
     -> Model  -- ^ Model to which add material
     -> Model
load content modelName model = foldl' (flip parseLine) model fileLines
  where
    -- TODO: downcase first word
    fileLines = map replaceTabs (lines content)
    replaceTabs = map (\c -> if c == '\t' then ' ' else c)

parseLine :: String -> Model -> Model
parseLine ('k':'a':' ':xs) = setAmbientColor xs
parseLine ('k':'d':' ':xs) = setDiffuseColor xs
parseLine ('k':'s':' ':xs) = setSpecularColor xs
parseLine ('k':'e':' ':xs) = setEmissiveColor xs
parseLine ('n':'s':' ':xs) = setShineness xs
parseLine ('n':'i':' ':xs) = setIor xs
parseLine ('n':'e':' ':xs) = createMaterial xs
parseLine ('d':' ':xs)     = setAlphaValue xs
parseLine ('m':' ':xs)     = getTexture xs
parseLine ('b':' ':xs)     = getTexture xs
parseLine ('i':' ':xs)     = getIlluminationModel xs
parseLine _ = id

setAlphaValue :: String -> Model -> Model
setAlphaValue = undefined

setAmbientColor :: String -> Model -> Model
setAmbientColor = undefined

setDiffuseColor :: String -> Model -> Model
setDiffuseColor = undefined

setSpecularColor :: String -> Model -> Model
setSpecularColor = undefined

setEmissiveColor :: String -> Model -> Model
setEmissiveColor = undefined

setShineness :: String -> Model -> Model
setShineness = undefined

setIor :: String -> Model -> Model
setIor = undefined

createMaterial :: String -> Model -> Model
createMaterial = undefined

getTexture :: String -> Model -> Model
getTexture = undefined

getIlluminationModel :: String -> Model -> Model
getIlluminationModel = undefined

getColorRGBA :: String -> Color3D
getColorRGBA line = case readMay ("V3 " ++ line) of
    Just color -> color
    Nothing    -> V3 0 0 0

onMaterial :: Lens' Model Material
onMaterial = lens
    (\ model ->
        fromJust (Map.lookup (model ^. modelCurrentMaterial)
        (model ^. modelMaterialMap)))
    (\ model material -> model & modelMaterialMap
        .~ Map.insert (model ^. modelCurrentMaterial)
        material (model ^. modelMaterialMap ))
