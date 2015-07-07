{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
-- | The parser of XGL models.
module Codec.Soten.Parser.XglParser (
    getModel
  , getBackgroud
) where

import           Data.Maybe
                 ( listToMaybe
                 )

import           Linear
                 ( V3(..)
                 )
import           Text.XML.HXT.Core

import           Codec.Soten.Data.XglData
                 ( Model(..)
                 , Mesh(..)
                 , Author(..)
                 , LightingTag(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 )

-- | Shortcut for parser declarations.
type Field f = forall cat.ArrowXml cat => cat XmlTree f

-- | Parses value as 'String'.
text :: ArrowXml cat => cat XmlTree String
text = getChildren >>> getText

-- | Helper for tags.
atTag :: ArrowXml cat => String -> cat XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- | Helper for attributes.
atAttr :: ArrowXml cat => String -> cat XmlTree XmlTree
atAttr tag = deep (isElem >>> hasAttr tag)

-- | Maybe parses a value, maybe not.
getMaybe :: Field c -> Field (Maybe c)
getMaybe arrow = (arrow >>> arr Just) `orElse` (constA Nothing)

-- | Parses a vertex.
getVector3 :: Field (V3 Float)
getVector3 = proc x -> do
    vertex   <- text -< x
    returnA -< parseVector3 "," vertex

-- | Parses backgound tag.
getBackgroud :: Field (V3 Float)
getBackgroud = atTag "BACKGROUND" >>> atTag "BACKCOLOR" >>> getVector3

-- | Parses model author.
getAuthor :: Field Author
getAuthor = atTag "AUTHOR" >>>
    proc x -> do
        name     <- getObjectName            -< x
        version  <- text <<< atTag "VERSION" -< x
        returnA -< Author name version

-- | Parses name of an object.
getObjectName :: Field String
getObjectName = atTag "NAME" >>> text

-- | Parses light tags.
getLighting :: Field LightingTag
getLighting = atTag "LIGHTING" >>> catA [getAmbient, getDirectional, getSphere]
  where
    -- | Parses ambient light.
    getAmbient :: Field LightingTag
    getAmbient = atTag "AMBIENT" >>>
        proc x -> do
            color    <- text -< x
            returnA -< LightingTagAmbient
                (parseVector3 "," color)

    -- | Parses directional light source.
    getDirectional :: Field LightingTag
    getDirectional = atTag "DIRECTIONALLIGHT" >>>
        proc x -> do
            direction <- text <<< atTag "DIRECTION" -< x
            diffuse   <- text <<< atTag "DIFFUSE"   -< x
            specular  <- text <<< atTag "SPECULAR"  -< x
            returnA  -< LightingTagDirectional
                (parseVector3 "," direction)
                (parseVector3 "," diffuse)
                (parseVector3 "," specular)

    -- | Parses spherical light source.
    getSphere :: Field LightingTag
    getSphere = atTag "SPHEREMAP" >>>
        proc x -> do
            center   <- text <<< atTag "CENTER" -< x
            radius   <- text <<< atTag "RADIUS" -< x
            returnA -< LightingTagSphereMap
                (parseVector3 "," center)
                (read radius)

-- | Parses mesh tags.
getMesh :: Field Mesh
getMesh = atTag "MESH" >>>
    proc x -> do
        id       <- getAttrValue0 "ID"               -< x
        vertices <- listA (getVector3 <<< atTag "P") -< x
        normals  <- listA (getVector3 <<< atTag "N") -< x
        returnA -< Mesh
          { meshID       = (read id)
          , meshVertices = vertices
          , meshNormals  = normals
          }

-- | Parses model file content.
getModel :: String -> Field Model
getModel fileContent = atTag "WORLD" >>>
    proc x -> do
        name       <- getMaybe getObjectName -< x
        lights     <- listA getLighting      -< x
        author     <- getMaybe getAuthor     -< x
        background <- getBackgroud           -< x
        returnA   -< Model
            { modelBackgroundColor = background
            , modelLightingTags    = lights
            , modelName            = name
            , modelAuthor          = author
            }
