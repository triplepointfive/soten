{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
-- | The parser of XGL models.
module Codec.Soten.Parser.XglParser (
    getModel
  , getBackgroud
) where
import           Linear
                 ( V3(..)
                 )
import           Text.XML.HXT.Core

import           Codec.Soten.Data.XglData
                 ( Model(..)
                 , Author(..)
                 , LightingTag(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 )

-- | Shortcut for parser declarations.
type Field f = forall cat.ArrowXml cat => cat XmlTree f

text :: ArrowXml cat => cat XmlTree String
text = getChildren >>> getText

atTag :: ArrowXml cat => String -> cat XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- | Parses backgound tag.
getBackgroud :: Field (V3 Float)
getBackgroud = atTag "BACKGROUND" >>> atTag "BACKCOLOR" >>>
    proc x -> do
        color <- text -< x
        returnA -< (parseVector3 "," color)

-- | Parses model author.
getAuthor :: Field Author
getAuthor = atTag "AUTHOR" >>>
    proc x -> do
        name <- getObjectName -< x
        version <- text <<< atTag "VERSION" -< x
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
            color <- text -< x
            returnA -< LightingTagAmbient
                (parseVector3 "," color)

    -- | Parses directional light source.
    getDirectional :: Field LightingTag
    getDirectional = atTag "DIRECTIONALLIGHT" >>>
        proc x -> do
            direction <- text <<< atTag "DIRECTION" -< x
            diffuse <- text <<< atTag "DIFFUSE" -< x
            specular <- text <<< atTag "SPECULAR" -< x
            returnA -< LightingTagDirectional
                (parseVector3 "," direction)
                (parseVector3 "," diffuse)
                (parseVector3 "," specular)

    -- | Parses spherical light source.
    getSphere :: Field LightingTag
    getSphere = atTag "SPHEREMAP" >>>
        proc x -> do
            center <- text <<< atTag "CENTER" -< x
            radius <- text <<< atTag "RADIUS" -< x
            returnA -< LightingTagSphereMap
                (parseVector3 "," center)
                (read radius)

-- | Parses model file content.
getModel :: String -> Model
getModel fileContent = undefined
