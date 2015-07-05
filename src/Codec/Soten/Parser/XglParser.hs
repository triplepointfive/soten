{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
                 , LightingTag(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 )

text :: ArrowXml cat => cat XmlTree String
text = getChildren >>> getText

atTag :: ArrowXml cat => String -> cat XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

getBackgroud :: ArrowXml cat => cat XmlTree (V3 Float)
getBackgroud = atTag "BACKGROUND" >>> atTag "BACKCOLOR" >>>
    proc x -> do
        color <- text -< x
        returnA -< (parseVector3 "," color)

-- | Parses light tags.
getLighting :: ArrowXml cat => cat XmlTree LightingTag
getLighting = atTag "LIGHTING" >>> catA [getAmbient, getDirectional, getSphere]
  where
    -- | Parses ambient light.
    getAmbient :: ArrowXml cat => cat XmlTree LightingTag
    getAmbient = atTag "AMBIENT" >>>
        proc x -> do
            color <- text -< x
            returnA -< LightingTagAmbient
                (parseVector3 "," color)

    -- | Parses directional light source.
    getDirectional :: ArrowXml cat => cat XmlTree LightingTag
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
    getSphere :: ArrowXml cat => cat XmlTree LightingTag
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
