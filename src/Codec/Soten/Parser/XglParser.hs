{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | The parser of XGL models.
module Codec.Soten.Parser.XglParser (
    getModel
) where
import           Linear
                 ( V3(..)
                 )

import           Codec.Soten.Data.XglData
                 ( Model(..)
                 , LightingTag(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 )

import Text.XML.HXT.Core

text = getChildren >>> getText
atTag tag = deep (isElem >>> hasName tag)

getAmbient     = atTag "AMBIENT" >>> text

getDirectional = atTag "DIRECTIONALLIGHT" >>>
    proc x -> do
        direction <- text <<< atTag "DIRECTION" -< x
        diffuse <- text <<< atTag "DIFFUSE" -< x
        specular <- text <<< atTag "SPECULAR" -< x
        returnA -< LightingTagDirectional
            (parseVector3 "," direction)
            (parseVector3 "," diffuse)
            (parseVector3 "," specular)

getSphere      = atTag "SPHEREMAP" >>>
    proc x -> do
        center <- text <<< atTag "CENTER" -< x
        radius <- text <<< atTag "RADIUS" -< x
        returnA -< LightingTagSphereMap
            (parseVector3 "," center)
            (read radius)

--getDirectionalLighting

-- | Parses model file content.
getModel :: String -> Model
getModel fileContent = undefined
