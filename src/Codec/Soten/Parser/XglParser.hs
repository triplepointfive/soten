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
                 , V2(..)
                 )
import           Text.XML.HXT.Core

import           Codec.Soten.Data.XglData
                 ( Model(..)
                 , Mesh(..)
                 , Author(..)
                 , LightingTag(..)
                 , Face(..)
                 , Vertex(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 , parseVector2
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

-- | Parses a 2x vector.
getVector2 :: Field (V2 Float)
getVector2 = proc x -> do
    vertex   <- text -< x
    returnA -< parseVector2 "," vertex

-- | Parses a 3x vector.
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
        idMesh   <- getAttrValue0 "ID"                -< x
        vertices <- listA (getVector3 <<< atTag "P")  -< x
        normals  <- listA (getVector3 <<< atTag "N")  -< x
        textureC <- listA (getVector2 <<< atTag "TC") -< x
        faces    <- listA (getFace)                   -< x
        returnA -< Mesh
            { meshID                 = read idMesh
            , meshPositions          = vertices
            , meshNormals            = normals
            , meshTextureCoordinates = textureC
            , meshFaces              = faces
            }

-- | Parses vertex references.
getVertex :: String -> Field Vertex
getVertex parent = atTag parent >>>
    proc x -> do
        pRef     <- text <<< atTag "PREF"             -< x
        nRef     <- getMaybe (text <<< atTag "NREF")  -< x
        tcRef    <- getMaybe (text <<< atTag "TCREF") -< x
        returnA -< Vertex
            { vertexPosition = read pRef
            , vertexNormal   = fmap read nRef
            , vertexTexture  = fmap read tcRef
            }

-- | Parses face tag.
getFace :: Field Face
getFace = atTag "F" >>>
    proc x -> do
        matID    <- text <<< atTag "MATREF" -< x
        vertex1  <- getVertex "FV1"         -< x
        vertex2  <- getVertex "FV2"         -< x
        vertex3  <- getVertex "FV3"         -< x
        returnA -< Face
            { faceMaterial = read matID
            , faceVertex1  = vertex1
            , faceVertex2  = vertex2
            , faceVertex3  = vertex3
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
