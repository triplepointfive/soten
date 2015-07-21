{-# LANGUAGE RecordWildCards #-}
-- | Defines the XglImporter.
{- Importer notes:
  - Ignores ambient and spherermap lights.

-}
module Codec.Soten.Importer.XglImporter (
    XglImporter(..)
  -- Only for testing purpose.
  , transformLights
  , transformMaterials
  , transformToScene
) where

import           Data.Maybe
                 ( catMaybes
                 )

import qualified Data.ByteString.Lazy as ByteString
                 ( readFile
                 )
import           Data.ByteString.Lazy.Char8
                 ( unpack
                 )
import           Data.Vector
                 ( fromList
                 )
import           Codec.Compression.Zlib
                 ( decompress
                 )
import           Control.Lens ((&), (^.), (.~))
import           Linear
                 ( V3(..)
                 , cross
                 , dot
                 , normalize
                 )
import           Linear.Matrix
                 ( M44
                 , (!!*)
                 , identity
                 , mkTransformationMat
                 )

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.XglData as X
                 ( Model(..)
                 , LightingTag(..)
                 , Material(..)
                 , Transform(..)
                 )
import qualified Codec.Soten.Parser.XglParser as Parser
                 ( getModel
                 )
import           Codec.Soten.Scene.Light
                 ( Light(..)
                 , LightSource(LightDirectional)
                 , newLight
                 , lightType
                 , lightDirection
                 , lightColorDiffuse
                 , lightColorSpecular
                 )
import           Codec.Soten.Scene.Material as S
                 ( Material(..)
                 , MaterialProperty(..)
                 , newMaterial
                 , addProperty
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 , newScene
                 , sceneLights
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 , DeadlyImporterError(..)
                 , throw
                 , hasExtention
                 , squareLength
                 )

-- | Implementation of the XGL/ZGL importer.
data XglImporter =
    XglImporter
    deriving Show

instance BaseImporter XglImporter where
  canImport _ filePath CheckExtension =
      return $ hasExtention filePath [".xgl", ".zgl"]
  canImport _ filePath CheckHeader    =
      searchFileHeaderForToken filePath ["<WORLD>"]
  readModel _ = internalReadFile

-- | Reads file content and parsers it into the 'Scene'. Returns error messages
-- as 'String's.
internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = Right <$> transformToScene <$> parseModelFile filePath

-- | Parses model file into its internal representation. Decodess zlib files if
-- needed.
parseModelFile :: FilePath -> IO Model
parseModelFile filePath =
    if hasExtention filePath [".zgl"]
    then do
        fileContent <- ByteString.readFile filePath
        Parser.getModel (unpack $ decompress fileContent)
    else
        readFile filePath >>= Parser.getModel

-- | Transforms internal model representation into the 'Scene' object.
transformToScene :: Model -> Scene
transformToScene Model{..} =
    newScene
        -- & sceneMaterials .~ V.singleton mat
        -- & sceneMeshes    .~ V.singleton mesh
        -- & sceneRootNode  .~ Just node
        & sceneLights .~ fromList (transformLights modelLightingTags)

-- | Transforms direction light into Light object.
transformLights :: [LightingTag] -> [Light]
transformLights = foldl tagToLight []
  where
    tagToLight :: [Light] -> LightingTag -> [Light]
    tagToLight acc LightingTagDirectional{..} = light : acc
      where
        light = newLight
            & lightType          .~ LightDirectional
            & lightDirection     .~ lightingTagDirectionalDirection
            & lightColorDiffuse  .~ lightingTagDirectionalDiffuse
            & lightColorSpecular .~ lightingTagDirectionalSpecular
    tagToLight acc _ = acc

-- | Transforms internal material into scene's ones.
transformMaterials :: [X.Material] -> [S.Material]
transformMaterials materials = undefined
  where
    -- TODO: Material id is missing!
    sceneMat X.Material{..} =
        foldl addProperty newMaterial (requiredProperties ++ optionalProperties)
      where
        requiredProperties =
          [ MaterialName "DefaultMaterial"
          , MaterialColorAmbient materialAmbient
          , MaterialColorDiffuse materialDiffuse
          ]
        optionalProperties = catMaybes
          [ fmap MaterialColorSpecular materialSpecular
          , fmap MaterialColorEmissive materialEmiss
          , fmap MaterialColorShininess materialShine
          , fmap MaterialColorOpacity materialAlpha
          ]

-- | Calculates matrix of transformation.
transformation :: Transform -> M44 Float
transformation Transform{..}
    | squareLength transForward < 1e-4 = identity
    | squareLength transUp < 1e-4      = identity
    | up `dot` forward > 1e-4          = identity
    | otherwise = mkTransformationMat scaledRotMat transForward
  where
    forward      = normalize transForward
    up           = normalize transUp
    right        = forward `cross` up
    rotateMatrix = V3 right up forward
    scaledRotMat = maybe rotateMatrix (rotateMatrix !!* ) transScale
