{-# LANGUAGE RecordWildCards #-}
-- | Defines the XglImporter.
{- Importer notes:
  - Ignores ambient and spherermap lights.

-}
module Codec.Soten.Importer.XglImporter (
    XglImporter(..)
  -- Only for testing purpose.
  , transformToScene
  , transformLights
) where

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

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.XglData
                 ( Model(..)
                 , LightingTag(..)
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

-- | Transform direction light into Light object.
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

