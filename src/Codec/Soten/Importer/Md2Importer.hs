-- | Defines the Md2Importer.
{- Importer notes:
  - Assumes model file contains a single mesh.
  - Assumes there is a single frame.
-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Soten.Importer.Md2Importer (
    Md2Importer(..)
) where

import           Control.Lens ((&), (^.), (.~))
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.Md2Data
import           Codec.Soten.Parser.Md2Parser
                 ( getModel
                 )
import           Codec.Soten.Scene.Material
                 ( newMaterial
                 , ShadingMode(..)
                 , MaterialProperty(..)
                 , TextureType(..)
                 , addProperty
                 )
import           Codec.Soten.Scene.Mesh
                 ( Mesh
                 , PrimitiveType(..)
                 , newMesh
                 , meshPrimitiveTypes
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 , sceneMeshes
                 , sceneMaterials
                 , sceneRootNode
                 , newScene
                 , nodeMeshes
                 , newNode
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 , DeadlyImporterError(..)
                 , throw
                 , hasExtention
                 )

data Md2Importer =
    Md2Importer
    deriving Show

instance BaseImporter Md2Importer where
  canImport _ filePath CheckExtension = return $ hasExtention filePath [".md2"]
  canImport _ filePath CheckHeader    =
      searchFileHeaderForToken filePath ["IDP2"]
  readModel _ = internalReadFile

-- | Reads file content and parsers it into the 'Scene'. Returns error messages
-- as 'String's.
-- TODO: Catch exceptions.
internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = (Right . transformModel) <$> getModel filePath

-- | Transforms model data to Scene struct.
transformModel :: Model -> Scene
transformModel Model{..} = newScene
    & sceneRootNode .~ rootNode
    & sceneMaterials .~ V.singleton material
    & sceneMeshes .~ V.singleton (foldl addTriangle mesh triangles)
  where
    rootNode = newNode
        & nodeMeshes .~ V.singleton 0

    material = foldl addProperty newMaterial
        (MaterialShadingModel ShadingModeGouraud : materialProperties)

    materialProperties = if null skins
        then [ MaterialColorDiffuse (V3 1 1 1)
             , MaterialColorSpecular (V3 1 1 1)
             , MaterialColorAmbient (V3 0.05 0.05 0.05)
             , MaterialTexture TextureTypeDiffuse (texture $ head skins)
             ]
        else [ MaterialColorDiffuse (V3 0.6 0.6 0.6)
             , MaterialColorSpecular (V3 0.6 0.6 0.6)
             , MaterialColorAmbient (V3 0.05 0.05 0.05)
             , MaterialTexture TextureTypeDiffuse "texture.bmp"
             ]
    mesh = newMesh
        & meshPrimitiveTypes .~ V.singleton PrimitiveTriangle
    -- TODO: Validate is not zero
    -- TOOD: Use 1.0 if there is no texture coords
    fDivisorU = skinWidth header
    fDivisorV = skinHeight header

    -- TODO: Validate list bounds.
    -- TODO: Use vectors.
    addTriangle :: Mesh -> Triangle -> Mesh
    addTriangle mesh (Triangle [v1, v2, v3] [t1, t2, t3]) = undefined

normals :: [V3 Float]
normals =
  [ V3 (-0.525731)   0.000000    0.850651
  , V3 (-0.442863)   0.238856    0.864188
  , V3 (-0.295242)   0.000000    0.955423
  , V3 (-0.309017)   0.500000    0.809017
  , V3 (-0.162460)   0.262866    0.951056
  , V3   0.000000    0.000000    1.000000
  , V3   0.000000    0.850651    0.525731
  , V3 (-0.147621)   0.716567    0.681718
  , V3   0.147621    0.716567    0.681718
  , V3   0.000000    0.525731    0.850651
  , V3   0.309017    0.500000    0.809017
  , V3   0.525731    0.000000    0.850651
  , V3   0.295242    0.000000    0.955423
  , V3   0.442863    0.238856    0.864188
  , V3   0.162460    0.262866    0.951056
  , V3 (-0.681718)   0.147621    0.716567
  , V3 (-0.809017)   0.309017    0.500000
  , V3 (-0.587785)   0.425325    0.688191
  , V3 (-0.850651)   0.525731    0.000000
  , V3 (-0.864188)   0.442863    0.238856
  , V3 (-0.716567)   0.681718    0.147621
  , V3 (-0.688191)   0.587785    0.425325
  , V3 (-0.500000)   0.809017    0.309017
  , V3 (-0.238856)   0.864188    0.442863
  , V3 (-0.425325)   0.688191    0.587785
  , V3 (-0.716567)   0.681718  (-0.147621)
  , V3 (-0.500000)   0.809017  (-0.309017)
  , V3 (-0.525731)   0.850651    0.000000
  , V3   0.000000    0.850651  (-0.525731)
  , V3 (-0.238856)   0.864188  (-0.442863)
  , V3   0.000000    0.955423  (-0.295242)
  , V3 (-0.262866)   0.951056  (-0.162460)
  , V3   0.000000    1.000000    0.000000
  , V3   0.000000    0.955423    0.295242
  , V3 (-0.262866)   0.951056    0.162460
  , V3   0.238856    0.864188    0.442863
  , V3   0.262866    0.951056    0.162460
  , V3   0.500000    0.809017    0.309017
  , V3   0.238856    0.864188  (-0.442863)
  , V3   0.262866    0.951056  (-0.162460)
  , V3   0.500000    0.809017  (-0.309017)
  , V3   0.850651    0.525731    0.000000
  , V3   0.716567    0.681718    0.147621
  , V3   0.716567    0.681718  (-0.147621)
  , V3   0.525731    0.850651    0.000000
  , V3   0.425325    0.688191    0.587785
  , V3   0.864188    0.442863    0.238856
  , V3   0.688191    0.587785    0.425325
  , V3   0.809017    0.309017    0.500000
  , V3   0.681718    0.147621    0.716567
  , V3   0.587785    0.425325    0.688191
  , V3   0.955423    0.295242    0.000000
  , V3   1.000000    0.000000    0.000000
  , V3   0.951056    0.162460    0.262866
  , V3   0.850651  (-0.525731)   0.000000
  , V3   0.955423  (-0.295242)   0.000000
  , V3   0.864188  (-0.442863)   0.238856
  , V3   0.951056  (-0.162460)   0.262866
  , V3   0.809017  (-0.309017)   0.500000
  , V3   0.681718  (-0.147621)   0.716567
  , V3   0.850651    0.000000    0.525731
  , V3   0.864188    0.442863  (-0.238856)
  , V3   0.809017    0.309017  (-0.500000)
  , V3   0.951056    0.162460  (-0.262866)
  , V3   0.525731    0.000000  (-0.850651)
  , V3   0.681718    0.147621  (-0.716567)
  , V3   0.681718  (-0.147621) (-0.716567)
  , V3   0.850651    0.000000  (-0.525731)
  , V3   0.809017  (-0.309017) (-0.500000)
  , V3   0.864188  (-0.442863) (-0.238856)
  , V3   0.951056  (-0.162460) (-0.262866)
  , V3   0.147621    0.716567  (-0.681718)
  , V3   0.309017    0.500000  (-0.809017)
  , V3   0.425325    0.688191  (-0.587785)
  , V3   0.442863    0.238856  (-0.864188)
  , V3   0.587785    0.425325  (-0.688191)
  , V3   0.688191    0.587785  (-0.425325)
  , V3 (-0.147621)   0.716567  (-0.681718)
  , V3 (-0.309017)   0.500000  (-0.809017)
  , V3   0.000000    0.525731  (-0.850651)
  , V3 (-0.525731)   0.000000  (-0.850651)
  , V3 (-0.442863)   0.238856  (-0.864188)
  , V3 (-0.295242)   0.000000  (-0.955423)
  , V3 (-0.162460)   0.262866  (-0.951056)
  , V3   0.000000    0.000000  (-1.000000)
  , V3   0.295242    0.000000  (-0.955423)
  , V3   0.162460    0.262866  (-0.951056)
  , V3 (-0.442863) (-0.238856) (-0.864188)
  , V3 (-0.309017) (-0.500000) (-0.809017)
  , V3 (-0.162460) (-0.262866) (-0.951056)
  , V3   0.000000  (-0.850651) (-0.525731)
  , V3 (-0.147621) (-0.716567) (-0.681718)
  , V3   0.147621  (-0.716567) (-0.681718)
  , V3   0.000000  (-0.525731) (-0.850651)
  , V3   0.309017  (-0.500000) (-0.809017)
  , V3   0.442863  (-0.238856) (-0.864188)
  , V3   0.162460  (-0.262866) (-0.951056)
  , V3   0.238856  (-0.864188) (-0.442863)
  , V3   0.500000  (-0.809017) (-0.309017)
  , V3   0.425325  (-0.688191) (-0.587785)
  , V3   0.716567  (-0.681718) (-0.147621)
  , V3   0.688191  (-0.587785) (-0.425325)
  , V3   0.587785  (-0.425325) (-0.688191)
  , V3   0.000000  (-0.955423) (-0.295242)
  , V3   0.000000  (-1.000000)   0.000000
  , V3   0.262866  (-0.951056) (-0.162460)
  , V3   0.000000  (-0.850651)   0.525731
  , V3   0.000000  (-0.955423)   0.295242
  , V3   0.238856  (-0.864188)   0.442863
  , V3   0.262866  (-0.951056)   0.162460
  , V3   0.500000  (-0.809017)   0.309017
  , V3   0.716567  (-0.681718)   0.147621
  , V3   0.525731  (-0.850651)   0.000000
  , V3 (-0.238856) (-0.864188) (-0.442863)
  , V3 (-0.500000) (-0.809017) (-0.309017)
  , V3 (-0.262866) (-0.951056) (-0.162460)
  , V3 (-0.850651) (-0.525731)   0.000000
  , V3 (-0.716567) (-0.681718) (-0.147621)
  , V3 (-0.716567) (-0.681718)   0.147621
  , V3 (-0.525731) (-0.850651)   0.000000
  , V3 (-0.500000) (-0.809017)   0.309017
  , V3 (-0.238856) (-0.864188)   0.442863
  , V3 (-0.262866) (-0.951056)   0.162460
  , V3 (-0.864188) (-0.442863)   0.238856
  , V3 (-0.809017) (-0.309017)   0.500000
  , V3 (-0.688191) (-0.587785)   0.425325
  , V3 (-0.681718) (-0.147621)   0.716567
  , V3 (-0.442863) (-0.238856)   0.864188
  , V3 (-0.587785) (-0.425325)   0.688191
  , V3 (-0.309017) (-0.500000)   0.809017
  , V3 (-0.147621) (-0.716567)   0.681718
  , V3 (-0.425325) (-0.688191)   0.587785
  , V3 (-0.162460) (-0.262866)   0.951056
  , V3   0.442863  (-0.238856)   0.864188
  , V3   0.162460  (-0.262866)   0.951056
  , V3   0.309017  (-0.500000)   0.809017
  , V3   0.147621  (-0.716567)   0.681718
  , V3   0.000000  (-0.525731)   0.850651
  , V3   0.425325  (-0.688191)   0.587785
  , V3   0.587785  (-0.425325)   0.688191
  , V3   0.688191  (-0.587785)   0.425325
  , V3 (-0.955423)   0.295242    0.000000
  , V3 (-0.951056)   0.162460    0.262866
  , V3 (-1.000000)   0.000000    0.000000
  , V3 (-0.850651)   0.000000    0.525731
  , V3 (-0.955423) (-0.295242)   0.000000
  , V3 (-0.951056) (-0.162460)   0.262866
  , V3 (-0.864188)   0.442863  (-0.238856)
  , V3 (-0.951056)   0.162460  (-0.262866)
  , V3 (-0.809017)   0.309017  (-0.500000)
  , V3 (-0.864188) (-0.442863) (-0.238856)
  , V3 (-0.951056) (-0.162460) (-0.262866)
  , V3 (-0.809017) (-0.309017) (-0.500000)
  , V3 (-0.681718)   0.147621  (-0.716567)
  , V3 (-0.681718) (-0.147621) (-0.716567)
  , V3 (-0.850651)   0.000000  (-0.525731)
  , V3 (-0.688191)   0.587785  (-0.425325)
  , V3 (-0.587785)   0.425325  (-0.688191)
  , V3 (-0.425325)   0.688191  (-0.587785)
  , V3 (-0.425325) (-0.688191) (-0.587785)
  , V3 (-0.587785) (-0.425325) (-0.688191)
  , V3 (-0.688191) (-0.587785) (-0.425325)
  ]
