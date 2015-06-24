module Codec.Soten.Importer.ObjImporter (
    ObjImporter(..)
) where

import           Control.Monad (when)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           System.Posix (getFileStatus, fileSize)

import qualified Data.Vector as V
import           Control.Lens ((^.), (&), (.~), (%~))

import           Codec.Soten.Parser.ObjFileParser (getModel)
import           Codec.Soten.Data.ObjData as Obj
import           Codec.Soten.BaseImporter (
                   BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 , newScene
                 , sceneMaterials
                 , sceneRootNode
                 , Node(..)
                 , newNode
                 , nodeName
                 )
import           Codec.Soten.Scene.Material as SM
import           Codec.Soten.Util ( CheckType(..)
                 , hasExtention
                 , throw
                 , DeadlyImporterError(..)
                 )

data ObjImporter =
    ObjImporter
    deriving Show

instance BaseImporter ObjImporter where
  canImport _ filePath CheckExtension = return $ hasExtention filePath [".obj"]
  canImport _ filePath CheckHeader    = searchFileHeaderForToken filePath tokens
    where
      tokens = ["mtllib", "usemtl", "v ", "vt ", "vn ", "o ", "g ", "s ", "f "]
  readModel _ = internalReadFile

internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = do
    content <- readFile filePath -- TODO: throw exception if file doesn't exist
    sizeOfFile <- fmap fileSize (getFileStatus filePath)
    when (sizeOfFile < objMinSize)
        (throw $ DeadlyImporterError "OBJ-file is too small.")
    createDataFromImport $ getModel (removeSlashes content) filePath
  where
    objMinSize = 16

createDataFromImport :: Model -> IO (Either String Scene)
createDataFromImport model = undefined
--  where
--    scene = newScene & sceneRootNode .~ Just rootNode

-- | Creates nodes from model\'s objects.
createNodes :: Model -> Node
createNodes model = foldl (createNode model) rootNode (model ^. modelObjects)
  where
    rootNode = newNode & nodeName .~ model ^. modelName

-- | Converts a single 'Object' to 'Node' and adds it to the root node.
createNode :: Model
           -> Node -- | Root node.
           -> Object
           -> Node
createNode model root obj = undefined

-- | Creates the materials.
createMaterials :: Model -> Scene -> Scene
createMaterials model scene =
    foldl (createMaterial model) scene (model ^. modelMaterialMap)

-- | Converts a 'Obj.Material' to 'Scene.Material' and adds to the 'Scene'.
createMaterial :: Model -> Scene -> Obj.Material -> Scene
createMaterial model scene material =
    scene & sceneMaterials %~ V.cons (setProperties SM.newMaterial)
  where
    setProperties :: SM.Material -> SM.Material
    setProperties mat = foldl addProperty mat (
        [ MaterialName           (material ^. materialName     )
        , MaterialShadingModel   shadingModel
        , MaterialColorAmbient   (material ^. meterialAmbient  )
        , MaterialColorDiffuse   (material ^. meterialDiffuse  )
        , MaterialColorSpecular  (material ^. meterialSpecular )
        , MaterialColorEmissive  (material ^. meterialEmissive )
        , MaterialColorShininess (material ^. meterialShineness)
        , MaterialColorOpacity   (material ^. meterialAlpha    )
        , MaterialRefracti       (material ^. meterialIor      )
        ] ++ textureProperties texturesMapping
        )
    shadingModel = case material ^. meterialIlluminationModel of
        0 -> ShadingModeNoShading
        2 -> ShadingModePhong
        _ -> ShadingModeGouraud
    textureProperties =
        intercalate [] .
        map textureProp .
        filter (\(name, _, _) -> not $ null name) .
        map (\(f, texType, objTex) -> (material ^. f, texType, objTex))
      where
        textureProp (name, texType, objTex)
            | fromMaybe False (Map.lookup objTex (material ^. meterialClamp)) =
                [ MaterialTexture texType name
                , MaterialMappingModeU texType TextureMapModeClamp
                , MaterialMappingModeV texType TextureMapModeClamp
                ]
            | otherwise = [ MaterialTexture texType name ]
    texturesMapping =
        [ (materialTexture
          , TextureTypeDiffuse
          , TextureDiffuseType)
        , (materialTextureSpecular
          , TextureTypeSpecular
          , TextureSpecularType)
        , (materialTextureAmbient
          , TextureTypeAmbient
          , TextureAmbientType)
        , (materialTextureEmissive
          , TextureTypeEmissive
          , TextureEmissiveType)
        , (materialTextureBump
          , TextureTypeHeight
          , TextureBumpType)
        , (materialTextureNormal
          , TextureTypeNormals
          , TextureNormalType)
        , (materialTextureSpecularity
          , TextureTypeShininess
          , TextureSpecularityType)
        , (materialTextureOpacity
          , TextureTypeOpacity
          , TextureOpacityType)
        , (materialTextureDisp
          , TextureTypeDisplacement
          , TextureDispType)
        ]

removeSlashes :: String -> String
removeSlashes str = iter str ""
  where
    iter :: String -> String -> String
    iter [] acc       = acc
    iter ('\\':xs) acc = iter (dropWhile (`elem` "\r\n") xs) acc
    iter (x:xs) acc   = iter xs (acc ++ [x])
