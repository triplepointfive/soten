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
                 , nodeChildren
                 , nodeMeshes
                 )
import           Codec.Soten.Scene.Material as SM
import           Codec.Soten.Types
                 ( Index
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 , DeadlyImporterError(..)
                 , hasExtention
                 , throw
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
-- createNodes :: Model -> Node
createNodes model =
    foldl (createNode model) (V.empty, rootNode) (model ^. modelObjects)
  where
    rootNode = newNode & nodeName .~ model ^. modelName

-- | Converts a single 'Object' to 'Node' and adds it to the root node.
createNode :: Model
           -- | (Meshes, Root node).
           -> (V.Vector Mesh, Node)
           -> Object
           -> (V.Vector Mesh, Node)
createNode model (meshes, root) obj =
    (meshes V.++ V.map snd objMeshes, root & nodeChildren %~ V.cons node)
  where
    node = newNode & nodeName .~ obj ^. objectName
        & nodeMeshes .~ V.map ((+ V.length meshes) . fst) objMeshes

    objMeshes :: V.Vector (Index, Mesh)
    objMeshes = V.indexed $ V.filter (not . blankMesh) $
        V.map (createTopology model obj) (obj ^. objectMeshes)

    blankMesh :: Mesh -> Bool
    blankMesh mesh = V.null (mesh ^. meshFaces)

-- | Creates topology data.
createTopology :: Model -> Object -> Index -> Mesh
createTopology = undefined


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

-- creaetVertexArray :: Model -> Object -> Mesh -> Mesh
-- creaetVertexArray model obj mesh
--     | empty (obj ^. objectMeshes) = mesh
--     | otherwise = mesh
--   where

removeSlashes :: String -> String
removeSlashes str = iter str ""
  where
    iter :: String -> String -> String
    iter [] acc       = acc
    iter ('\\':xs) acc = iter (dropWhile (`elem` "\r\n") xs) acc
    iter (x:xs) acc   = iter xs (acc ++ [x])
