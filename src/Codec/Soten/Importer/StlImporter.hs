-- | Defines the StlImporter.
module Codec.Soten.Importer.StlImporter (
    StlImporter(..)
) where

import           Control.Monad
                 ( liftM2
                 )
import qualified Data.ByteString as BS
import           Data.Maybe
                 ( isJust
                 , fromJust
                 )
import qualified Data.Vector as V

import           Control.Lens ((&), (^.), (.~))
import           Linear
                 ( V3(..)
                 )

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.StlData
                 ( Model
                 , modelName
                 , modelFacets
                 , facetNormal
                 , facetVertices
                 )
import           Codec.Soten.Parser.StlParser
                 ( parseASCII
                 , parseBinary
                 )
import           Codec.Soten.Scene.Material
                 ( MaterialProperty(..)
                 , addProperty
                 , newMaterial
                 )
import           Codec.Soten.Scene.Mesh
                 ( Mesh
                 , meshFaces
                 , meshMaterialIndex
                 , meshName
                 , meshNormals
                 , meshVertices
                 , newMesh
                 , Face(..)
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

-- | Importer for the sterolithography STL file format.
data StlImporter =
    StlImporter
    deriving Show

instance BaseImporter StlImporter where
  canImport _ filePath CheckExtension = return $ hasExtention filePath [".stl"]
  canImport p filePath CheckHeader    =
      liftM2 (||)
        (canImport p filePath CheckExtension)
        (searchFileHeaderForToken filePath ["solid", "STL"])
  readModel _ = internalReadFile

-- | Reads file content and parsers it into the 'Scene'. Returns error messages
-- as 'String's.
internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = do
    -- TODO: Catch exceptions here.
    model <- getModel filePath
    let scene = newScene & sceneMaterials .~ V.singleton mat
            & sceneMeshes .~ V.singleton mesh
            & sceneRootNode .~ node
        mesh = getMesh model
        node = newNode & nodeMeshes .~ V.singleton 0
        in return $ Right scene
  where
    -- TODO: Move DefaultMaterial to constants
    mat = foldl addProperty newMaterial
        [ MaterialName "DefaultMaterial"
        , MaterialColorDiffuse clrDiffuseColor
        , MaterialColorSpecular clrDiffuseColor
        , MaterialColorAmbient (V3 0.5 0.5 0.5)
        ]
    clrDiffuseColor = V3 0.6 0.6 0.6

-- | Creates mesh according to the 'Facet' data.
getMesh :: Model -> Mesh
getMesh model = newMesh
    & meshName .~ (model ^. modelName)
    & meshNormals .~ normals
    & meshVertices .~ vertices
    & meshFaces .~ faces
    & meshMaterialIndex .~ Just 0
  where
    vertices = foldl (\ vp facet -> vp V.++ (facet ^. facetVertices))
        V.empty (model ^. modelFacets)

    normals = foldl (\ vn facet -> V.snoc vn (facet ^. facetNormal))
        V.empty (model ^. modelFacets)

    faces = V.imap (\ i _ -> Face (V.fromList [3 * i, 3 * i + 1, 3 * i + 2 ]))
        (V.take (V.length vertices `div` 3) vertices)

-- | Checks which wheter file has binary or ascii representation.
getModel :: FilePath -> IO Model
getModel fileName = do
    binary <- isBinary fileName
    ascii <- isAscii fileName
    if isJust binary
        then return $ parseBinary (fromJust binary)
        else if isJust ascii
            then return $ parseASCII (fromJust ascii)
            else throw $ DeadlyImporterError $
                "Failed to determine STL storage representation for "
                ++ fileName ++ "."

-- | Checks if file starts with "solid" token. Note: this check is not
-- sufficient, binary format could also begin with "solid".
isAscii :: FilePath -> IO (Maybe String)
isAscii fileName = do
    content <- readFile fileName
    if take 5 content == "solid"
        then return (Just content)
        else return Nothing

-- | Checks if file size matching the formula:
-- 80 byte header, 4 byte face count, 50 bytes per face
isBinary :: FilePath -> IO (Maybe BS.ByteString )
isBinary fileName = do
    content <- BS.readFile fileName
    if BS.length content < 84
        then return Nothing
        else
            let [w1, w2, w3, w4] = BS.unpack (BS.take 4 (BS.drop 80 content))
                facetsCount = w1 + 255 * (w2 + 255 * (w3 + 255 * w4))
            in
            if BS.length content ==  84 + fromIntegral facetsCount * 50
                then return (Just content)
                else return Nothing
