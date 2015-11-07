{-# LANGUAGE RecordWildCards #-}
module Codec.Soten.Importer.ObjImporter (
    ObjImporter(..)
) where

import           Control.Monad (when)
import           System.Posix (getFileStatus, fileSize)

import           Control.Lens ((&), (%~), (.~), (^.))
import qualified Data.Vector as V

import           Codec.Soten.Parser.ObjParser (getModel)
import           Codec.Soten.Data.ObjData (Model(..))
import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 , newScene
                 , sceneMeshes
                 , sceneRootNode
                 , nodeMeshes
                 )
import           Codec.Soten.Scene.Mesh
                 ( Mesh(..)
                 , newMesh
                 , meshVertices
                 , meshNormals
                 , meshName
                 , meshTextureCoords
                 , meshFaces
                 , meshPrimitiveTypes
                 , PrimitiveType(..)
                 )
import qualified Codec.Soten.Scene.Mesh as S
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
    createDataFromImport $ getModel content
  where
    objMinSize = 16

-- | Folds a list of tokens ('Model') into a 'Scene' object.
createDataFromImport :: Model -> IO (Either String Scene)
createDataFromImport model = return (Right baseScene)
  where
    baseScene = newScene
        & sceneMeshes   .~ V.singleton (addMeshFaces newMesh model)
        & sceneRootNode %~ nodeMeshes .~ V.singleton 0

-- | Adds faces into a mesh.
addMeshFaces :: Mesh -> Model -> Mesh
addMeshFaces mesh model = V.foldl (addFace model) mesh (faces model)

-- | Adds a face to mesh.
addFace :: Model -> Mesh -> ([Int], [Int], [Int]) -> Mesh
addFace Model{..} mesh (verts, texts, normals) = mesh
    & meshFaces          %~ (`V.snoc` S.Face (V.fromList indices))
    & meshPrimitiveTypes %~ (`V.snoc` primitiveType)
    & meshVertices       %~ (\ v -> v V.++ faceData vertsAcc verts)
    & meshNormals        %~ (\ v -> v V.++ faceData normAcc normals)
    & meshTextureCoords  %~ (\ v -> v V.++ faceData texstsAcc texts)
    & meshName           .~ objName
  where
    lastVertIndex = V.length (mesh ^. meshVertices)
    indices = take (length verts) $ iterate succ lastVertIndex
    primitiveType = case length verts of
        1 -> PrimitivePoint
        2 -> PrimitiveLine
        3 -> PrimitiveTriangle
        _ -> PrimitivePolygone

-- | Loads a list of objects from a model with indices from a face.
faceData :: V.Vector a -> [Int] -> V.Vector a
faceData verts = V.map (\i -> verts V.! i) . V.fromList
