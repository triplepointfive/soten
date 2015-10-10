module Codec.Soten.Importer.ObjImporter (
    ObjImporter(..)
) where

import           Control.Monad (when)
import           System.Posix (getFileStatus, fileSize)

import           Control.Lens ((^.), (&), (%~), (.~), Lens', lens)
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Parser.ObjParser (getModel)
import           Codec.Soten.Data.ObjData as Obj
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
                 , meshName
                 , meshTextureCoords
                 , meshFaces
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
createDataFromImport model = return (Right (foldl addToken baseScene model))
  where
    baseScene = newScene
        & sceneMeshes   .~ V.singleton newMesh
        & sceneRootNode %~ nodeMeshes .~ V.singleton 0

-- | Applies a single token to a scene.
addToken :: Scene -> Token -> Scene
addToken scene (Vertex x y z)      = scene & currentMesh %~ addVert x y z
addToken scene (VertexTexture u v) = scene & currentMesh %~ addTexture u v
addToken scene (Face verts texts)  = scene & currentMesh %~ addFace verts texts
addToken scene (Object name)       = scene & currentMesh %~ meshName .~ name

-- | A lens, that assumes we have only single mesh.
currentMesh :: Lens' Scene Mesh
currentMesh = lens
    (\ scene -> V.head (scene ^. sceneMeshes))
    (\ scene mesh -> scene & sceneMeshes .~ V.singleton mesh)

-- | Adds a vertex to mesh.
addVert :: Float -> Float -> Float -> Mesh -> Mesh
addVert x y z = meshVertices %~ \v -> v `V.snoc` V3 x y z

-- | Adds a face to mesh. TODO: Order doesn't match: vertex might be connected
-- with another texture coordinate!
addFace :: [Int] -> [Int] -> Mesh -> Mesh
addFace verts _ = meshFaces %~ \v -> v `V.snoc` S.Face (V.fromList $ map pred verts)

-- | Adds texture coordinates to mesh.
addTexture :: Float -> Float -> Mesh -> Mesh
addTexture u v = meshTextureCoords %~ \vec -> vec `V.snoc` V3 u v 0
