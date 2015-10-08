module Codec.Soten.Importer.ObjImporter (
    ObjImporter(..)
) where

import           Control.Monad (when)
import           System.Posix (getFileStatus, fileSize)

import           Codec.Soten.Parser.ObjParser (getModel)
import           Codec.Soten.Data.ObjData as Obj
import           Codec.Soten.BaseImporter (
                   BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
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
    createDataFromImport $ getModel content
  where
    objMinSize = 16

createDataFromImport :: Model -> IO (Either String Scene)
createDataFromImport _ = return (Right undefined)
