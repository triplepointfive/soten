module Codec.Soten.Importer.ObjImporter (
    canImport
  , ObjImporter(..)
) where

import Control.Monad (when)
import System.Posix (getFileStatus, fileSize)

import Codec.Soten.Format.ObjFormat (ObjFormat(..))
import Codec.Soten.Parser.ObjFileParser (Model(..), getModel)
import Codec.Soten.Importer (searchFileHeaderForToken)
import Codec.Soten.Util ( CheckType(..)
                        , hasExtention
                        , throw
                        , DeadlyImporterError(..)
                        )

data ObjImporter = ObjImporter

canImport :: FilePath -> CheckType -> IO Bool
canImport filePath CheckExtension = return $ hasExtention filePath [".obj"]
canImport filePath CheckHeader    = searchFileHeaderForToken filePath tokens
  where
    tokens = ["mtllib", "usemtl", "v ", "vt ", "vn ", "o ", "g ", "s ", "f "]

internalReadFile :: FilePath -> IO ObjImporter
internalReadFile filePath = do
    content <- readFile filePath -- TODO: throw exception if file doesn't exist
    sizeOfFile <- fmap fileSize (getFileStatus filePath)
    when (sizeOfFile < objMinSize)
        (throw $ DeadlyImporterError "OBJ-file is too small.")
    createDataFromImport $ getModel (removeSlashes content) filePath
  where
    objMinSize = 16

removeSlashes :: String -> String
removeSlashes str = iter str ""
  where
    iter :: String -> String -> String
    iter [] acc       = acc
    iter ('\\':xs) acc = iter (dropWhile (`elem` "\r\n") xs) acc
    iter (x:xs) acc   = iter xs (acc ++ [x])

createDataFromImport :: Model -> IO ObjImporter
createDataFromImport = undefined
