module Codec.Soten.Format.ObjImporter where

import Codec.Soten.Importer (searchFileHeaderForToken)
import Codec.Soten.Util (CheckType(..), hasExtention)

data ObjImporter = ObjImporter

canImport :: FilePath -> CheckType -> IO Bool
canImport filePath CheckExtension = return $ hasExtention filePath [".obj"]
canImport filePath CheckHeader    = searchFileHeaderForToken filePath tokens
  where
    tokens = ["mtllib", "usemtl", "v ", "vt ", "vn ", "o ", "g ", "s ", "f "]
