module Codec.Soten.FileImporter where

import qualified Codec.Soten.Format.ObjImporter as ObjImporter
                 (canImport)
import           Codec.Soten.Util (CheckType)

data FileImporter = ObjFileImporter

canImport :: FileImporter -> FilePath -> CheckType -> IO Bool
canImport ObjFileImporter = ObjImporter.canImport
