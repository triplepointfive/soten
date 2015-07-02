-- | Defines the XglImporter.
module Codec.Soten.Importer.XglImporter (
    XglImporter(..)
) where

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 , DeadlyImporterError(..)
                 , throw
                 , hasExtention
                 )

-- | Implementation of the XGL/ZGL importer.
data XglImporter =
    XglImporter
    deriving Show

instance BaseImporter XglImporter where
  canImport _ filePath CheckExtension =
      return $ hasExtention filePath [".xgl", ".zgl"]
  canImport _ filePath CheckHeader    =
      searchFileHeaderForToken filePath ["<WORLD>"]
  readModel _ = internalReadFile

-- | Reads file content and parsers it into the 'Scene'. Returns error messages
-- as 'String's.
internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = undefined
