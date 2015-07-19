-- | Defines the XglImporter.
module Codec.Soten.Importer.XglImporter (
    XglImporter(..)
) where

import qualified Data.ByteString.Lazy as ByteString
                 ( readFile
                 )
import           Data.ByteString.Lazy.Char8
                 ( unpack
                 )
import           Codec.Compression.Zlib
                 ( decompress
                 )

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.XglData
                 ( Model(..)
                 )
import qualified Codec.Soten.Parser.XglParser as Parser
                 ( getModel
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

-- | Parses model file into its internal representation. Decodess zlib files if
-- needed.
parseModelFile :: FilePath -> IO Model
parseModelFile filePath =
    if hasExtention filePath [".zgl"]
    then do
        fileContent <- ByteString.readFile filePath
        Parser.getModel (unpack $ decompress fileContent)
    else
        readFile filePath >>= Parser.getModel

