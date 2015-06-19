module Codec.Soten.Importer (
    searchFileHeaderForToken
) where

import           Control.Exception (IOException, try)
import           Data.Char (toLower)

import qualified Data.ByteString.Char8 as B

searchFileHeaderForToken :: FilePath -> [String] -> IO Bool
searchFileHeaderForToken filePath tokens =
    tryReadFile >>= either failSearch searchHeader
  where
    tryReadFile :: IO (Either IOException B.ByteString)
    tryReadFile = try (fmap proceedFile (B.readFile filePath))

    proceedFile :: B.ByteString -> B.ByteString
    proceedFile = B.map toLower . B.take searchBytes
      where searchBytes = 200

    failSearch _ = return False

    searchHeader :: B.ByteString -> IO Bool
    searchHeader context = return $ any (`B.isInfixOf ` context) listTokens
      where listTokens = map B.pack tokens


