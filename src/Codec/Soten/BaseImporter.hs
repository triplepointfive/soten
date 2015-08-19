{-# LANGUAGE RankNTypes #-}
module Codec.Soten.BaseImporter (
    BaseImporter(..)
  , searchFileHeaderForToken
) where

import           Control.Exception (IOException, try)
import           Data.Char (toLower)

import qualified Data.ByteString.Char8 as B

import           Codec.Soten.Scene
                 ( Scene(..)
                 )
import           Codec.Soten.Util (CheckType)

-- | A common interface for all importer worker classes.
class Show a => BaseImporter a where
    -- | Returns whether the class can handle the format of the given file.
    canImport :: a -> FilePath -> CheckType -> IO Bool
    -- | Imports the given file and returns the imported data.
    readModel :: a -> FilePath -> IO (Either String Scene)

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
    searchHeader context = return $ any (`B.isInfixOf` context) listTokens
      where listTokens = map (B.pack . map toLower) tokens
