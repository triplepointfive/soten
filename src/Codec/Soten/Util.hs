module Codec.Soten.Util (
    CheckType(..)
  , hasExtention

  -- Exception handling
  , throw
  , DeadlyImporterError(..)
  -- Util
  , nothing
  , tryReadFile
) where

import           Control.Exception (IOException, try, Exception, throw)
import           Data.Char (toLower)
import           Data.Typeable (Typeable)
import           System.FilePath (takeExtension)

data CheckType
    = CheckExtension
    | CheckHeader
    deriving Show

hasExtention :: Foldable t => FilePath -> t String -> Bool
hasExtention file = elem (map toLower (takeExtension file))

data DeadlyImporterError = DeadlyImporterError String
    deriving (Show, Typeable)
instance Exception DeadlyImporterError

nothing :: a -> Maybe a -> Maybe a
nothing v Nothing  = Just v
nothing _ a        = a

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile filePath = try (readFile filePath)
