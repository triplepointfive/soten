module Codec.Soten.Util (
    CheckType(..)
  , hasExtention

  -- Exception handling
  , throw
  , DeadlyImporterError(..)
  -- Util
  , nothing
  , tryReadFile
  , parseVector3
) where

import           Control.Exception (IOException, try, Exception, throw)
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           System.FilePath (takeExtension)

import           Data.String.Utils (split)
import           Linear (V3(..))
import           Safe (readMay)

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

parseVector3 :: String -> String -> V3 Float
parseVector3 delim line = case tokens of
    [x, y, z] -> V3 x y z
    _ -> parseError
  where
    tokens = map (fromMaybe parseError . readMay) $ take 3
        $ filter (not . null) $ split delim line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "' and delimeter: '"
            ++ delim ++ "'"
