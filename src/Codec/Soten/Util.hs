{-# LANGUAGE RankNTypes #-}
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
  , parseVector2
  , squareLength
  , degToRad
  , radToDeg
) where

import           Control.Exception (IOException, try, Exception, throw)
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import           System.FilePath (takeExtension)

import           Data.String.Utils (split)
import           Linear (V3(..), V2(..))
import           Safe (readMay)

data CheckType
    = CheckExtension
    | CheckHeader
    deriving Show

hasExtention :: Foldable t => FilePath -> t String -> Bool
hasExtention file = elem (map toLower (takeExtension file))

degToRad, radToDeg :: forall a. Fractional a => a -> a
degToRad x = x * 0.0174532925
radToDeg x = x * 57.2957795

data DeadlyImporterError = DeadlyImporterError String
    deriving (Show, Typeable)
instance Exception DeadlyImporterError

nothing :: a -> Maybe a -> Maybe a
nothing v Nothing  = Just v
nothing _ a        = a

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile filePath = try (readFile filePath)

squareLength :: V3 Float -> Float
squareLength (V3 x y z) = x * x + y * y + z * z

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

parseVector2 :: String -> String -> V2 Float
parseVector2 delim line = case tokens of
    [x, y] -> V2 x y
    _ -> parseError
  where
    tokens = map (fromMaybe parseError . readMay) $ take 2
        $ filter (not . null) $ split delim line
    parseError = throw $ DeadlyImporterError $
        "Failed to getVertex for line: '" ++ line ++ "' and delimeter: '"
            ++ delim ++ "'"
