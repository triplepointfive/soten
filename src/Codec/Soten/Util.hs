module Codec.Soten.Util (
    CheckType(..)
  , hasExtention
) where

import           Data.Char (toLower)
import           System.FilePath (takeExtension)

data CheckType = CheckExtension | CheckHeader

hasExtention :: Foldable t => FilePath -> t String -> Bool
hasExtention file = elem (map toLower (takeExtension file))
