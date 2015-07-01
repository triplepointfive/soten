{-# LANGUAGE TemplateHaskell #-}
-- | Defines Importer API.
module Codec.Soten.Importer (
    Importer
  , importerScene
  , importerIntProperty
  , importerFloatProperty
  , importerStringProperty
  , importerMatrixProperty
  , readModelFile
  , resetImporterScene
)where

import qualified Data.Map as Map
import           System.Directory
                 ( doesFileExist
                 )

import           Control.Lens
                 ( makeLenses
                 , (^.)
                 , (.~)
                 , (&)
                 )
import qualified Data.Vector as V
import           Linear.Matrix
                 ( M44
                 )

-- TODO: Add post process registry
import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 )
import           Codec.Soten.Internal.ImporterRegistry
                 ( Importers(..)
                 , getImporterInstanceList
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 , newScene
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 )

-- | Data type to store the key hash.
type Key = Int

-- | Forms the Importer structure.
data Importer =
    Importer
    { -- | The imported data.
      _importerScene          :: !Scene
      -- | Format-specific importer worker objects - one for each format it
      -- can read.
    , _importerImporters      :: !(V.Vector Importers)
      -- | List of integer properties.
    , _importerIntProperty    :: !(Map.Map Key Int)
      -- | List of floating-point properties
    , _importerFloatProperty  :: !(Map.Map Key Float)
      -- | List of string properties
    , _importerStringProperty :: !(Map.Map Key String)
      -- | List of Matrix properties
    , _importerMatrixProperty :: !(Map.Map Key (M44 Float))
    } deriving Show
makeLenses ''Importer

newImporter :: Importer
newImporter =
    Importer
    { _importerScene          = newScene
    , _importerImporters      = getImporterInstanceList
    , _importerIntProperty    = Map.empty
    , _importerFloatProperty  = Map.empty
    , _importerStringProperty = Map.empty
    , _importerMatrixProperty = Map.empty
    }

-- | Creates new scene for importer.
resetImporterScene :: Importer -> Importer
resetImporterScene importer = importer & importerScene .~ newScene

-- | Reads the given file and returns its contents if successful.
readModelFile :: FilePath -> IO (Either String Scene)
readModelFile fileName =
    doesFileExist fileName >>=
        bool
            (return fileNotExist)
            (importerByExtension >>=
                maybe
                    (importerByHeader >>=
                        maybe (return parserNotFound) parse)
                    parse)
  where
    importer = newImporter
    fileNotExist = Left ("Unable to open file \"" ++ fileName ++ "\";")
    parserNotFound = Left (
        "No suitable reader found for the file format of file \""
        ++ fileName ++ "\".")
    importerByExtension = findImporterWithExtension fileName
        CheckExtension importer
    importerByHeader = findImporterWithExtension fileName
        CheckHeader importer
    parse = parseWithImporter fileName newImporter

parseWithImporter :: FilePath
                  -> Importer
                  -> Importers
                  -> IO (Either String Scene)
parseWithImporter filePath _ (Importers imp) =
    readModel imp filePath

-- | Find an worker class which can handle the file by file extension.
findImporterWithExtension :: FilePath
                          -> CheckType
                          -> Importer
                          -> IO (Maybe Importers)
findImporterWithExtension fileName checker importer =
    V.filterM search (importer ^. importerImporters) >>= maybeHead
  where
    search :: Importers -> IO Bool
    search (Importers imp) = canImport imp fileName checker

-- | Safe head function for Vector.
maybeHead :: Monad m => V.Vector a -> m (Maybe a)
maybeHead vector = return (vector V.!? 0)

-- | Like 'maybe' function for boolean value.
bool :: a -> a -> Bool -> a
bool f t b = if b then t else f
