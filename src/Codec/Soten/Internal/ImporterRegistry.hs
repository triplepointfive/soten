{-# LANGUAGE ExistentialQuantification #-}
-- | Codec.Soten.Internal.ImporterRegistry
module Codec.Soten.Internal.ImporterRegistry (
    Importers
  , getImporterInstanceList
) where

import qualified Data.Vector as V

import           Codec.Soten.BaseImporter (BaseImporter(..))
import           Codec.Soten.Importer.ObjImporter (ObjImporter(..))

-- | Intermediate structure to include all instances of BaseImporter class.
data Importers = forall a. BaseImporter a => Importers a

-- | Add an instance of each worker class here.
getImporterInstanceList :: V.Vector Importers
getImporterInstanceList =
    V.fromList
    [ Importers ObjImporter
    ]
