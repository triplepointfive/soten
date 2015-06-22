-- | Defines Importer API
module Codec.Soten.Importer (
    readModelFile
)where

import qualified Data.Map as Map

import           Linear.Matrix (M44)

-- TODO: Add post process registry
import           Codec.Soten.Internal.ImporterRegistry (getImporterInstanceList)
import           Codec.Soten.Scene (Scene(..))

readModelFile :: FilePath -> Either Scene String
readModelFile = undefined

-- | Data type to store the key hash
type Key = Int

-- | Forms the Importer structure
data Importer =
    Importer
    { -- | The imported data.
      _importerScene          :: !Scene
      -- | List of integer properties.
    , _importerIntProperty    :: !(Map.Map Key Int)
      -- | List of floating-point properties
    , _importerFloatProperty  :: !(Map.Map Key Float)
      -- | List of string properties
    , _importerStringProperty :: !(Map.Map Key String)
      -- | List of Matrix properties
    , _importerMatrixProperty :: !(Map.Map Key (M44 Float))
    }

