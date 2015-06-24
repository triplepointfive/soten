module Codec.Soten.ImportDesc (
    ImporterFlag(..)
  , ImportDesc(..)
) where

-- | Mixed set of flags indicating some common features.
data ImporterFlag
    -- | It supports a textual encoding of the file format.
    = ImporterFlagSupportTextFlavour
    -- | It supports a binary encoding of the file format.
    | ImporterFlagSupportBinaryFlavour
    -- | It supports a compressed encoding of the file format.
    | ImporterFlagSupportCompressedFlavour
    -- | Reads only a very particular subset of the file format.
    | ImporterFlagLimitedSupport
    -- | Is highly experimental and should be used with care.
    | ImporterFlagExperimental
    deriving Show

-- | Meta information about a particular importer.
data ImportDesc
    ImportDesc
    { -- | Full name of the importer.
      importDescName :: !String
    } deriving Show
