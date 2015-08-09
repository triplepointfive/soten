{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Soten.Parser.Md2Parser (
    load
) where

import           Data.ByteString.Lazy as BS

import           Codec.Soten.Data.Md2Data
import           Codec.Soten.Types (Color3D)
import           Codec.Soten.Util
                 ( DeadlyImporterError(..)
                 , throw
                 , parseVector3
                 )

-- | Parses a file content into model object.
load :: BS.ByteString -> Model
load fileContent = if (ident header /= 844121161) || (version header /= 8)
    then throw $ DeadlyImporterError "Bad version or identifier"
    else loadWithHeader header fileContent
  where
    header = decode (BS.take sizeOfHeader fileContent)

loadWithHeader :: Header -> BS.ByteString -> Model
loadWithHeader header@Header{..} fileContent = Model
    { header    = header
    , skins     = decode $ BS.take (sizeOfSkin * numSkins) $ BS.drop offsetSkins
    , texCoords = decode $ BS.take (sizeOfTexCoord * numSt) $ BS.drop offsetSt
    , triangles = decode $ BS.take (sizeOfTriangle * numTris) $ BS.drop offsetTris
    , frames    = decode $ BS.take (sizeOfFrame * numFrames) $ BS.drop offsetFrames
    , glCmds    = ![Int32]
    }
