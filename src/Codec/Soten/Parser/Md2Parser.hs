{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Soten.Parser.Md2Parser (
    load
) where

import qualified Data.ByteString as BS
import           Data.ByteString.Char8
                 ( unpack
                 )
import           Data.Serialize
                 ( decode
                 )

import           Codec.Soten.Data.Md2Data
import           Codec.Soten.Types (Color3D)
import           Codec.Soten.Util
                 ( DeadlyImporterError(..)
                 , throw
                 , parseVector3
                 )

-- | Parses a file content into model object.
load :: BS.ByteString -> Model
load fileContent = case decode (BS.take sizeOfHeader fileContent) of
    Right header -> loadWithHeader (validateHeader header) fileContent
    Left message -> throw $ DeadlyImporterError message

validateHeader :: Header -> Header
validateHeader header = if (ident header /= 844121161) || (version header /= 8)
    then throw $ DeadlyImporterError "Bad version or identifier"
    else header

loadWithHeader :: Header -> BS.ByteString -> Model
loadWithHeader header@Header{..} fileContent = Model
    { header    = header
    , skins     = loadSkins $ BS.take (sizeOfSkin * fromIntegral numSkins) $ BS.drop (fromIntegral offsetSkins) fileContent
    , texCoords = [] -- decode $ BS.take (sizeOfTexCoord * numSt) $ BS.drop offsetSt
    , triangles = [] -- decode $ BS.take (sizeOfTriangle * numTris) $ BS.drop offsetTris
    , frames    = [] -- decode $ BS.take (sizeOfFrame * numFrames) $ BS.drop offsetFrames
    , glCmds    = [] -- ![Int32]
    }

loadSkins :: BS.ByteString -> [Skin]
loadSkins string
    | BS.null string = []
    | otherwise      = Skin (unpack (BS.filter (/=0) x)) : loadSkins xs
  where
    (x, xs) = BS.splitAt sizeOfSkin string

