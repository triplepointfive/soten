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
import           Linear (V3(..))

import           Codec.Soten.Data.Md2Data
import           Codec.Soten.Util
                 ( DeadlyImporterError(..)
                 , throw
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
    , skins     = loadSkins $ BS.take (sizeOfSkin * fromIntegral numSkins) $
        BS.drop (fromIntegral offsetSkins) fileContent
    , texCoords = loadTexCoord $ BS.take (sizeOfTexCoord * fromIntegral numSt) $
        BS.drop (fromIntegral offsetSt) fileContent
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

loadTexCoord :: BS.ByteString -> [TexCoord]
loadTexCoord string
    | BS.null string = []
    | otherwise      = case decode x of
        Right texCoord -> texCoord : loadTexCoord xs
        Left message   -> throw $ DeadlyImporterError $
            "Failed to parse MD2.TexCoord: " ++ message
  where
    (x, xs) = BS.splitAt sizeOfTexCoord string
