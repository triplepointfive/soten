{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Codec.Soten.Parser.Md2Parser (
    load
) where

import           Data.Int
                 ( Int32
                 )
import           Data.Word
                 ( Word64
                 )

import qualified Data.ByteString as BS
import           Data.ByteString.Char8
                 ( unpack
                 )
import           Data.Serialize.Int
import           Data.Serialize
                 ( decode
                 , encode
                 , get
                 )
import           Data.Serialize.Get
                 ( getListOf
                 , runGet
                 )

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

-- TODO: Validate offsetEnd with sizes * num + offset of all structures
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
    , triangles = loadTriangle $ BS.take (sizeOfTriangle * fromIntegral numTris)
        $ BS.drop (fromIntegral offsetTris) fileContent
    , frames    = loadFrames numVertices $
        BS.take (frameSize * fromIntegral numFrames) $
        BS.drop (fromIntegral offsetFrames) fileContent
    , glCmds    = loadGLCommands numGLCmds $
        BS.drop (fromIntegral offsetGLCmds) fileContent
    }
  where
    frameSize = (sizeOfFrame + fromIntegral (numVertices * sizeOfVertex))

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

loadTriangle :: BS.ByteString -> [Triangle]
loadTriangle string
    | BS.null string = []
    | otherwise      = case decode x of
        Right texCoord -> texCoord : loadTriangle xs
        Left message   -> throw $ DeadlyImporterError $
            "Failed to parse MD2.Triangle: " ++ message
  where
    (x, xs) = BS.splitAt sizeOfTriangle string

loadGLCommands :: Int32 -> BS.ByteString -> [Int32]
loadGLCommands count string = case runGet (getListOf getInt32le) parseLine of
    Right commands -> commands
    Left message -> throw $ DeadlyImporterError $
        "Failed to parse MD2.GLCommand: " ++ message
  where
    parseLine = BS.concat
        [ countPrefix
        , BS.take (sizeOfGLCommand * fromIntegral count) string
        ]
    countPrefix = encode (fromIntegral count :: Word64)

loadFrames :: Int32 -> BS.ByteString -> [Frame]
loadFrames verticesCount string
    | BS.null string = []
    | otherwise      = case decode x of
        Right frame -> addVertices frame : loadFrames verticesCount xs
        Left message   -> throw $ DeadlyImporterError $
            "Failed to parse MD2.Frame: " ++ message
  where
    (x, xs) = BS.splitAt (sizeOfFrame + fromIntegral (verticesCount * sizeOfVertex)) string

    addVertices :: Frame -> Frame
    addVertices frame =
      case runGet (getListOf get) verticesString of
        Right vertices -> frame { verts = vertices }
        Left message -> throw $ DeadlyImporterError $
            "Failed to parse MD2.Frame.Vertices: " ++ message
      where
        verticesString = BS.concat
            [ encode (fromIntegral verticesCount :: Word64)
            , BS.drop sizeOfFrame x
            ]

