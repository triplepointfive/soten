-- | Defines the StlImporter.
module Codec.Soten.Importer.StlImporter (
    StlImporter(..)
) where

import           Control.Monad
                 ( liftM2
                 )
import qualified Data.ByteString as BS
import           Data.Maybe
                 ( isJust
                 , fromJust
                 )

import           Codec.Soten.BaseImporter
                 ( BaseImporter(..)
                 , searchFileHeaderForToken
                 )
import           Codec.Soten.Data.StlData
                 ( Model
                 )
import           Codec.Soten.Parser.StlParser
                 ( parseASCII
                 , parseBinary
                 )
import           Codec.Soten.Scene
                 ( Scene(..)
                 )
import           Codec.Soten.Util
                 ( CheckType(..)
                 , DeadlyImporterError(..)
                 , throw
                 , hasExtention
                 )

-- | Importer for the sterolithography STL file format.
data StlImporter =
    StlImporter
    deriving Show

instance BaseImporter StlImporter where
  canImport _ filePath CheckExtension = return $ hasExtention filePath [".stl"]
  canImport p filePath CheckHeader    =
      liftM2 (||)
        (canImport p filePath CheckExtension)
        (searchFileHeaderForToken filePath ["solid", "STL"])
  readModel _ = internalReadFile

-- | Reads file content and parsers it into the 'Scene'. Returns error messages
-- as 'String's.
internalReadFile :: FilePath -> IO (Either String Scene)
internalReadFile filePath = undefined

-- | Checks which wheter file has binary or ascii representation.
getModel :: FilePath -> IO Model
getModel fileName = do
    binary <- isBinary fileName
    ascii <- isAscii fileName
    if isJust binary
        then return $ parseBinary (fromJust binary)
        else if isJust ascii
            then return $ parseASCII (fromJust ascii)
            else throw $ DeadlyImporterError $
                "Failed to determine STL storage representation for "
                ++ fileName ++ "."

-- | Checks if file starts with "solid" token. Note: this check is not
-- sufficient, binary format could also begin with "solid".
isAscii :: FilePath -> IO (Maybe String)
isAscii fileName = do
    content <- readFile fileName
    if take 5 content == "solid"
        then return (Just content)
        else return Nothing

-- | Checks if file size matching the formula:
-- 80 byte header, 4 byte face count, 50 bytes per face
isBinary :: FilePath -> IO (Maybe BS.ByteString )
isBinary fileName = do
    content <- BS.readFile fileName
    if BS.length content < 84
        then return Nothing
        else
            let [w1, w2, w3, w4] = BS.unpack (BS.take 4 (BS.drop 80 content))
                facetsCount = w1 + 256 * (w2 + 256 * (w3 + 256 * w4))
            in
            if BS.length content == fromIntegral (84 + facetsCount * 50)
                then return (Just content)
                else return Nothing