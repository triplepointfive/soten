{-# LANGUAGE OverloadedStrings #-}
-- | The parser of XGL models.
module Codec.Soten.Parser.XglParser (
    getModel
) where

import qualified Data.Text.Lazy as T
                 ( Text(..)
                 , fromChunks
                 , replace
                 , unpack
                 )

import           Linear
                 ( V3(..)
                 )
import           Text.XML
import           Text.XML.Cursor

import           Codec.Soten.Data.XglData
                 ( Model(..)
                 )
import           Codec.Soten.Util
                 ( parseVector3
                 )

-- | Parses 3x vector with spaces as separators.
-- TODO: Improve performance
parseTextVector :: T.Text -> V3 Float
parseTextVector = parseVector3 . T.unpack . T.replace "," " "

-- | Parses model file content.
getModel :: T.Text -> Model
getModel fileContent = Model (parseTextVector backgroudColor)
  where
    cur :: Cursor
    cur = fromDocument $ parseText_ def fileContent
    backgroudColor = T.fromChunks $ cur $| element "WORLD" &/
        element "BACKGROUND" &/ element "BACKCOLOR" &/ content
