-- | The parser of STL models.
module Codec.Soten.Parser.StlParser (
    parseASCII
  , parseBinary
) where

import           Control.Lens ((&), (%~), (.~))
import qualified Data.ByteString as BS
import           Data.Serialize
import qualified Data.Vector as V
import           Linear (V3(..))

import           Codec.Soten.Data.StlData
                 ( Model(..)
                 , modelName
                 , modelFacets
                 , newModel
                 , Facet(..)
                 , facetNormal
                 , facetVertices
                 , newFacet
                 )
import           Codec.Soten.Util
                 ( DeadlyImporterError(..)
                 , throw
                 , parseVector3
                 )

-- | Represents a state after parsing of each line.
data ParserState
    -- | Initial state or after finishing block
    = StateNone
    -- | Started building of 'Facet'
    | StateFacet (V3 Float) (V.Vector (V3 Float))
    deriving (Show, Eq)

-- | Parse ASCII file.
parseASCII :: String -> Model
parseASCII content = case parser of
    StateNone -> model
    _ -> throw $ DeadlyImporterError
        $ "Invalid parser state on file end: '" ++ show parser ++ "'"
  where
    (model, parser) = foldl (flip (parseLine . removeTrailingSpaces))
        (newModel, StateNone) (lines content)

-- | Parse binary file.
parseBinary :: BS.ByteString -> Model
parseBinary content =
    newModel & modelFacets .~ V.map parseToken (tokens (BS.drop 84 content))
  where
    tokens :: BS.ByteString -> V.Vector BS.ByteString
    tokens rest
        | BS.null rest = V.empty
        | otherwise    = let (token, remainder) = BS.splitAt 50 rest
            in V.cons token (tokens remainder)

-- | Converts 50 bytes of binary file into the facet.
parseToken :: BS.ByteString -> Facet
parseToken token = case decode token of
    Left msg -> throw $ DeadlyImporterError msg
    Right facet -> facet

-- | Parses a signle line.
parseLine :: String -> (Model, ParserState) -> (Model, ParserState)
parseLine ('s':'o':'l':'i':'d':' ':name) = setName name
parseLine ('e':'n':'d':_) = id
parseLine ('o':'u':'t':_) = id
parseLine ('f':'a':'c':'e':'t':' ':'n':'o':'r':'m':'a':'l':' ':normal) =
    setNormal normal
parseLine ('v':'e':'r':'t':'e':'x':' ':vertex) = setVertex vertex
parseLine line = throw $ DeadlyImporterError $
    "Received unkown token for line: '" ++ line ++ "'"

-- | Sets a model name.
setName :: String -> (Model, ParserState) -> (Model, ParserState)
setName name (model, _) = (model & modelName .~ name, StateNone)

-- | Parsers a normal for facet.
setNormal :: String -> (Model, ParserState) -> (Model, ParserState)
setNormal normal (model, parser) =
    (model, addNormal (parseVector3 normal) parser)

-- | Adds a normal to the current parser.
addNormal :: V3 Float -> ParserState -> ParserState
addNormal v StateNone = StateFacet v V.empty
addNormal _ parser = throw $ DeadlyImporterError $
    "addNormal is called for initialized parser: '" ++ show parser ++ "'"

-- | Adds a vertext to the current facet or initializates new.
setVertex :: String -> (Model, ParserState) -> (Model, ParserState)
setVertex vertex (model, parser) = either
    (\ facet -> (model & modelFacets %~ flip V.snoc facet, StateNone))
    (\ updParser -> (model, updParser))
    (addVertex (parseVector3 vertex) parser)

-- | Adds a vertex to parser state. Returns new facet for each 3 vertices.
addVertex :: V3 Float -> ParserState -> Either Facet ParserState
addVertex v StateNone = throw $ DeadlyImporterError $
    "addVertex adds a vertex without normals: '" ++ show v ++ "'"
addVertex v (StateFacet normal vertices)
    | V.length vertices == 2 = Left $
        newFacet & facetNormal .~ normal & facetVertices .~ V.snoc vertices v
    | otherwise = Right $ StateFacet normal (V.snoc vertices v)

-- | Removes spaces in the beginning of string.
removeTrailingSpaces :: String -> String
removeTrailingSpaces (' ':xs) = removeTrailingSpaces xs
removeTrailingSpaces xs = xs
