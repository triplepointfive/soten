{-
 - Not supported:
 - Free-form geometry statement.
 -}
module Codec.Soten.Parser.ObjParser (
      getModel
) where
import           Control.Monad (void)

import           Text.Parsec

import           Codec.Soten.Data.ObjData
import           Codec.Soten.Util
                 ( DeadlyImporterError(..)
                 , throw
                 )

-- | A synonym just to simplify signatures a bit.
type Parser = Parsec String ()

-- | Parses obj file content into a model.
getModel :: String -> Model
getModel = validate . parse model ""

-- | Returns model or fails with well-known 'DeadlyImporterError'.
validate :: Either ParseError Model -> Model
validate (Right m) = m
validate (Left e) = throw $ DeadlyImporterError $
    "Failed to parse OBJ model: " ++ show e

-- | A parser for all implemented tokens.
model :: Parser [Token]
model = many $ choice $ map (try . lexeme) availableTags

-- | A list of supported tags parsers.  availableTags :: [Parser Token]
availableTags :: [Parser Token]
availableTags = [vertexTexture, vertex, object, face]

-- | Parses `v` tag.
vertex :: Parser Token
vertex = char 'v' >> Vertex <$> floatS <*> floatS <*> floatS -- <*> option 1 floatS

-- | Parses `vt` tag.
vertexTexture :: Parser Token
vertexTexture = string "vt" >> VertexTexture <$> floatS <*> floatS -- <*> option 0 floatS

-- | Parses `o` tag.
object :: Parser Token
object = char 'o' *> whitespace *> (Object <$> manyTill anyChar newline)

-- | Parses `f` tag.
face :: Parser Token
face = char 'f' *> (Face <$> many1 intS)

-- | Integer number parser.
num :: Parser Int
num = read <$> many1 digit

-- | Fast & dirty. TODO: should be more consistent.
float :: Parser Float
float = read <$> many1 (digit <|> oneOf "-.")

-- | Skips spaces and parse float val.
floatS :: Parser Float
floatS = whitespace *> float

-- | Skips spaces and parse int val.
intS :: Parser Int
intS = whitespace *> num

-- | Skips spaces and tab characters. At least one matching symbol expected.
whitespace :: Parser ()
whitespace = skipMany1 (oneOf " \t")

-- | Skips all characters up to end of line.
eol :: Parser ()
eol = void $ manyTill anyChar newline

-- | A parser of unimplemented / unsupported tags.
ignoreTags :: Parser ()
ignoreTags = choice $ map (*> (whitespace *> eol)) unsupported

-- | A list of unsupported tags parsers.
unsupported :: [Parser ()]
unsupported = [void (string "usemtl"), void (oneOf "sf")]

-- | A helper to ignore comments.
lexeme :: Parser a -> Parser a
lexeme p = many (comment <|> ignoreTags) *> p <* eol

-- | A comment, ignore the whole line.
comment :: Parser ()
comment = char '#' *> eol
