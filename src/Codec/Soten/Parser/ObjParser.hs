module Codec.Soten.Parser.ObjParser where
{-
 - Not supported:
 - Free-form geometry statement.
 -}

import Control.Monad (replicateM, void)

import Text.Parsec

file = unlines
    [ "v -1.000000 1.000000 0.000000  "
    , "v 1.000000 1.000000 0.000000   "
    , "v -1.000000 -1.000000 0.000000 "
    , "v 1.000000 -1.000000 0.000000  "
    , "vt 1.000000 1.000000           "
    , "vt 0.000000 1.000000           "
    , "vt 0.000000 0.000000           "
    , "vt 1.000000 0.000000           "
    ]

-- | A synonym just to simplify signatures a bit.
type Parser = Parsec String ()

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof

-- | All parseble tokens.
data Token
    -- | Polygonal and free-form geometry statement.
    = Vertex Float Float Float
    -- | Vertex statement for both polygonal and free-form geometry.
    | VertexTexture Float Float
    deriving Show

-- | A parser for all implemented tokens.
model :: Parser [Token]
model = many $ choice (map try [
  between (string "vt") eol vertexTexture,
  between (char 'v') eol vertex
  ])

-- | Fast & dirty. TODO: should be more consistent.
float :: Parser Float
float = read <$> many1 (digit <|> oneOf "-.")

-- | Skips spaces and parse float val.
floatS :: Parser Float
floatS = whitespace *> float

-- | Skips spaces and tab characters. At least one matching symbol expected.
whitespace :: Parser ()
whitespace = skipMany1 (oneOf " \t")

-- | Skips all characters up to end of line.
eol :: Parser ()
eol = void $ manyTill anyChar newline

-- | Parses `v` tag.
vertex :: Parser Token
vertex = Vertex <$> floatS <*> floatS <*> floatS -- <*> option 1 floatS

-- | Parses `vt` tag.
vertexTexture :: Parser Token
vertexTexture = VertexTexture <$> floatS <*> floatS -- <*> option 0 floatS

-- | A comment, ignore the whole line.
comment :: Parser ()
comment = char '#' *> eol

-- | An empty line.
emptyLine :: Parser ()
emptyLine = many space *> eol
