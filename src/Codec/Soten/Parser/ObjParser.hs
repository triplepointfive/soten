module Codec.Soten.Parser.ObjParser where
{-
 - Not supported:
 - Free-form geometry statement.
 -}

import Control.Monad (void)

import Text.Parsec

file, cube :: String
file = unlines
    [ "# Blender v2.57 (sub 0) OBJ File: ''"
    , "# www.blender.org"
    , "o Cube_Cube.001"
    , "v -1.000000 1.000000 0.000000  "
    , "s off"
    , "v 1.000000 1.000000 0.000000   "
    , "v -1.000000 -1.000000 0.000000 "
    , "v 1.000000 -1.000000 0.000000  "
    , "vt 1.000000 1.000000           "
    , "vt 0.000000 1.000000           "
    , "vt 0.000000 0.000000           "
    , "vt 1.000000 0.000000           "
    , "usemtl (null)"
    , "f 2/1 4/4 1/2"
    , "f 4/4 3/3 1/2"
    ]
cube = unlines
    [ "v 0.000000 2.000000 2.000000"
    , "v 0.000000 0.000000 2.000000"
    , "v 2.000000 0.000000 2.000000"
    , "v 2.000000 2.000000 2.000000"
    , "v 0.000000 2.000000 0.000000"
    , "v 0.000000 0.000000 0.000000"
    , "v 2.000000 0.000000 0.000000"
    , "v 2.000000 2.000000 0.000000"
    , "f 1 2 3 4"
    , "f 8 7 6 5"
    , "f 4 3 7 8"
    , "f 5 1 4 8"
    , "f 5 6 2 1"
    , "f 2 6 7 3"
    ]

-- | A synonym just to simplify signatures a bit.
type Parser = Parsec String ()

-- | All parseble tokens.
data Token
    -- | Specifies a geometric vertex and its x y z coordinates.
    = Vertex !Float !Float !Float
    -- | Specifies a texture vertex and its coordinates.
    | VertexTexture !Float !Float
    -- | Specifies a user-defined object name.
    | Object !String
    -- | Specifies a face element and its vertex reference number.
    | Face ![Int]
    deriving Show

-- | A parser for all implemented tokens.
model :: Parser [Token]
model = many $ choice $ map (try . lexeme) availableTags

-- | A list of supported tags parsers.
availableTags :: [Parser Token]
availableTags = [vertexTexture, vertex, object]

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

-- | An empty line.
emptyLine :: Parser ()
emptyLine = many space *> eol
