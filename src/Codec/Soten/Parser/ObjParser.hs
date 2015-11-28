{-
 - Not supported:
 - Free-form geometry statement.
 -}
module Codec.Soten.Parser.ObjParser (
      getModel
    , tokenize
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
getModel = modelize . validate . tokenize

-- | Reads filecontent and splits it in tokens.
tokenize :: String -> Either ParseError [Token]
tokenize = parse modelTokens ""

-- | Transforms a set of tokens into intermediate model object.
modelize :: [Token] -> Model
modelize = foldl modifyModel newModel

-- | Modifies a model with a single given token.
modifyModel :: Model -> Token -> Model
modifyModel model (Vertex x y z) = addVertex model (x, y, z)
modifyModel model (VertexTexture u v) = addTextureCoord model (u, v)
modifyModel model (Normal x y z) = addNormal model (x, y, z)
modifyModel model (Face v t n) = addFace model (v, t, n)
modifyModel model (Object name) = model { objName = name }

-- | Returns model or fails with well-known 'DeadlyImporterError'.
validate :: Either ParseError [Token] -> [Token]
validate (Right m) = m
validate (Left e) = throw $ DeadlyImporterError $
    "Failed to parse OBJ model: " ++ show e

-- | A parser for all implemented tokens.
modelTokens :: Parser [Token]
modelTokens = many $ choice $ map try availableTags

-- | A list of supported tags parsers.
availableTags :: [Parser Token]
availableTags = special ++ map lexeme lexemed
  where
    lexemed = [normal, vertexTexture, vertex, face]
    special = [lexemePrefix *> object]

-- | Parses `v` tag.
vertex :: Parser Token
vertex = char 'v' >> Vertex <$> floatS <*> floatS <*> floatS -- <*> option 1 floatS

-- | Parses `vn` tag.
normal :: Parser Token
normal = string "vn" >> Normal <$> floatS <*> floatS <*> floatS -- <*> option 1 floatS

-- | Parses `vt` tag.
vertexTexture :: Parser Token
vertexTexture = string "vt" >> VertexTexture <$> floatS <*> floatS -- <*> option 0 floatS

-- | Parses `o` tag.
object :: Parser Token
object = char 'o' *> whitespace *> (Object <$> manyTill anyChar endOfLine)

-- | Parses `f` tag.
face :: Parser Token
face = char 'f' *> choice
    [try vertsAndNormals, try vertsAndTextures, try verts]
  where
    verts, vertsAndTextures :: Parser Token
    verts = Face <$> many1 intS <*> return [] <*> return []
    vertsAndTextures = do
      vsATX <- many1 numWithSeparator
      return (Face (map fst vsATX) (map snd vsATX) [])
    vertsAndNormals = do
      vsANs <- many1 numWithDSeparator
      return (Face (map fst vsANs) [] (map snd vsANs))

-- | Parses an expression matching pattern d//d.
numWithDSeparator :: Parser (Int, Int)
numWithDSeparator = whitespace *> do
    a <- num
    void $ string "//"
    b <- num
    return (a, b)

-- | Parses an expression matching pattern d/d.
numWithSeparator :: Parser (Int, Int)
numWithSeparator = whitespace *> do
    a <- num
    void $ char '/'
    b <- num
    return (a, b)

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
eol = void $ manyTill anyChar endOfLine

-- | A parser of unimplemented / unsupported tags.
ignoreTags :: Parser ()
ignoreTags = choice $ map (*> (whitespace *> eol)) unsupported

-- | A list of unsupported tags parsers.
unsupported :: [Parser ()]
unsupported = [void (string "usemtl"), void (string "mtllib"), void (oneOf "s")]

-- | A helper to ignore comments.
lexeme :: Parser a -> Parser a
lexeme p = lexemePrefix *> p <* eol

-- | Ignores shit before the actual tag.
lexemePrefix :: Parser [()] -- TODO: Fix signature.
lexemePrefix = many (void endOfLine <|> comment <|> ignoreTags)

-- | A comment, ignore the whole line.
comment :: Parser ()
comment = char '#' *> eol
