module STT.Parser
  ( parseExpr
  , parseTy
  , parseDecl
  , parseToplevel
  , parseFile
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import STT.Syntax (Decl(..), Expr(..), Ty(..))
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

-- ----------------------------------------------------------------------------
-- Lexer

keywords :: [String]
keywords =
  [ "false"
  , "true"
  , "def"
  , "let"
  , "in"
  , "Int"
  , "Bool"
  , "Any"
  , "Empty"
  ]

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser $ emptyDef
  { Token.commentLine = "#"
  , Token.identStart = letter <|> oneOf "_"
  , Token.identLetter = alphaNum <|> oneOf "_'"
  , Token.reservedNames = keywords
  }

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

symbol :: String -> Parser ()
symbol = void <$> Token.symbol lexer

-- ----------------------------------------------------------------------------
-- Common Parsers

int :: Parser Int
int = lexeme (read <$> many1 digit) <?> "int"

bool :: Parser Bool
bool = True  <$ reserved "true"
   <|> False <$ reserved "false"
   <?> "bool"

var :: Parser Text
var = T.pack <$> identifier <?> "variable"

-- ----------------------------------------------------------------------------
-- Expression Parser

-- | Parse a sequence of applications.
eapp :: Parser Expr
eapp = foldl1 EApp <$> many1 term
  where
    term :: Parser Expr
    term = EInt  <$> int
       <|> EBool <$> bool
       <|> EVar  <$> var
       <|> epair

-- | Parse a parenthesized expression. If it contains commas, parse the
-- expression as a sequence of pairs.
epair :: Parser Expr
epair = do
  symbol "("
  es <- sepBy1 expr (symbol ",")
  symbol ")"
  return $ foldr1 EPair es

-- | Parse a lambda.
efn :: Parser Expr
efn = do
  symbol "λ" <|> symbol "\\"
  x <- T.pack <$> identifier
  symbol "->"
  e <- expr
  return $ EFn x e

-- | Parse a let binding.
elet :: Parser Expr
elet = do
  reserved "let"
  x <- T.pack <$> identifier
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet x e1 e2

-- | Parse an expression.
expr :: Parser Expr
expr = efn <|> elet <|> eapp

-- ----------------------------------------------------------------------------
-- Type Parser

-- | Parse the 'Any' type.
tany :: Parser Ty
tany = TAny <$ reserved "Any"

-- | Parse the 'Empty' type.
tempty :: Parser Ty
tempty = TEmpty <$ reserved "Empty"

-- | Parse the 'Int' type.
tint :: Parser Ty
tint = TInt <$> val
  where val = Nothing <$ reserved "Int"
          <|> Just <$> int

-- | Parse the 'Bool' type.
tbool :: Parser Ty
tbool = TBool <$> val
  where val = Nothing <$ reserved "Bool"
          <|> Just <$> bool

-- | Parse a parenthesized type. If it contains commas, parse the
-- expression as a sequence of pair types.
tpair :: Parser Ty
tpair = do
  symbol "("
  es <- sepBy1 ty (symbol ",")
  symbol ")"
  return $ foldr1 TPair es

-- | Parse a type.
ty :: Parser Ty
ty = buildExpressionParser table term
  where
    term :: Parser Ty
    term = tany <|> tempty <|> tbool <|> tint <|> tpair

    table =
      [ [Prefix (TNot  <$ symbol "~")]
      , [Infix  (TFn   <$ symbol "->") AssocRight]
      , [Infix  (TAnd  <$ symbol "&")  AssocRight]
      , [Infix  (TOr   <$ symbol "|")  AssocRight]
      , [Infix  (TDiff <$ symbol "\\") AssocRight]
      ]

-- ----------------------------------------------------------------------------
-- Declaration Parser

decl :: Parser Decl
decl = do
  reserved "def"
  x <- T.pack <$> identifier
  symbol "="
  e <- expr
  return $ DDef x e

-- ----------------------------------------------------------------------------
-- Exported Parsers

-- | Parse an expression.
parseExpr :: String -> String -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr <* eof)

-- | Parse a type.
parseTy :: String -> String -> Either ParseError Ty
parseTy = parse (whiteSpace >> ty <* eof)

-- | Parse a declaration.
parseDecl :: String -> String -> Either ParseError Decl
parseDecl = parse (whiteSpace >> decl <* eof)

-- | Parse a top-level statement.
parseToplevel :: String -> String -> Either ParseError (Either Decl Expr)
parseToplevel = parse (whiteSpace >> p <* eof)
  where
    p :: Parser (Either Decl Expr)
    p = Left <$> decl <|> Right <$> expr

-- | Parse a sequence of declarations.
parseFile :: String -> String -> Either ParseError [Decl]
parseFile = parse (whiteSpace >> many decl <* eof)
