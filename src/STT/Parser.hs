module STT.Parser
  ( parseExpr
  , parseDecl
  , parseToplevel
  , parseFile
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import STT.Syntax (Decl (..), Expr (..))
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
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
-- Expression Parser

-- | Parse an integer expression.
eint :: Parser Expr
eint = EInt <$> lexeme (read <$> many1 digit) <?> "int"

-- | Parse a boolean expression.
ebool :: Parser Expr
ebool = EBool <$> val <?> "bool"
  where
    val :: Parser Bool
    val = True  <$ reserved "true"
      <|> False <$ reserved "false"

-- | Parse a variable.
evar :: Parser Expr
evar = EVar . T.pack <$> identifier <?> "variable"

-- | Parse a sequence of applications.
eapp :: Parser Expr
eapp = foldl1 EApp <$> many1 term
  where
    term :: Parser Expr
    term = eint <|> ebool <|> evar <|> epair

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
parseExpr :: String -- ^ Source name
          -> String -- ^ Input text
          -> Either ParseError Expr
parseExpr = parse (whiteSpace >> expr <* eof)

-- | Parse a declaration.
parseDecl :: String -- ^ Source name
          -> String -- ^ Input text
          -> Either ParseError Decl
parseDecl = parse (whiteSpace >> decl <* eof)

-- | Parse a top-level statement.
parseToplevel :: String -- ^ Source name
              -> String -- ^ Input text
              -> Either ParseError (Either Decl Expr)
parseToplevel = parse (whiteSpace >> p <* eof)
  where
    p :: Parser (Either Decl Expr)
    p = Left <$> decl <|> Right <$> expr

-- | Parse a sequence of declarations.
parseFile :: String -- ^ Source name
          -> String -- ^ Input text
          -> Either ParseError [Decl]
parseFile = parse (whiteSpace >> many decl <* eof)
