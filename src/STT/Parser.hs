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
  , "if"
  , "is"
  , "then"
  , "else"
  , "fst"
  , "snd"
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

parens :: Parser a -> Parser a
parens = Token.parens lexer

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

-- | Parse an expression suitible for the left-hand side of an application.
eterm :: Parser Expr
eterm = EInt  <$> int
    <|> EBool <$> bool
    <|> EVar  <$> var
    <|> eparen

efst :: Parser Expr
efst = do
  reserved "fst"
  e  <- eterm
  es <- many eterm
  return $ foldl EApp (EFst e) es

esnd :: Parser Expr
esnd = do
  reserved "snd"
  e  <- eterm
  es <- many eterm
  return $ foldl EApp (ESnd e) es

-- | Parse a sequence of applications.
eapp :: Parser Expr
eapp = foldl1 EApp <$> many1 eterm

-- | Parse an expression with an optional annotation.
eann :: Parser Expr
eann = do
  e <- efst <|> esnd <|> eapp
  m <- optionMaybe (symbol ":" *> ty)
  case m of
    Nothing -> return e
    Just t  -> return (EAnn e t)

-- | Parse a parenthetical expression. A prenthetical expression is either
-- the unit value '()', a parenthesized expresion '(x)', or a pair '(x, y)'.
eparen :: Parser Expr
eparen = parens $ option EUnit $ do
  e1 <- expr
  m2 <- optionMaybe (symbol "," *> expr)
  case m2 of
     Nothing -> return e1
     Just e2 -> return (EPair e1 e2)

-- | Parse a lambda.
efn :: Parser Expr
efn = do
  symbol "λ" <|> symbol "\\"
  xs <- many1 var
  symbol ","
  e <- expr
  return $ foldr EFn e xs

-- | Parse a let binding.
elet :: Parser Expr
elet = do
  reserved "let"
  x <- var
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet x e1 e2

-- | Parse an if statement.
eif :: Parser Expr
eif = do
  reserved "if"
  e1 <- expr
  reserved "is"
  t <- ty
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return $ EIf e1 t e2 e3

-- | Parse an expression.
expr :: Parser Expr
expr = efn <|> elet <|> eif <|> eann

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

-- | Parse a parenthetical type. A prenthetical type is either the unit type
-- '()', a parenthesized type '(a)', or a pair type '(a, b)'.
tparen :: Parser Ty
tparen = parens $ option TUnit $ do
  t1 <- ty
  m2 <- optionMaybe (symbol "," *> ty)
  case m2 of
     Nothing -> return t1
     Just t2 -> return (TPair t1 t2)

-- | Parse a type.
ty :: Parser Ty
ty = buildExpressionParser table term
  where
    term :: Parser Ty
    term = tany <|> tempty <|> tbool <|> tint <|> tparen

    table =
      [ [Prefix (TNot  <$ symbol "~")]
      , [Infix  (TFn   <$ symbol "->") AssocRight]
      , [Infix  (TAnd  <$ symbol "&")  AssocRight]
      , [Infix  (TOr   <$ symbol "|")  AssocRight]
      , [Infix  (TDiff <$ symbol "\\") AssocLeft]
      ]

-- ----------------------------------------------------------------------------
-- Declaration Parser

decl :: Parser Decl
decl = do
  reserved "def"
  x <- var
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
