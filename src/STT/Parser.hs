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
import STT.Syntax
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec

-- ----------------------------------------------------------------------------
-- Lexer

keywords :: [String]
keywords =
  [ "Any", "Bool", "Empty", "Int", "case", "def", "false", "fix", "fst", "in"
  , "is", "let", "of", "sig", "snd"
  ]

operators :: [String]
operators =
  [ "<", "<=", "==", ">=", ">", "+", "-", "*", "/", "%", "~", "¬", "->", "→"
  , "&", "∧", "|", "∨", "\\", "$"
  ]

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser $ emptyDef
  { Token.commentLine = "--"
  , Token.identStart = letter <|> oneOf "_"
  , Token.identLetter = alphaNum <|> oneOf "_'"
  , Token.reservedNames = keywords
  , Token.reservedOpNames = operators
  }

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

identifier :: Parser Text
identifier = T.pack <$> Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

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

-- ----------------------------------------------------------------------------
-- Expression Parser

-- | Parse an integer expression.
eInt :: Parser Expr
eInt = EInt <$> int

-- | Parse a boolean expression.
eBool :: Parser Expr
eBool = EBool <$> bool

-- | Parse a variable.
eVar :: Parser Expr
eVar = do
  name  <- identifier
  index <- option 0 (symbol "@" *> int)
  return $ EVar name index

-- | Parse a sequence of applications.
eApp :: Parser Expr
eApp = do
    hd <- EFst <$> (reserved "fst" *> term)
      <|> ESnd <$> (reserved "snd" *> term)
      <|> term
    tl <- many term
    return $ foldl EApp hd tl
  where
    term :: Parser Expr
    term = eInt <|> eBool <|> eVar <|> paren

    -- Parse a parenthetical expression. A prenthetical expression is
    -- either the unit value '()', a parenthesized expresion '(x)',
    -- or a pair '(x, y)'.
    paren :: Parser Expr
    paren = parens $ option EUnit $ do
      e1 <- expr
      m2 <- optionMaybe (symbol "," *> expr)
      case m2 of
        Nothing -> return e1
        Just e2 -> return (EPair e1 e2)

-- | Parse an arithmetic or comparison operatation.
eArith :: Parser Expr
eArith = buildExpressionParser table eApp
  where
    binary op assoc e = Infix (e <$ reservedOp op) assoc
    table =
      [ [ binary "<"  AssocNone  $ \x y -> ECmp (OpLT x y)
        , binary "<=" AssocNone  $ \x y -> ECmp (OpLE x y)
        , binary "==" AssocNone  $ \x y -> ECmp (OpEQ x y)
        , binary ">=" AssocNone  $ \x y -> ECmp (OpGE x y)
        , binary ">"  AssocNone  $ \x y -> ECmp (OpGT x y) ]
      , [ binary "*"  AssocRight $ \x y -> EArith (OpMul x y)
        , binary "/"  AssocRight $ \x y -> EArith (OpDiv x y)
        , binary "%"  AssocRight $ \x y -> EArith (OpMod x y) ]
      , [ binary "+"  AssocRight $ \x y -> EArith (OpAdd x y)
        , binary "-"  AssocRight $ \x y -> EArith (OpSub x y) ]
      ]

-- | Parse an annotated expression.
eAnn :: Parser Expr
eAnn = do
  e <- eArith
  m <- optionMaybe (symbol ":" *> ty)
  case m of
    Nothing -> return e
    Just t  -> return (EAnn e t)

-- | Parse a lambda.
eFn :: Parser Expr
eFn = do
  symbol "λ" <|> symbol "\\"
  xs <- many1 identifier
  symbol ","
  e <- expr
  return $ foldr EFn e xs

-- | Parse a let binding.
eLet :: Parser Expr
eLet = do
  reserved "let"
  x <- identifier
  symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet x e1 e2

-- | Parse a case expression.
eCase :: Parser Expr
eCase = do
    reserved "case"
    e  <- expr
    reserved "of"
    ECase e <$> many1 branch
  where
    branch :: Parser (Ty, Expr)
    branch = do
      symbol "|"
      t <- ty
      symbol "=>" <|> symbol "⇒"
      e <- expr
      return (t, e)

-- | Parse a fixpoint.
eFix :: Parser Expr
eFix = do
  reserved "fix"
  e <- eFn <|> eApp
  return $ EFix e

-- | Parse an expression.
expr :: Parser Expr
expr = eFn
   <|> eLet
   <|> eCase
   <|> eFix
   <|> eAnn

-- ----------------------------------------------------------------------------
-- Type Parser

-- | Parse the 'Any' type.
tAny :: Parser Ty
tAny = TAny <$ reserved "Any"

-- | Parse the 'Empty' type.
tEmpty :: Parser Ty
tEmpty = TEmpty <$ reserved "Empty"

-- | Parse the 'Int' type.
tInt :: Parser Ty
tInt = TInt <$> val
  where val = Nothing <$ reserved "Int"
          <|> Just <$> int

-- | Parse the 'Bool' type.
tBool :: Parser Ty
tBool = TBool <$> val
  where val = Nothing <$ reserved "Bool"
          <|> Just <$> bool

-- | Parse a type.
ty :: Parser Ty
ty = buildExpressionParser table term
  where
    term :: Parser Ty
    term = tAny <|> tEmpty <|> tBool <|> tInt <|> paren

    -- Parse a parenthetical type. A prenthetical type is either the unit
    -- type '()', a parenthesized type '(a)', or a pair type '(a, b)'.
    paren :: Parser Ty
    paren = parens $ option TUnit $ do
      t1 <- ty
      m2 <- optionMaybe (symbol "," *> ty)
      case m2 of
        Nothing -> return t1
        Just t2 -> return (TPair t1 t2)

    opNot   = reservedOp "¬" <|> reservedOp "~"
    opArrow = reservedOp "→" <|> reservedOp "->"
    opConj  = reservedOp "∧" <|> reservedOp "&"
    opDisj  = reservedOp "∨" <|> reservedOp "|"
    opDiff  = reservedOp "\\"

    table =
      [ [Prefix (TNot  <$ opNot)]
      , [Infix  (TFn   <$ opArrow) AssocRight]
      , [Infix  (TAnd  <$ opConj)  AssocRight]
      , [Infix  (TOr   <$ opDisj)  AssocRight]
      , [Infix  (TDiff <$ opDiff)  AssocLeft]
      ]

-- ----------------------------------------------------------------------------
-- Declaration Parser

dSig :: Parser Decl
dSig = do
  reserved "sig"
  x <- identifier
  symbol ":"
  t <- ty
  return $ DSig x t

dDef :: Parser Decl
dDef = do
  reserved "def"
  x <- identifier
  symbol "="
  e <- expr
  return $ DDef x e

decl :: Parser Decl
decl = dSig <|> dDef

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
