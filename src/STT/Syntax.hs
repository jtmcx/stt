module STT.Syntax where

import Data.Text (Text)

data Expr
  = EInt Int
    -- ^ A literal integer.
  | EBool Bool
    -- ^ A literal boolean.
  | EVar Text
    -- ^ A variable.
  | EPair Expr Expr
    -- ^ A pair.
  | EApp Expr Expr
    -- ^ An application.
  | EFn Text Expr
    -- ^ A lambda.
  | ELet Text Expr Expr
    -- ^ A let binding.
  deriving (Eq, Show)

-- | Top level declarations
data Decl
  = DDef Text Expr
    -- ^ A top-level definition
