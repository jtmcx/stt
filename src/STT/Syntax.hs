module STT.Syntax where

import Data.Text (Text)

data Expr
  = EInt Int
    -- ^ A literal integer.
  | EBool Bool
    -- ^ A literal boolean.
  | EVar Text
    -- ^ A variable.
  | EApp Expr Expr
    -- ^ An application.
  | EFn Text Expr
    -- ^ A lambda.
  | EPair Expr Expr
    -- ^ A pair.
  deriving (Eq, Show)
