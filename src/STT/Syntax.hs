module STT.Syntax where

import Data.Text (Text)

-- | Expressions.
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

-- | Types.
data Ty
  = TEmpty
    -- ^ The bottom type.
  | TAny
    -- ^ The top type.
  | TInt (Maybe Int)
    -- ^ Either the 'Int' type, or a singleton integer.
  | TBool (Maybe Bool)
    -- ^ Either the 'Bool' type, or a singleton boolean.
  | TPair Ty Ty
    -- ^ The type of pairs.
  | TFn Ty Ty
    -- ^ The type of functions.
  | TAnd Ty Ty
    -- ^ Intersection of two types.
  | TOr Ty Ty
    -- ^ Union of two types.
  | TDiff Ty Ty
    -- ^ Difference of two types.
  | TNot Ty
    -- ^ Negation of a type.
  deriving (Eq, Show)

-- | Top-level declarations.
data Decl
  = DDef Text Expr
    -- ^ A top-level definition.
