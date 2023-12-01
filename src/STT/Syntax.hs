module STT.Syntax where

import Data.Text (Text)

-- | Expressions.
data Expr
  = EUnit
    -- ^ The unit value '()'.
  | EInt Int
    -- ^ A literal integer.
  | EBool Bool
    -- ^ A literal boolean.
  | EPair Expr Expr
    -- ^ A pair.
  | EVar Text
    -- ^ A variable.
  | EApp Expr Expr
    -- ^ An application.
  | EFn Text Expr
    -- ^ A lambda.
  | ELet Text Expr Expr
    -- ^ A let binding.
  | EIf Expr Ty Expr Expr
    -- ^ An if statement.
  | EFst Expr
    -- ^ First projection of a pair.
  | ESnd Expr
    -- ^ Second projection of a pair.
  | EAnn Expr Ty
    -- ^ A type annotation.
  deriving (Eq, Show)

-- | Types.
data Ty
  = TEmpty
    -- ^ The bottom type.
  | TAny
    -- ^ The top type.
  | TUnit
    -- ^ The unit type.
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
