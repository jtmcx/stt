module STT.Syntax where

import Data.Text (Text)

-- | Expressions.
data Expr
  = EUnit
    -- ^ The unit value '()'.
  | EBool Bool
    -- ^ A literal boolean.
  | EInt Int
    -- ^ A literal integer.
  | EPair Expr Expr
    -- ^ A pair.
  | EVar Text Int
    -- ^ A variable.
  | EApp Expr Expr
    -- ^ An application.
  | EFn Text Expr
    -- ^ A lambda.
  | EFix Expr
    -- ^ A fixpoint.
  | ELet Text Expr Expr
    -- ^ A let binding.
  | ECase Expr [(Ty, Expr)]
    -- ^ A case statement.
  | EAnn Expr Ty
    -- ^ A type annotation.
  | EFst Expr
    -- ^ First projection of a pair.
  | ESnd Expr
    -- ^ Second projection of a pair.
  | EArith ArithOp
    -- ^ Arithmetic operations.
  | ECmp CmpOp
    -- ^ Comparison operations.
  deriving (Eq, Show)

-- | Arithmetic operators.
data ArithOp
  = OpAdd Expr Expr
    -- ^ @e1 + e2@
  | OpSub Expr Expr
    -- ^ @e1 - e2@
  | OpMul Expr Expr
    -- ^ @e1 * e2@
  | OpDiv Expr Expr
    -- ^ @e1 / e2@
  | OpMod Expr Expr
    -- ^ @e1 % e2@
  deriving (Eq, Show)

-- | Comparison operators.
data CmpOp
  = OpLT Expr Expr
    -- ^ @e1 < e2@
  | OpLE Expr Expr
    -- ^ @e1 <= e2@
  | OpEQ Expr Expr
    -- ^ @e1 == e2@
  | OpGE Expr Expr
    -- ^ @e1 >= e2@
  | OpGT Expr Expr
    -- ^ @e1 > e2@
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
  = DSig Text Ty
    -- ^ A top-level signature.
  | DDef Text Expr
    -- ^ A top-level definition.
