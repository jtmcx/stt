{-# LANGUAGE LambdaCase #-}

module STT.Subst
  ( shift
  , subst
  , apply
  ) where

import Data.Text (Text)
import Data.Bifunctor (second)
import STT.Syntax ( CmpOp(..), ArithOp(..), Expr(..) )

-- | Shift deBruijn indices in a given expression.
shift :: Text   -- ^ The variable name to shift.
      -> Int    -- ^ The cutoff index.
      -> Int    -- ^ The ammount to shift by.
      -> Expr   -- ^ The expression to shift.
      -> Expr
shift x k n = go
  where
    go = \case
      EUnit         -> EUnit
      EBool v       -> EBool v
      EInt v        -> EInt v
      EPair e1 e2   -> EPair (go e1) (go e2)
      EApp e1 e2    -> EApp (go e1) (go e2)
      EFix e        -> EFix (go e)
      ECase e cases -> ECase (go e) (map (second go) cases)
      EFst e        -> EFst (go e)
      ESnd e        -> ESnd (go e)
      EArith op     -> EArith (goArith op)
      ECmp op       -> ECmp (goCmp op)
      EAnn e ty     -> EAnn (go e) ty
      EVar y i      -> goVar y i
      EFn y e       -> goFn y e
      ELet y e1 e2  -> goLet y e1 e2

    goVar :: Text -> Int -> Expr
    goVar y i = EVar y i'
      where i' | x == y && i >= k = i + n
               | otherwise        = i

    goFn :: Text -> Expr -> Expr
    goFn y e = EFn y e'
      where e' | x == y    = shift x (k + 1) n e
               | otherwise = go e

    goLet :: Text -> Expr -> Expr -> Expr
    goLet y e1 e2 = ELet y (go e1) e2'
      where e2' | x == y    = shift x (k + 1) n e2
                | otherwise = go e2

    goArith :: ArithOp -> ArithOp
    goArith = \case
      OpAdd e1 e2 -> OpAdd (go e1) (go e2)
      OpSub e1 e2 -> OpSub (go e1) (go e2)
      OpMul e1 e2 -> OpMul (go e1) (go e2)
      OpDiv e1 e2 -> OpDiv (go e1) (go e2)
      OpMod e1 e2 -> OpMod (go e1) (go e2)

    goCmp :: CmpOp -> CmpOp
    goCmp = \case
      OpLT e1 e2 -> OpLT (go e1) (go e2)
      OpLE e1 e2 -> OpLE (go e1) (go e2)
      OpEQ e1 e2 -> OpEQ (go e1) (go e2)
      OpGE e1 e2 -> OpGE (go e1) (go e2)
      OpGT e1 e2 -> OpGT (go e1) (go e2)


subst :: Text -> Int -> Expr -> Expr -> Expr
subst x i s = go
  where
    go = \case
      EUnit         -> EUnit
      EBool v       -> EBool v
      EInt v        -> EInt v
      EPair e1 e2   -> EPair (go e1) (go e2)
      EApp e1 e2    -> EApp (go e1) (go e2)
      EFst e        -> EFst (go e)
      ESnd e        -> ESnd (go e)
      EFix e        -> EFix (go e)
      EAnn e ty     -> EAnn (go e) ty
      ECase e cases -> ECase (go e) (map (second go) cases)
      EArith op     -> EArith (goArith op)
      ECmp op       -> ECmp (goCmp op)
      EVar y j      -> goVar y j
      EFn y e       -> goFn y e
      ELet y e1 e2  -> goLet y e1 e2

    goVar :: Text -> Int -> Expr
    goVar y j =
      if x == y && i == j
        then s
        else EVar y j

    goFn :: Text -> Expr -> Expr
    goFn y e = EFn y e'
      where e' | x == y    = subst x (i + 1) (shift x 0 1 s) e
               | otherwise = go e

    goLet :: Text -> Expr -> Expr -> Expr
    goLet y e1 e2 = ELet y (go e1) e2'
      where e2' | x == y    = subst x (i + 1) (shift x 0 1 s) e2
                | otherwise = go e2

    goArith :: ArithOp -> ArithOp
    goArith = \case
      OpAdd e1 e2 -> OpAdd (go e1) (go e2)
      OpSub e1 e2 -> OpSub (go e1) (go e2)
      OpMul e1 e2 -> OpMul (go e1) (go e2)
      OpDiv e1 e2 -> OpDiv (go e1) (go e2)
      OpMod e1 e2 -> OpMod (go e1) (go e2)

    goCmp :: CmpOp -> CmpOp
    goCmp = \case
      OpLT e1 e2 -> OpLT (go e1) (go e2)
      OpLE e1 e2 -> OpLE (go e1) (go e2)
      OpEQ e1 e2 -> OpEQ (go e1) (go e2)
      OpGE e1 e2 -> OpGE (go e1) (go e2)
      OpGT e1 e2 -> OpGT (go e1) (go e2)


apply :: Text -> Expr -> Expr -> Expr
apply x e1 e2 = shift x 0 (-1) (subst x 0 (shift x 0 1 e2) e1)
