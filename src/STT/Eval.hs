{-# LANGUAGE LambdaCase #-}

module STT.Eval
  ( Env
  , eval
  , steps
  ) where

import Control.Monad.Except
import Control.Monad.Reader (Reader, runReader, MonadReader(..))
import Data.Text (Text)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
import STT.Syntax
import STT.Subst (apply)
import STT.Typing.DNF (dnf, subset)

-- ----------------------------------------------------------------------------
-- Small Step Evaluation

-- | A list of bound variables.
type Env = Map Text [Expr]

-- | A unit value that's thrown when we can't reduce an expression any
-- further within the `Step` monad.
data Stuck = Stuck

-- | Monad for single-stepping expressions.
type Step = ExceptT Stuck (Reader Env)

stuck :: Step a
stuck = throwError Stuck

(<|>) :: Step a -> Step a -> Step a
(<|>) m1 m2 = catchError m1 (const m2)

step :: Expr -> Step Expr
step = \case
  EUnit           -> stuck
  EBool _         -> stuck
  EInt _          -> stuck
  EPair e1 e2     -> stepPair e1 e2
  EVar x i        -> stepVar x i
  EApp e1 e2      -> stepApp e1 e2
  EFn _ _         -> stuck
  EFix e          -> stepFix e
  ELet x e1 e2    -> stepLet x e1 e2
  ECase e cases   -> stepCase e cases
  EFst e          -> stepFst e
  ESnd e          -> stepSnd e
  EArith op       -> stepArith op
  ECmp op         -> stepCmp op
  EAnn e _        -> step e

stepVar :: Text -> Int -> Step Expr
stepVar x i = do
  env <- ask
  case Map.lookup x env of
    Just es -> return (es !! i)
    Nothing -> stuck

stepPair :: Expr -> Expr -> Step Expr
stepPair e1 e2 =
      do e1' <- step e1
         return (EPair e1' e2)
  <|> do e2' <- step e2
         return (EPair e1 e2')

stepApp :: Expr -> Expr -> Step Expr
stepApp e1 e2 =
      do e1' <- step e1
         return (EApp e1' e2)
  <|> do e2' <- step e2
         return (EApp e1 e2')
  <|> case e1 of
        EFn x e -> return (apply x e e2)
        _       -> stuck

stepLet :: Text -> Expr -> Expr -> Step Expr
stepLet x e1 e2 =
      do e1' <- step e1
         return (ELet x e1' e2)
  <|> return (apply x e2 e1)

stepFix :: Expr -> Step Expr
stepFix e =
      do e' <- step e
         return (EFix e')
  <|> case e of
        EFn x e' -> return $ apply x e' (EFix e)
        _        -> stuck

stepCase :: Expr -> [(Ty, Expr)] -> Step Expr
stepCase e cases =
      do e' <- step e
         return (ECase e' cases)
  <|> do t <- typeOf e
         case match t of
           Just e' -> return e'
           Nothing -> stuck
  where
    -- Find the first case where the type of the expression being
    -- matched (t) is a subset of the given case (t').
    match :: Ty -> Maybe Expr
    match t = lookup True (map (first $ \t' -> dnf t `subset` dnf t') cases)

    -- Calculate the type of a given expression at runtime. Only
    -- "values" have types at runtime. Note that functions always have
    -- the type `⊥ → ⊤` (i.e., the super type of all functions).
    typeOf :: Expr -> Step Ty
    typeOf = \case
      EUnit       -> return TUnit
      EBool x     -> return $ TBool (Just x)
      EInt x      -> return $ TInt (Just x)
      EFn _ _     -> return $ TFn TEmpty TAny
      EPair v1 v2 -> TPair <$> typeOf v1 <*> typeOf v2
      _           -> stuck

stepFst :: Expr -> Step Expr
stepFst e =
      do e' <- step e
         return (EFst e')
  <|> case e of
        EPair e1 _ -> return e1
        _          -> stuck

stepSnd :: Expr -> Step Expr
stepSnd e =
      do e' <- step e
         return (ESnd e')
  <|> case e of
        EPair _ e2 -> return e2
        _          -> stuck

stepArith :: ArithOp -> Step Expr
stepArith = \case
    OpAdd e1 e2 -> binary OpAdd (+) e1 e2
    OpSub e1 e2 -> binary OpSub (-) e1 e2
    OpMul e1 e2 -> binary OpMul (*) e1 e2
    OpDiv e1 e2 -> binary OpDiv div e1 e2
    OpMod e1 e2 -> binary OpAdd mod e1 e2
  where
    binary op f e1 e2 =
          do e1' <- step e1
             return (EArith (op e1' e2))
      <|> do e2' <- step e2
             return (EArith (op e1 e2'))
      <|> case (e1, e2) of
            (EInt x, EInt y) -> return $ EInt (x `f` y)
            _                -> stuck

stepCmp :: CmpOp -> Step Expr
stepCmp = \case
    OpLT e1 e2 -> binary OpLT (<) e1 e2
    OpLE e1 e2 -> binary OpLE (<=) e1 e2
    OpEQ e1 e2 -> binary OpEQ (==) e1 e2
    OpGE e1 e2 -> binary OpGE (>=) e1 e2
    OpGT e1 e2 -> binary OpGT (>) e1 e2
  where
    binary op f e1 e2 =
          do e1' <- step e1
             return (ECmp (op e1' e2))
      <|> do e2' <- step e2
             return (ECmp (op e1 e2'))
      <|> case (e1, e2) of
            (EInt x, EInt y) -> return $ EBool (x `f` y)
            _                -> stuck

steps :: Env -> Expr -> [Expr]
steps env e =
  case runReader (runExceptT (step e)) env of
    Right e'   -> e' : steps env e'
    Left Stuck -> []

eval :: Env -> Expr -> Expr
eval env e = last (e : steps env e)
