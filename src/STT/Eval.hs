{-# LANGUAGE LambdaCase #-}

module STT.Eval
  ( Value(..)
  , Closure(..)
  , Env
  , eval
  , reify
  , normalize
  ) where

import Control.Monad.Reader (Reader, runReader, MonadReader(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import STT.Syntax (Expr (..), Ty(..))

-- ----------------------------------------------------------------------------
-- Normal Form

-- | A value.
data Value
  = VUnit
    -- ^ The unit value.
  | VBool Bool
    -- ^ A boolean.
  | VInt Int
    -- ^ An integer.
  | VPair Value Value
    -- ^ A pair.
  | VAbs Closure
    -- ^ A closure.
  | VVar Text
    -- ^ A variable.
  | VApp Value Value
    -- ^ An application.
  deriving (Eq, Show)

-- | A lambda with a captured environment.
data Closure = Closure Text Expr Env
  deriving (Eq, Show)

-- | A list of bound variables.
type Env = Map Text Value

-- ----------------------------------------------------------------------------
-- Evaluation

type Eval = Reader Env

-- | Evaluate an expression into normal form.
eval :: Env -> Expr -> Value
eval env e = runReader (evalExpr e) env

-- | Evaluate an expression into normal form.
evalExpr :: Expr -> Eval Value
evalExpr = \case
  EUnit          -> return VUnit
  EBool x        -> return (VBool x)
  EInt x         -> return (VInt x)
  EVar x         -> evalVar x
  EPair e1 e2    -> evalPair e1 e2
  EApp e1 e2     -> evalApp e1 e2
  EFn x e        -> evalFn x e
  ELet x e1 e2   -> evalLet x e1 e2
  EIf e1 t e2 e3 -> evalIf e1 t e2 e3

-- | Lookup a variable in the environment.
evalVar :: Text -> Eval Value
evalVar x = do
  env <- ask
  case Map.lookup x env of
    Just v  -> return v
    Nothing -> return (VVar x)

-- | Evaluate the elements of a pair.
evalPair :: Expr -> Expr -> Eval Value
evalPair e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return (VPair v1 v2)

-- | Evaluate an application.
evalApp :: Expr -> Expr -> Eval Value
evalApp e1 e2 = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case v1 of
    VAbs closure -> instantiate closure v2
    _            -> return (VApp v1 v2)
  where
    instantiate :: Closure -> Value -> Eval Value
    instantiate (Closure x e env) v =
      local (const $ Map.insert x v env) (evalExpr e)

-- | Convert a lambda into a closure with the current 'Env'.
evalFn :: Text -> Expr -> Eval Value
evalFn x e = do
  env <- ask
  return $ VAbs (Closure x e env)

-- | Evaluate a let binding.
evalLet :: Text -> Expr -> Expr -> Eval Value
evalLet x e1 e2 = do
  v1 <- evalExpr e1
  local (Map.insert x v1) $ evalExpr e2

evalIf :: Expr -> Ty -> Expr -> Expr -> Eval Value
evalIf = undefined

-- ----------------------------------------------------------------------------
-- Reification

type Reify = Reader [Text]

-- | Readback a 'Value' as an 'Expr'.
reify :: [Text] -> Value -> Expr
reify names v = runReader (reifyValue v) names

-- | Readback a 'Value' as an 'Expr'.
reifyValue :: Value -> Reify Expr
reifyValue = \case
  VUnit        -> return EUnit
  VBool x      -> return (EBool x)
  VInt x       -> return (EInt x)
  VVar x       -> return (EVar x)
  VPair v1 v2  -> EPair <$> reifyValue v1 <*> reifyValue v2
  VApp v1 v2   -> EApp <$> reifyValue v1 <*> reifyValue v2
  VAbs closure -> reifyClosure closure

-- | Readback a 'Closure' as an 'Expr'.
reifyClosure :: Closure -> Reify Expr
reifyClosure (Closure x e env) = do
  x' <- freshen x
  let v = eval (Map.insert x (VVar x') env) e
  EFn x' <$> local (x' :) (reifyValue v)

-- | Generate a fresh variable name.
freshen :: Text -> Reify Text
freshen x = do
  names <- ask
  if x `elem` names
    then freshen (T.snoc x '\'')
    else return x

-- ----------------------------------------------------------------------------
-- Normalization

-- | Evaluate and reify a given expression.
normalize :: Env -> Expr -> Expr
normalize env e = reify (Map.keys env) (eval env e)
