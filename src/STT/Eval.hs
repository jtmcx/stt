module STT.Eval
  ( Normal(..)
  , Neutral(..)
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
import STT.Syntax (Expr (..))

-- ----------------------------------------------------------------------------
-- Normal Form

-- | An expression in normal form.
data Normal
  = NNeutral Neutral
    -- ^ A neutral value.
  | NClosure Closure
    -- ^ A closure.
  deriving (Eq, Show)

-- | A neutral value.
data Neutral
  = NBool Bool
    -- ^ A boolean.
  | NInt Int
    -- ^ An integer.
  | NVar Text
    -- ^ A variable.
  | NApp Neutral Normal
    -- ^ An application.
  | NPair Normal Normal
    -- ^ A pair.
  deriving (Eq, Show)

-- | A lambda with a captured environment.
data Closure = Closure Text Expr Env
  deriving (Eq, Show)

-- | A list of bound variables.
type Env = Map Text Normal

-- ----------------------------------------------------------------------------
-- Evaluation

type Eval = Reader Env

-- | Evaluate an expression into normal form.
eval :: Env -> Expr -> Normal
eval env e = runReader (evalExpr e) env

-- | Evaluate an expression into normal form.
evalExpr :: Expr -> Eval Normal
evalExpr e = case e of
  EBool x     -> return $ NNeutral (NBool x)
  EInt x      -> return $ NNeutral (NInt x)
  EVar x      -> evalVar x
  EPair e1 e2 -> evalPair e1 e2
  EApp e1 e2  -> evalApp e1 e2
  EFn x e'    -> evalFn x e'

-- | Lookup a variable in the environment.
evalVar :: Text -> Eval Normal
evalVar x = do
  env <- ask
  case Map.lookup x env of
    Just v  -> return v
    Nothing -> return $ NNeutral (NVar x)

-- | Evaluate the elements of a pair.
evalPair :: Expr -> Expr -> Eval Normal
evalPair e1 e2 = do
  n1 <- evalExpr e1
  n2 <- evalExpr e2
  return $ NNeutral (NPair n1 n2)

-- | Evaluate an application.
evalApp :: Expr -> Expr -> Eval Normal
evalApp e1 e2 = do
  n1 <- evalExpr e1
  n2 <- evalExpr e2
  case n1 of
    NClosure closure -> instantiate closure n2
    NNeutral n1' -> return $ NNeutral (NApp n1' n2)
  where
    instantiate :: Closure -> Normal -> Eval Normal
    instantiate (Closure x e env) n =
      local (const $ Map.insert x n env) (evalExpr e)

-- | Convert a lambda into a closure with the current 'Env'.
evalFn :: Text -> Expr -> Eval Normal
evalFn x e = do
  env <- ask
  return $ NClosure (Closure x e env)

-- ----------------------------------------------------------------------------
-- Reification

type Reify = Reader [Text]

-- | Readback a 'Normal' term as an 'Expr'.
reify :: [Text] -> Normal -> Expr
reify names n = runReader (reifyNormal n) names

-- | Readback a 'Normal' term as an 'Expr'.
reifyNormal :: Normal -> Reify Expr
reifyNormal (NNeutral n) = reifyNeutral n
reifyNormal (NClosure (Closure x e env)) = do
  x' <- freshen x
  let n = eval (Map.insert x (NNeutral (NVar x')) env) e
  EFn x' <$> local (x' :) (reifyNormal n)

-- | Readback a 'Neutral' term as an 'Expr'.
reifyNeutral :: Neutral -> Reify Expr
reifyNeutral n = case n of
  NBool x   -> return $ EBool x
  NInt x    -> return $ EInt x
  NVar x    -> return $ EVar x
  NPair x y -> EPair <$> reifyNormal x <*> reifyNormal y
  NApp x y  -> EApp <$> reifyNeutral x <*> reifyNormal y

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
