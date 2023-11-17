-- ----------------------------------------------------------------------------
-- | = Binary Decision Diagrams
--
-- This module is a minimal implementation of /Binary Decision Diagrams/,
-- first described in [Graph-Based Algorithms for Boolean Function
-- Manipulation](https://www.cs.cmu.edu/~bryant/pubdir/ieeetc86.pdf), IEEE
-- Transactions on Computers, Vol. C - 35, No. 8, August, 1986, pp. 677 - 691.
--
-- Binary Decision Diagrams (BDDs) are a novel way of encoding boolean
-- functions. For more information see
-- <https://en.wikipedia.org/wiki/Binary_decision_diagram>.
--
-- === Importing
--
-- Like @Data.Set@ and @Data.Map@, this module is intended to be imported
-- qualified, e.g.
--
-- > import STT.Typing.BDD (BDD)
-- > import qualified STT.Typing.BDD as BDD
--
-- ----------------------------------------------------------------------------

module STT.Typing.BDD
  ( BDD(..)
  -- * Constructors
  , top
  , bottom
  , singleton
  -- * Operators
  , disjunction
  , conjunction
  , difference
  , negate
  -- * Miscellaneous
  , flatten
  , tree
  ) where

import Prelude hiding (negate)
import Data.Bifunctor (first, second)
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import STT.Typing.Classes.BooleanAlgebra (BooleanAlgebra)
import qualified STT.Typing.Classes.BooleanAlgebra as BooleanAlgebra

-- ----------------------------------------------------------------------------
-- Data Types

-- | A Binary Decision Diagram.
data BDD a
  = Top
  | Bot
  | Fork a (BDD a) (BDD a)
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------------------
-- Constructors

-- | todo: explain ...
top :: BDD a
top = Top

-- | A BDD with no elements.
bottom :: BDD a
bottom = Bot

-- | A BDD that contains a single element.
singleton :: a -> BDD a
singleton x = Fork x Top Bot

-- ----------------------------------------------------------------------------
-- Operators

fork :: Eq a => a -> BDD a -> BDD a -> BDD a
fork x l r
  | l == r = l
  | otherwise = Fork x l r

-- | The disjuction of two BDDs.
disjunction :: Ord a => BDD a -> BDD a -> BDD a
disjunction Top _ = Top
disjunction _ Top = Top
disjunction Bot a = a
disjunction a Bot = a
disjunction a@(Fork x l r) a'@(Fork x' l' r') =
  case compare x x' of
    LT -> fork x  (l `disjunction` a') (r `disjunction` a')
    EQ -> fork x  (l `disjunction` l') (r `disjunction` r')
    GT -> fork x' (a `disjunction` l') (a `disjunction` r')

-- | The conjuction of two BDDs.
conjunction :: Ord a => BDD a -> BDD a -> BDD a
conjunction Top a = a
conjunction a Top = a
conjunction Bot _ = Bot
conjunction _ Bot = Bot
conjunction a@(Fork x l r) a'@(Fork x' l' r') =
  case compare x x' of
    LT -> fork x  (l `conjunction` a') (r `conjunction` a')
    EQ -> fork x  (l `conjunction` l') (r `conjunction` r')
    GT -> fork x' (a `conjunction` l') (a `conjunction` r')

-- | The (asymmetric) difference of two BDDs.
difference :: Ord a => BDD a -> BDD a -> BDD a
difference a Bot = a
difference Bot _ = Bot
difference _ Top = Bot
difference Top a = negate a
difference a@(Fork x l r) a'@(Fork x' l' r') =
  case compare x x' of
    LT -> fork x  (l `difference` a') (r `difference` a')
    EQ -> fork x  (l `difference` l') (r `difference` r')
    GT -> fork x' (a `difference` l') (a `difference` r')

-- | The negation of a BDD.
negate :: Ord a => BDD a -> BDD a
negate Top = Bot
negate Bot = Top
negate (Fork x l r) = fork x (negate l) (negate r)

-- ----------------------------------------------------------------------------
-- Miscellaneous

-- | todo: explain ...
flatten :: BDD a -> [([a], [a])]
flatten Top = [([], [])]
flatten Bot = []
flatten (Fork x l r) = l' ++ r'
  where l' = map (first (x :)) (flatten l)
        r' = map (second (x :)) (flatten r)

-- | Convert a BDD to a 'Data.Tree'.
tree :: BDD a -> Tree (Either Bool a)
tree Top = Tree.Node (Left True) []
tree Bot = Tree.Node (Left False) []
tree (Fork x l r) = Tree.Node (Right x) [tree l, tree r]

-- ----------------------------------------------------------------------------
-- Boolean Algebra

-- | BDDs form a Boolean algebra.
instance Ord a => BooleanAlgebra (BDD a) where
  top = top
  bottom = bottom
  disjunction = disjunction
  conjunction = conjunction
  negate = negate
  difference = difference
