-- | todo: explain ...

module STT.Typing.DNFInt
  ( Polarity(..)
  , DNFInt
  -- * Constructors
  , top
  , bottom
  , singleton
  , fromList
  -- * Queries
  , isTop
  , isBottom
  , member
  , elements
  , toList
  -- * Operators
  , disjunction
  , conjunction
  , difference
  , negate
  ) where

import Prelude hiding (negate)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import STT.Typing.Classes.BooleanAlgebra (BooleanAlgebra)
import qualified STT.Typing.Classes.BooleanAlgebra as BooleanAlgebra

-- ----------------------------------------------------------------------------
-- Data Types

-- | todo: explain ...
data Polarity
  = Pos  -- ^ Interpret as a union with the empty set.
  | Neg  -- ^ Interpret as a subtraction from the universal set.
  deriving (Eq, Ord, Show)

-- | todo: explain ...
newtype DNFInt = DNFInt (Polarity, IntSet)
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------------------
-- Constructors

-- | todo: explain ...
top :: DNFInt
top = DNFInt (Neg, IntSet.empty)

-- | todo: explain ...
bottom :: DNFInt
bottom = DNFInt (Pos, IntSet.empty)

-- | todo: explain ...
singleton :: Int -> DNFInt
singleton x = DNFInt (Pos, IntSet.singleton x)

-- | todo: explain ...
fromList :: Polarity -> [Int] -> DNFInt
fromList p is = DNFInt (p, IntSet.fromList is)

-- ----------------------------------------------------------------------------
-- Queries

-- | todo: explain ...
isBottom :: DNFInt -> Bool
isBottom (DNFInt (Pos, i)) = IntSet.null i
isBottom _ = False

-- | todo: explain ...
isTop :: DNFInt -> Bool
isTop (DNFInt (Neg, i)) = IntSet.null i
isTop _ = False

-- | todo: explain ...
member :: Int -> DNFInt -> Bool
member x (DNFInt (Pos, i)) = IntSet.member x i
member x (DNFInt (Neg, i)) = not $ IntSet.member x i

-- | todo: explain ...
elements :: DNFInt -> [Int]
elements = undefined

-- | todo: explain ...
toList :: DNFInt -> (Polarity, [Int])
toList (DNFInt (p, i)) = (p, IntSet.toList i)

-- ----------------------------------------------------------------------------
-- Operators

-- | todo: explain ...
disjunction :: DNFInt -> DNFInt -> DNFInt
disjunction (DNFInt (p1, s1)) (DNFInt (p2, s2)) =
  DNFInt $ case (p1, p2) of
    (Pos, Pos) -> (Pos, IntSet.union s1 s2)
    (Pos, Neg) -> (Neg, IntSet.difference s2 s1)
    (Neg, Pos) -> (Neg, IntSet.difference s1 s2)
    (Neg, Neg) -> (Neg, IntSet.intersection s1 s2)

-- | todo: explain ...
conjunction :: DNFInt -> DNFInt -> DNFInt
conjunction (DNFInt (p1, s1)) (DNFInt (p2, s2)) =
  DNFInt $ case (p1, p2) of
    (Pos, Pos) -> (Pos, IntSet.intersection s1 s2)
    (Pos, Neg) -> (Pos, IntSet.difference s1 s2)
    (Neg, Pos) -> (Pos, IntSet.difference s2 s1)
    (Neg, Neg) -> (Neg, IntSet.union s1 s2)

-- | todo: explain ...
difference :: DNFInt -> DNFInt -> DNFInt
difference (DNFInt (p1, s1)) (DNFInt (p2, s2)) =
  DNFInt $ case (p1, p2) of
    (Pos, Pos) -> (Pos, IntSet.difference s1 s2)
    (Pos, Neg) -> (Pos, IntSet.intersection s1 s2)
    (Neg, Pos) -> (Neg, IntSet.union s1 s2)
    (Neg, Neg) -> (Pos, IntSet.difference s2 s1)

-- | todo: explain ...
negate :: DNFInt -> DNFInt
negate (DNFInt (Pos, xs)) = DNFInt (Neg, xs)
negate (DNFInt (Neg, xs)) = DNFInt (Pos, xs)

-- ----------------------------------------------------------------------------
-- Boolean Algebra

instance BooleanAlgebra DNFInt where
  top = top
  bottom = bottom
  disjunction = disjunction
  conjunction = conjunction
  negate = negate
  difference = difference
