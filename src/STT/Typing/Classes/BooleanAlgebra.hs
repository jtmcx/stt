module STT.Typing.Classes.BooleanAlgebra
  ( -- * Definition
    BooleanAlgebra(..)
    -- * Infix Operators
  , (\/)
  , (/\)
  ) where

import Prelude hiding (negate)

-- ----------------------------------------------------------------------------
-- Definition

-- | Formally, a Boolean algebra is defined as a bounded, complemented,
-- distributive lattice. It is a set equipped with two elements \(⊤\) and \(⊥\)
-- (called 'top' and 'bottom'), two binary operators \(∨\) and \(∧\) (called
-- 'disjunction' and 'conjunction'), and a unary operator \(¬\) (called
-- 'negate'). A Boolean algebra respects the following laws:
--
-- \[\begin{align}
--       x∨⊥ &= x            &      x∧⊤ &= x            \tag{identity}       \\
--       x∨⊤ &= ⊤            &      x∧⊥ &= ⊥            \tag{annihilation}   \\
--       x∨y &= y∨x          &      x∧y &= y∧x          \tag{commutativity}  \\
--   x∨(y∨z) &= (x∨y)∨z      &  x∧(y∧z) &= (x∧y)∧z      \tag{associativity}  \\
--   x∨(y∧z) &= (x∧y)∨(x∧z)  &  x∧(y∨z) &= (x∨y)∧(x∨z)  \tag{distributivity} \\
--       x∨x &= x            &      x∧x &= x            \tag{idempotence}    \\
--      x∨¬x &= ⊤            &     x∧¬x &= ⊥            \tag{complement}     \\
--    ¬(x∨y) &= ¬x∧¬y        &   ¬(x∧y) &= ¬x∨¬y        \tag{demorgan's}     \\
-- \end{align}\]
--
class BooleanAlgebra a where
  -- | The greatest element.
  top :: a
  -- | The least element.
  bottom :: a
  -- | Join, union, "or", etc.
  disjunction :: a -> a -> a
  -- | Meet, intersection, "and", etc.
  conjunction :: a -> a -> a
  -- | Logical complement.
  negate :: a -> a

  -- | When dealing with sets, it's often desirable to provide an optimized
  -- definition for set asymmetric difference, i.e., the subtraction of one set
  -- from another. If unspecified, the definition defaults to:
  --
  -- > difference x y = x /\ negate y
  --
  difference :: a -> a -> a
  difference x y = x /\ negate y

-- ----------------------------------------------------------------------------
-- Infix Operators

infixr 6 /\
infixr 5 \/

-- | Shorthand for 'disjunction'
(\/) :: BooleanAlgebra a => a -> a -> a
(\/) = disjunction

-- | Shorthand for 'conjunction'
(/\) :: BooleanAlgebra a => a -> a -> a
(/\) = conjunction

-- ----------------------------------------------------------------------------
-- Booleans

instance BooleanAlgebra Bool where
  top = True
  bottom = False
  disjunction = (||)
  conjunction = (&&)
  negate = not
