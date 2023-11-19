module STT.Typing.DNFBool
  ( DNFBool
  -- * Constructors
  , top
  , bottom
  , singleton
  -- * Queries
  , isTop
  , isBottom
  , member
  , elements
  -- * Operators
  , disjunction
  , conjunction
  , difference
  , negate
  ) where

import Prelude hiding (negate)
import Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import STT.Typing.Classes.BooleanAlgebra (BooleanAlgebra)
import qualified STT.Typing.Classes.BooleanAlgebra as BooleanAlgebra

-- ----------------------------------------------------------------------------
-- Data Types

-- | todo: explain ...
newtype DNFBool = DNFBool Int
  deriving (Eq, Ord)

-- ----------------------------------------------------------------------------
-- Constructors

-- | todo: explain ...
top :: DNFBool
top = DNFBool 3

-- | The empty set.
bottom :: DNFBool
bottom = DNFBool 0

-- | todo: explain ...
singleton :: Bool -> DNFBool
singleton True  = DNFBool 1
singleton False = DNFBool 2

-- ----------------------------------------------------------------------------
-- Queries

-- | todo: explain ...
isBottom :: DNFBool -> Bool
isBottom (DNFBool x) = x == 0

-- | todo: explain ...
isTop :: DNFBool -> Bool
isTop (DNFBool x) = x == 3

-- | todo: explain ...
member :: Bool -> DNFBool -> Bool
member x d = isBottom (difference (singleton x) d)

-- | todo: explain ...
elements :: DNFBool -> [Bool]
elements = undefined

-- ----------------------------------------------------------------------------
-- Operators

-- | todo: explain ...
disjunction :: DNFBool -> DNFBool -> DNFBool
disjunction (DNFBool x) (DNFBool y) = DNFBool (x .|. y)

-- | todo: explain ...
conjunction :: DNFBool -> DNFBool -> DNFBool
conjunction (DNFBool x) (DNFBool y) = DNFBool (x .&. y)

-- | todo: explain ...
difference :: DNFBool -> DNFBool -> DNFBool
difference d1 d2 = conjunction d1 (negate d2)

-- | todo: explain ...
negate :: DNFBool -> DNFBool
negate (DNFBool x) = DNFBool (3 .&. Bits.complement x)

-- ----------------------------------------------------------------------------
-- Boolean Algebra

instance BooleanAlgebra DNFBool where
  top = top
  bottom = bottom
  disjunction = disjunction
  conjunction = conjunction
  negate = negate
  difference = difference

-- ----------------------------------------------------------------------------
-- Printing

instance Show DNFBool where
  show (DNFBool 0) = "Empty"
  show (DNFBool 1) = "true"
  show (DNFBool 2) = "false"
  show (DNFBool 3) = "Bool"
  show (DNFBool _) = undefined
