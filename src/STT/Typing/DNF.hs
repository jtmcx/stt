{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- | Disjunctive Normal Form

module STT.Typing.DNF
  ( DNF
  -- * Constructors
  , top
  , bottom
  -- * Operators
  , disjunction
  , conjunction
  , difference
  , negate
  -- * Queries
  , subset
  , isEmpty
  -- * TODO
  , dnf
  ) where

import Prelude hiding (negate)
import STT.Syntax (Ty(..))
import STT.Typing.BDD (BDD)
import STT.Typing.Classes.BooleanAlgebra (BooleanAlgebra)
import STT.Typing.DNFBool (DNFBool)
import STT.Typing.DNFInt (DNFInt)
import qualified STT.Typing.BDD as BDD
import qualified STT.Typing.Classes.BooleanAlgebra as BooleanAlgebra
import qualified STT.Typing.DNFBool as DNFBool
import qualified STT.Typing.DNFInt as DNFInt

-- ----------------------------------------------------------------------------
-- Data Types

-- | A type in disjunctive normal form.
data DNF = DNF
  { dnfUnit :: Bool             -- ^ Is the unit included?
  , dnfBool :: DNFBool          -- ^ Set of boolean types
  , dnfInt  :: DNFInt           -- ^ Set of integer types
  , dnfPair :: BDD (DNF, DNF)   -- ^ Set of pair types
  }
  deriving (Eq, Ord, Show)

-- ----------------------------------------------------------------------------
-- Uilities

mapalg0 :: (forall a. BooleanAlgebra a => a) -> DNF
mapalg0 x = DNF
  { dnfUnit = x
  , dnfBool = x
  , dnfInt  = x
  , dnfPair = x
  }

mapalg1 :: (forall a. BooleanAlgebra a => a -> a) -> DNF -> DNF
mapalg1 f d = DNF
  { dnfUnit = f (dnfUnit d)
  , dnfBool = f (dnfBool d)
  , dnfInt  = f (dnfInt d)
  , dnfPair = f (dnfPair d)
  }

mapalg2 :: (forall a. BooleanAlgebra a => a -> a -> a) -> DNF -> DNF -> DNF
mapalg2 op d1 d2 = DNF
  { dnfUnit = dnfUnit d1 `op` dnfUnit d2
  , dnfBool = dnfBool d1 `op` dnfBool d2
  , dnfInt  = dnfInt d1  `op` dnfInt d2
  , dnfPair = dnfPair d1 `op` dnfPair d2
  }

-- ----------------------------------------------------------------------------
-- Constructors

-- | A 'DNF' representing the @Any@ type
top :: DNF
top = mapalg0 BooleanAlgebra.top

-- | A 'DNF' representing the @Empty@ type
bottom :: DNF
bottom = mapalg0 BooleanAlgebra.bottom

-- ----------------------------------------------------------------------------
-- Operators

-- | The disjunction of two DNFs
disjunction :: DNF -> DNF -> DNF
disjunction = mapalg2 BooleanAlgebra.disjunction

-- | The conjunction of two DNFs
conjunction :: DNF -> DNF -> DNF
conjunction = mapalg2 BooleanAlgebra.conjunction

-- | The difference of two DNFs
difference :: DNF -> DNF -> DNF
difference = mapalg2 BooleanAlgebra.difference

-- | The negation of a DNF
negate :: DNF -> DNF
negate = mapalg1 BooleanAlgebra.negate

-- ----------------------------------------------------------------------------
-- Boolean Algebra

instance BooleanAlgebra DNF where
  top = top
  bottom = bottom
  disjunction = disjunction
  conjunction = conjunction
  negate = negate
  difference = difference

-- ----------------------------------------------------------------------------
-- Queries

subset :: DNF -> DNF -> Bool
subset t u = isEmpty (t `difference` u)

-- | Return true if the given DNF is @Empty@.
isEmpty :: DNF -> Bool
isEmpty d =
  not (dnfUnit d) &&
  DNFBool.isBottom (dnfBool d) &&
  DNFInt.isBottom (dnfInt d) &&
  isPairEmpty (dnfPair d)

-- | todo : explain ...
isPairEmpty :: BDD (DNF, DNF) -> Bool
isPairEmpty b = aux b (top, top) []
  where
    -- needs explaining ...
    aux :: BDD (DNF, DNF) -> (DNF, DNF) -> [(DNF, DNF)] -> Bool
    aux bdd (u1, u2) n =
      case bdd of
        BDD.Bot -> True
        BDD.Top -> isEmpty u1 || isEmpty u2 || theta (u1, u2) n
        BDD.Fork (t1, t2) l r ->
             aux l (t1 `conjunction` u1, t2 `conjunction` u2) n
          && aux r (u1, u2) ((t1, t2) : n)

    -- needs explaining ...
    theta :: (DNF, DNF) -> [(DNF, DNF)] -> Bool
    theta _ [] = False
    theta (u1, u2) ((t1, t2) : n) =
         (u1 `subset` t1 || theta (u1 `difference` t1, u2) n)
      && (u2 `subset` t2 || theta (u1, u2 `difference` t2) n)

-- -- | todo : explain ...
-- isFuncEmpty :: BDD (DNF, DNF) -> Bool
-- isFuncEmpty b = aux b bottom [] []
--   where
--     -- needs explaining ...
--     aux :: BDD (DNF, DNF) -> DNF -> [(DNF, DNF)] -> [(DNF, DNF)] -> Bool
--     aux BDD.Bot _ _ _  = True
--     aux BDD.Top _ _ [] = False
--     aux BDD.Top u p ((t1, t2) : n) =
--       (t1 `subset` u && theta (t1, negate t2) p) || aux BDD.Top u p n
--     aux (BDD.Fork (t1, t2) l r) u p n =
--       aux l (u `disjunction` t1) ((t1, t2) : p) n && aux r u p ((t1, t2) : n)

--     -- needs explaining ...
--     theta :: (DNF, DNF) -> [(DNF, DNF)] -> Bool
--     theta (t1, t2) [] = isEmpty t1 || isEmpty t2
--     theta (t1, t2) ((u1, u2) : p) =
--          (t1 `subset` u1 || theta (t1 `difference` u1, t2) p)
--       && (t2 `subset` negate u2 || theta (t1, t2 `conjunction` u2) p)


-- ----------------------------------------------------------------------------
-- Conversions

-- | Convert a type into disjunctive normal form.
dnf :: Ty -> DNF
dnf = \case
  TAny           -> top
  TEmpty         -> bottom
  TUnit          -> bottom { dnfUnit = True }
  TBool Nothing  -> bottom { dnfBool = DNFBool.top }
  TBool (Just x) -> bottom { dnfBool = DNFBool.singleton x }
  TInt Nothing   -> bottom { dnfInt = DNFInt.top }
  TInt (Just x)  -> bottom { dnfInt = DNFInt.singleton x }
  TPair t u      -> bottom { dnfPair = BDD.singleton (dnf t, dnf u) }
  TFn _ _        -> bottom -- TODO
  TOr t u        -> dnf t `disjunction` dnf u
  TAnd t u       -> dnf t `conjunction` dnf u
  TDiff t u      -> dnf t `difference` dnf u
  TNot t         -> negate (dnf t)
  
