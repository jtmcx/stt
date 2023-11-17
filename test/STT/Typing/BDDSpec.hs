module STT.Typing.BDDSpec (spec) where

import Prelude hiding (negate)
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import STT.Typing.Classes.BooleanAlgebraSpec (booleanAlgebraSpec)
import STT.Typing.BDD (BDD(..))
import qualified STT.Typing.BDD as BDD

-- ----------------------------------------------------------------------------
-- Random BDD Generation

genBDD :: Ord a => Gen a -> Gen (BDD a)
genBDD gen = Gen.sized $ \(Size n) -> Gen.frequency
  -- non-recursive generators.
  [ (2, return BDD.top)
  , (2, return BDD.bottom)
  , (2, BDD.singleton <$> gen)
  -- recursive generators
  , (n, Gen.small $ Gen.subterm (genBDD gen) BDD.negate)
  , (n, Gen.small $ Gen.subterm2 (genBDD gen) (genBDD gen) BDD.conjunction)
  , (n, Gen.small $ Gen.subterm2 (genBDD gen) (genBDD gen) BDD.disjunction)
  ]

genIntBDD :: Gen (BDD Int)
genIntBDD = genBDD $ Gen.int (Range.linear 0 10)

-- ----------------------------------------------------------------------------
-- Structure Properties

-- | The function 'allGT k bdd' returns 'True' if every node in 'bdd'
-- has a value greater than than 'k'.
allGT :: Ord a => a -> BDD a -> Bool
allGT _ Top = True
allGT _ Bot = True
allGT k (Fork x l r) = x > k && allGT k l && allGT k r

-- | A BDD is 'wellOrdered' if it forms a min-heap.
wellOrdered :: Ord a => BDD a -> Bool
wellOrdered Top = True
wellOrdered Bot = True
wellOrdered (Fork x l r) =
  allGT x l && wellOrdered l && allGT x r && wellOrdered r

-- | A BDD is 'wellFormed' if ...
wellFormed :: Eq a => BDD a -> Bool
wellFormed Top = True
wellFormed Bot = True
wellFormed (Fork _ l r) = l /= r && wellFormed l && wellFormed r

-- ----------------------------------------------------------------------------
-- Specification

spec :: Spec
spec = do
  describe "structure" $ do
    it "forms a min-heap" $ do
      x <- forAll genIntBDD
      assert $ wellOrdered x

    it "has no duplicate children" $ do
      x <- forAll genIntBDD
      assert $ wellFormed x

  -- BDDs satisfy a boolean algebra.
  describe "algebra" $ booleanAlgebraSpec genIntBDD
