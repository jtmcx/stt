{-# LANGUAGE ScopedTypeVariables #-}

module STT.Typing.Classes.BooleanAlgebraSpec
  ( booleanAlgebraSpec
  , spec
  ) where

import Prelude hiding (negate)
import Test.Hspec
import Test.Hspec.Hedgehog
import STT.Typing.Classes.BooleanAlgebra

booleanAlgebraSpec :: forall a. (BooleanAlgebra a, Show a, Eq a)
                   => Gen a
                   -> Spec
booleanAlgebraSpec gen = do
  describe "disjunction" $ do
    it "has identity: ⊥ ∨ x = x" $ do
      x <- forAll gen
      bottom \/ x === x

    it "has identity: x ∨ ⊥ = x" $ do
      x <- forAll gen
      x \/ bottom === x

    it "has annihilator: ⊤ ∨ x = ⊤" $ do
      x <- forAll gen
      top \/ x === top

    it "has annihilator: x ∨ ⊤ = ⊤" $ do
      x <- forAll gen
      x \/ top === top

    it "is idempotent: x ∨ x = x" $ do
      x <- forAll gen
      x \/ x === x

    it "is commutative: x ∨ y = y ∨ x" $ do
      x <- forAll gen
      y <- forAll gen
      x \/ y === y \/ x

    it "is associative: x ∨ (y ∨ z) = (x ∨ y) ∨ z" $ do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      x \/ (y \/ z) === (x \/ y) \/ z

  describe "conjunction" $ do
    it "has identity: ⊤ ∧ x = x" $ do
      x <- forAll gen
      top /\ x === x

    it "has identity: x ∧ ⊤ = x" $ do
      x <- forAll gen
      x /\ top === x

    it "has annihilator: ⊥ ∧ x = ⊥" $ do
      x <- forAll gen
      bottom /\ x === bottom

    it "has annihilator: x ∧ ⊥ = ⊥" $ do
      x <- forAll gen
      x /\ bottom === bottom

    it "is idempotent: x ∧ x = x" $ do
      x <- forAll gen
      x /\ x === x

    it "is commutative: x ∧ y = y ∧ x" $ do
      x <- forAll gen
      y <- forAll gen
      x /\ y === y /\ x

    it "is associative: x ∧ (y ∧ z) = (x ∧ y) ∧ z" $ do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      x /\ (y /\ z) === (x /\ y) /\ z

  describe "lattice" $ do
    it "is distributive: x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)" $ do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      x /\ (y \/ z) === (x /\ y) \/ (x /\ z)

    it "is distributive: x ∨ (y ∧ z) = (x ∨ y) ∧ (x ∨ z)" $ do
      x <- forAll gen
      y <- forAll gen
      z <- forAll gen
      x \/ (y /\ z) === (x \/ y) /\ (x \/ z)

    it "is absorptive: x ∧ (x ∨ y) = x" $ do
      x <- forAll gen
      y <- forAll gen
      x /\ (x \/ y) === x

    it "is absorptive: x ∨ (x ∧ y) = x" $ do
      x <- forAll gen
      y <- forAll gen
      x \/ (x /\ y) === x

  describe "negate" $ do
    it "is involutive: ¬ ¬ x = x" $ do
      x <- forAll gen
      negate (negate x) === x

    it "has complement: ¬ ⊤ = ⊥" $ do
      negate (top :: a) `shouldBe` bottom

    it "has complement: ¬ ⊥ = ⊤" $ do
      negate (bottom :: a) `shouldBe` top

    it "has complement: x ∧ ¬ x = ⊥" $ do
      x <- forAll gen
      x /\ negate x === bottom

    it "has complement: x ∨ ¬ x = ⊤" $ do
      x <- forAll gen
      x \/ negate x === top

    it "has demorgans: ¬ (x ∨ y) = ¬ x ∧ ¬ y" $ do
      x <- forAll gen
      y <- forAll gen
      negate (x \/ y) === negate x /\ negate y

    it "has demorgans: ¬ (x ∧ y) = ¬ x ∨ ¬ y" $ do
      x <- forAll gen
      y <- forAll gen
      negate (x /\ y) === negate x \/ negate y

  describe "difference" $ do
    it "has identity: x \\ ⊥ = x" $ do
      x <- forAll gen
      x `difference` bottom === x

    it "has identity: ⊤ \\ x = ¬ x" $ do
      x <- forAll gen
      top `difference` x === negate x

    it "has annihilator: ⊥ \\ x = ⊥" $ do
      x <- forAll gen
      bottom `difference` x === bottom

    it "has annihilator: x \\ ⊤ = ⊥" $ do
      x <- forAll gen
      x `difference` top === bottom

    it "is and-not: x \\ y = x ∧ ¬ y" $ do
      x <- forAll gen
      y <- forAll gen
      x `difference` y === x /\ negate y


-- | Do nothing. Defined and exported to satisfy @hspec-discover@.
spec :: Spec
spec = return ()
