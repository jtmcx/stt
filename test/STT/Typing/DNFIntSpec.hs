-- |

module STT.Typing.DNFIntSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import STT.Typing.Classes.BooleanAlgebraSpec (booleanAlgebraSpec)
import STT.Typing.DNFInt (DNFInt, Polarity(..))
import qualified STT.Typing.DNFInt as DNFInt


-- | Generate a 'Polarity'
genPolarity :: Gen Polarity
genPolarity = Gen.element [Pos, Neg]

-- | Generate a list of upto 100 integers, each in the range [0, 100].
genIntList :: Gen [Int]
genIntList = Gen.list range (Gen.int range)
  where range = Range.linear 0 100

-- | Generate a 'DNFInt'
genDNFInt :: Gen DNFInt
genDNFInt = DNFInt.fromList <$> genPolarity <*> genIntList

spec :: Spec
spec = describe "algebra" $ booleanAlgebraSpec genDNFInt
