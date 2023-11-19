module STT.Typing.DNFBoolSpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog
import Data.Bifunctor (second)
import STT.Typing.Classes.BooleanAlgebraSpec (booleanAlgebraSpec)
import STT.Typing.DNFBool (DNFBool)
import qualified Hedgehog.Gen as Gen
import qualified STT.Typing.DNFBool as DNFBool

genDNFBool :: Gen DNFBool
genDNFBool = Gen.frequency $ map (second return)
  [ (1, DNFBool.top)
  , (2, DNFBool.singleton True)
  , (2, DNFBool.singleton False)
  , (1, DNFBool.bottom)
  ]

spec :: Spec
spec = describe "algebra" $ booleanAlgebraSpec genDNFBool
