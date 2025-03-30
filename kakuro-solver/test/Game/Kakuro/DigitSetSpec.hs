module Game.Kakuro.DigitSetSpec where

import Control.Monad
import Data.Bits
import Data.Function
import qualified Data.IntSet as IS
import qualified Game.Kakuro.DigitSet as DS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

{-
  Here we just assume IntSet is implemented correctly,
  so we simply compare our behavior to that of IntSet.
 -}

{-
  In practice we can have a wider range of elements,
  but for purpose of testing, let's just make sure
  this works for 0~9.
 -}
elemsOfInterest :: [Int]
elemsOfInterest = [0 .. 9]

getEoi :: Gen Int
getEoi = chooseInt (0, 9)

getDigits :: Gen [Int]
getDigits = do
  v <- chooseInt (0, 0x3ff)
  pure $ concatMap (\i -> i <$ guard (testBit v i)) elemsOfInterest

isEqv :: IS.IntSet -> DS.DigitSet -> Property
isEqv is ds =
  foldr
    (\e p -> (IS.member e is === DS.member e ds) .&&. p)
    (property True)
    elemsOfInterest

spec :: Spec
spec = do
  prop "Equivalent IntSet" do
    forAll getDigits \ds ->
      isEqv (IS.fromList ds) (DS.fromList ds)

  prop "toList . fromList" do
    forAll getDigits \ds ->
      DS.toList (DS.fromList ds) === ds

  prop "size" do
    forAll getDigits \ds ->
      IS.size (IS.fromList ds) === DS.size (DS.fromList ds)

  prop "singleton" do
    forAll getEoi \x ->
      isEqv (IS.singleton x) (DS.singleton x)

  do
    let
      propTest tag fI fD propCmp =
        prop tag do
          forAll getEoi \e ->
            forAll getDigits \ds -> do
              let
                ls = IS.fromList ds
                rs = DS.fromList ds
                memberTag =
                  if IS.member e ls then "hit" else "miss"
              label memberTag $ propCmp (fI e ls) (fD e rs)

    propTest "member" IS.member DS.member (===)
    propTest "insert" IS.insert DS.insert isEqv
    propTest "delete" IS.delete DS.delete isEqv

  do
    let
      binProp tag binI binD =
        prop tag do
          forAll getDigits \ls ->
            forAll getDigits \rs ->
              isEqv
                ((binI `on` IS.fromList) ls rs)
                ((binD `on` DS.fromList) ls rs)

    binProp "union" IS.union DS.union
    binProp "intersection" IS.intersection DS.intersection
    binProp "difference" IS.difference DS.difference

  prop "unions" do
    forAll (chooseInt (3, 5)) \n ->
      forAll (replicateM n getDigits) \sets -> do
        let
          lSets = fmap IS.fromList sets
          rSets = fmap DS.fromList sets
        label ("len " <> show n) $
          isEqv (IS.unions lSets) (DS.unions rSets)
