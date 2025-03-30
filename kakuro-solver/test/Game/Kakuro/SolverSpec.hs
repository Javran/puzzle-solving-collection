module Game.Kakuro.SolverSpec where

import Data.Bifunctor
import Data.Char
import qualified Game.Kakuro.DigitSet as DS
import qualified Data.List.NonEmpty as NE
import Game.Kakuro.Solver
import Test.Hspec

ds :: [] Char -> DS.DigitSet
ds = DS.fromList . fmap (\x -> ord x - ord '0')

spec :: Spec
spec = do
  let
    -- shorthand for generating examples
    mk raw = (tag, ds dgs)
      where
        ~[tag, dgs] = words raw
    shouldBe1 a b =
      shouldBe (fmap (bimap NE.toList NE.toList) a) b

  describe "nakedPairsElim" do
    specify "simple pair" do
      nakedPairsElim
        (fmap mk ["a 12", "b 2456", "c 68", "d 19", "e 21"])
        `shouldBe1` [
                      ( fmap mk ["a 12", "e 12"]
                      , fmap mk ["b 456", "d 9"]
                      )
                    ]

    specify "no progress" do
      nakedPairsElim (fmap mk ["a 12", "b 21", "c 68", "d 69", "e 456"])
        `shouldBe1` []
    specify "multi alts" do
      nakedPairsElim (fmap mk ["a 12", "b 2456", "c 68", "d 19", "e 21", "f 68"])
        `shouldBe1` [ (fmap mk ["a 12", "e 12"], fmap mk ["b 456", "d 9"])
                    , (fmap mk ["c 68", "f 68"], fmap mk ["b 245"])
                    ]

  describe "nakedTriplesElim" do
    {-
      Cases for "Naked Triples":

      + (123) (123) (123) - {3/3/3} (in terms of candidates per cell)
      + (123) (123) (12) - {3/3/2} (or some combination thereof)
      + (123) (12) (23) - {3/2/2}
      + (12) (23) (13) - {2/2/2}

      ref: https://www.sudokuwiki.org/naked_candidates

     -}
    specify "2/2/2" do
      nakedTriplesElim (fmap mk ["a 12", "b 2456", "c 68", "d 19", "e 14", "f 24", "g 1247"])
        `shouldBe1` [(fmap mk ["a 12", "e 14", "f 24"], fmap mk ["b 56", "d 9", "g 7"])]
    specify "2/2/2 no progress" do
      nakedTriplesElim (fmap mk ["a 12", "b 56", "c 68", "d 9", "e 14", "f 24", "g 7"])
        `shouldBe1` []
    specify "3/3/3" do
      nakedTriplesElim (fmap mk ["a 124", "b 2456", "c 68", "d 19", "e 124", "f 124", "g 1247"])
        `shouldBe1` [(fmap mk ["a 124", "e 124", "f 124"], fmap mk ["b 56", "d 9", "g 7"])]
    specify "3/3/2" do
      nakedTriplesElim (fmap mk ["a 124", "b 2456", "c 68", "d 19", "e 24", "f 124", "g 1247"])
        `shouldBe1` [(fmap mk ["a 124", "e 24", "f 124"], fmap mk ["b 56", "d 9", "g 7"])]
    specify "3/2/2" do
      nakedTriplesElim (fmap mk ["a 24", "b 2456", "c 68", "d 19", "e 14", "f 124", "g 1247"])
        `shouldBe1` [(fmap mk ["a 24", "e 14", "f 124"], fmap mk ["b 56", "d 9", "g 7"])]
