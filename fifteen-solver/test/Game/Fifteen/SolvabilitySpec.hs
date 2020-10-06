{-# LANGUAGE ScopedTypeVariables #-}

module Game.Fifteen.SolvabilitySpec where

import Data.List
import qualified Data.List.Split
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.Solvability
import Game.Fifteen.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

genBoard :: (Int, Int) -> Gen Board
genBoard range = do
  sz <- choose range
  let lastNum = sz * sz -1
      tr x = if x == lastNum then Nothing else Just x
  xs <- shuffle [0 .. lastNum]
  pure $ mkBoard $ (fmap . fmap) tr $ Data.List.Split.chunksOf sz xs

newtype SmallBoard = SmallBoard Board deriving (Show)

instance Arbitrary SmallBoard where
  arbitrary = SmallBoard <$> genBoard (3, 3)

spec :: Spec
spec = do
  describe "mergeSortFromListN" $
    prop "compare with library sort" $
      \(xs :: [Int]) ->
        let n = length xs
            (ys, _) = mergeSortFromListN n xs
         in V.toList ys == sort xs
  -- commented out for now as it runs extremely slow even on 3x3 board.
  {-
  describe "isSolvable" $
    prop "SmallBoard" $
      \(SmallBoard bd) ->
        let goal = goalBoard (bdSize bd)
            solutions = solveBoard goal bd
         in if isSolvable bd
              then label "solvable" $ not (null solutions)
              else label "not solvable" $ null solutions -}
