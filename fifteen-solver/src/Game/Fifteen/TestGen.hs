module Game.Fifteen.TestGen where

{-
  Generate random boards for testing and benchmarking.
 -}

import qualified Data.List.Split
import Game.Fifteen.Board
import Game.Fifteen.Solvability
import Game.Fifteen.Types
import Test.QuickCheck

genBoardOfSize :: Int -> Gen Board
genBoardOfSize sz = do
  let lastNum = sz * sz -1
      tr x = if x == lastNum then Nothing else Just x
  xs <- shuffle [0 .. lastNum]
  pure $ mkBoard $ (fmap . fmap) tr $ Data.List.Split.chunksOf sz xs

genSolvableBoardOfSize :: Int -> Gen Board
genSolvableBoardOfSize sz = do
  bd <- genBoardOfSize sz
  pure $
    if isSolvable bd
      then bd
      else flipSolvability bd

testGen :: IO ()
testGen = pure ()
