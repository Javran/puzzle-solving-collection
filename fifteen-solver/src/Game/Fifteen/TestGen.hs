module Game.Fifteen.TestGen where

{-
  Generate random boards for testing and benchmarking.
 -}

import Test.QuickCheck
import qualified Data.List.Split
import Game.Fifteen.Types
import Game.Fifteen.Board

genBoardOfSize :: Int -> Gen Board
genBoardOfSize sz = do
  let lastNum = sz * sz -1
      tr x = if x == lastNum then Nothing else Just x
  xs <- shuffle [0 .. lastNum]
  pure $ mkBoard $ (fmap . fmap) tr $ Data.List.Split.chunksOf sz xs

