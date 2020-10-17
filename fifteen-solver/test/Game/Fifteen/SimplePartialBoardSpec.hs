module Game.Fifteen.SimplePartialBoardSpec where

import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.SimplePartialBoard
import Game.Fifteen.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- the purpose of DemoBoard is to verify correctness of SimplePartialBoard,
-- which does not require the Board to be solvable.
newtype DemoBoard = DemoBoard Board deriving (Show)

instance Arbitrary DemoBoard where
  arbitrary =
    DemoBoard <$> do
      sz <- choose (5, 32)
      let bd0 = mkGoalBoard sz
      -- pick a random coord and move hole to that coord.
      hR <- choose (0, sz -1)
      let bd1 =
            if hR == sz -1
              then bd0
              else fromJust $ applyMoves bd0 [(hR, sz -1)]
      hC <- choose (0, sz -1)
      let bd2 =
            if hC == sz -1
              then bd1
              else fromJust $ applyMoves bd1 [(hR, hC)]
      pure bd2

spec :: Spec
spec =
  describe "makeMove" $ do
    describe "4x4 move examples" $ do
      let bd =
            mkBoard
              [ [Just 0, Just 1, Just 2, Just 3]
              , [Just 4, Just 5, Just 6, Just 7]
              , [Just 8, Just 9, Nothing, Just 10]
              , [Just 12, Just 13, Just 14, Just 11]
              ]
          tilesToCheck = [2, 6, 8, 9, 10]
          movePairs = possibleMoves bd
      forM_ movePairs $ \(mv, bd') ->
        specify (show mv) $
          forM_ tilesToCheck $ \tileNum -> do
            let spBd = (bdNums bd V.! tileNum, bdHole bd)
                expectedSpBd = (bdNums bd' V.! tileNum, bdHole bd')
            makeMove spBd mv `shouldBe` Just expectedSpBd
    describe "correctness check against Board module with generated boards" $
      prop "DemoBoard" $
        \(DemoBoard bd) -> conjoin $ do
          let tilesToCheck :: [Int]
              tilesToCheck = fmap (fromJust . bdGet bd . fst) movePairs
              movePairs = possibleMoves bd
          (mv, bd') <- movePairs
          tileNum <- tilesToCheck
          let bdToSpBd = (,) <$> ((V.! tileNum) . bdNums) <*> bdHole
              [spBd, expectedSpBd] = fmap bdToSpBd [bd, bd']
          pure $ makeMove spBd mv === Just expectedSpBd
