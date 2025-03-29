module Game.Fifteen.SimplePartialBoardSpec where

import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.SimplePartialBoard
import Game.Fifteen.TestGen
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
      sz <- choose (5, 12)
      let bd0 = mkGoalBoard sz
      -- pick a random coord and move hole to that coord.
      hR <- choose (0, sz - 1)
      let bd1 =
            if hR == sz - 1
              then bd0
              else fromJust $ applyMoves bd0 [(hR, sz - 1)]
      hC <- choose (0, sz - 1)
      let bd2 =
            if hC == sz - 1
              then bd1
              else fromJust $ applyMoves bd1 [(hR, hC)]
      pure bd2

{-
  Convert to SPBoard given a tile (specified by tileNum).
 -}
bdToSpBd :: Int -> Board -> SPBoard
bdToSpBd tileNum = (,) <$> ((V.! tileNum) . bdNums) <*> bdHole

spec :: Spec
spec = do
  describe "applyMove" $ do
    describe "4x4 move examples" $ do
      let bd =
            mkBoard
              [ [Just 0, Just 1, Just 2, Just 3]
              , [Just 4, Just 5, Just 6, Just 7]
              , [Just 8, Just 9, Nothing, Just 10]
              , [Just 12, Just 13, Just 14, Just 11]
              ]
          movePairs = possibleMoves bd
      forM_ movePairs $ \(mv, bd') ->
        specify (show mv) $ do
          {-
            we are not just checking tiles that on the same row or col
            of the hole, we check that all tiles are present and correct.
            this is important as it covers cases where curCoord is not involved
            with the move at all.
           -}
          forM_ [0 .. 4 * 4 - 2] $ \tileNum -> do
            let spBd = bdToSpBd tileNum bd
                expectedSpBd = bdToSpBd tileNum bd'
            applyMove spBd mv `shouldBe` Just expectedSpBd
    describe "all tiles should be moved to the expected locations" $
      prop "DemoBoard" $
        \(DemoBoard bd) -> conjoin $ do
          let sz = bdSize bd
              movePairs = possibleMoves bd
          (mv, bd') <- movePairs
          tileNum <- [0 .. sz * sz - 2]
          let [spBd, expectedSpBd] = fmap (bdToSpBd tileNum) [bd, bd']
          pure $ applyMove spBd mv === Just expectedSpBd
  describe "searchMoveTile" $ do
    describe "SmallBoard, no boundingRect or pCoords restriction" $
      prop "correctness" $ do
        sz <- choose (3, 3)
        bd <- genBoardOfSize sz
        let boundingRect = ((0, 0), (sz - 1, sz - 1))
            rowOrCol = choose (0, sz - 1)
        dstCoord <- (,) <$> rowOrCol <*> rowOrCol
        srcTile <- choose (0, sz * sz - 2)
        let srcCoord = bdNums bd V.! srcTile
            Just moves =
              searchMoveTile boundingRect mempty srcCoord (bdHole bd) dstCoord
            Just bd' = Game.Fifteen.Board.applyMoves bd moves
        pure $ bdGet bd' dstCoord === Just srcTile
