module Game.Fifteen.SimplePartialBoardSpec where

import Game.Fifteen.Board
import Game.Fifteen.SimplePartialBoard
import Game.Fifteen.Types
import Test.Hspec
import Control.Monad
import qualified Data.Vector as V

spec :: Spec
spec = describe "4x4 move examples" $ do
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

