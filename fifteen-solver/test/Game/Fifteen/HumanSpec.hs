module Game.Fifteen.HumanSpec where

import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.TestGen
import Game.Fifteen.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype SolvableBoard = SolvableBoard Board deriving (Show)

instance Arbitrary SolvableBoard where
  arbitrary =
    SolvableBoard
      <$> (choose (3, 8) >>= genSolvableBoardOfSize)

spec :: Spec
spec =
  describe "solveBoard" $
    prop "correctness on generated boards" $
      \(SolvableBoard bd) ->
        let goal = goalBoard size
            solutions = solveBoard goal bd
            size = bdSize bd
            tag = "size: " <> show size
         in label tag $
              not (null solutions)
                .&&. let moves = head solutions
                      in case applyMoves bd moves of
                           Nothing -> False
                           Just bdFin ->
                             bdTiles bdFin == bdTiles goal
