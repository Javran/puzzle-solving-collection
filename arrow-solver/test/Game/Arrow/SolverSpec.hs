module Game.Arrow.SolverSpec where

import Game.Arrow.Simulator
import Game.Arrow.Solver
import Game.Arrow.Types
import Test.Hspec

spec :: Spec
spec = describe "solve" $
  specify "example" $ do
    let xs =
          [ [0, 1, 3, 4]
          , [4, 4, 2, 1, 3]
          , [0, 0, 2, 2, 1, 3]
          , [4, 5, 1, 3, 1, 1, 5]
          , [4, 0, 2, 4, 1, 5]
          , [0, 3, 1, 0, 2]
          , [3, 4, 5, 3]
          ]
        puzzle = Puzzle 6 (Hexagon, 4) xs
        Right moves = solve puzzle
    applyMoves puzzle moves
      `shouldBe` (fmap . fmap) (const 0) xs
