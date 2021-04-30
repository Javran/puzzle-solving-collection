module Game.Arrow.SolverSpec where

import Game.Arrow.Simulator
import Game.Arrow.Solver
import Game.Arrow.Types
import Test.Hspec

spec :: Spec
spec = describe "solve" $ do
  let solvable tag puzzle = specify tag $ do
        let result = solve puzzle
        case result of
          Left err ->
            expectationFailure $
              "Expected puzzle to be solvable but got: " <> show err
          Right moves ->
            applyMoves puzzle moves
              `shouldBe` (fmap . fmap) (const 0) (grid puzzle)
  solvable
    "example: mod 4, square 3"
    $ Puzzle
      4
      (Square, 3)
      [ [0, 2, 1]
      , [3, 2, 0]
      , [2, 2, 3]
      ]
  solvable
    "example: mod 6, hexagon 4"
    $ Puzzle
      6
      (Hexagon, 4)
      [ [0, 1, 3, 4]
      , [4, 4, 2, 1, 3]
      , [0, 0, 2, 2, 1, 3]
      , [4, 5, 1, 3, 1, 1, 5]
      , [4, 0, 2, 4, 1, 5]
      , [0, 3, 1, 0, 2]
      , [3, 4, 5, 3]
      ]
