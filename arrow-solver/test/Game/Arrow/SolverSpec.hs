module Game.Arrow.SolverSpec where

import Game.Arrow.Simulator
import Game.Arrow.Solver
import Game.Arrow.Types
import Test.Hspec
import Test.QuickCheck.Gen

genMoves :: Int -> [[a]] -> Gen [[Int]]
genMoves p = (traverse . traverse) (const $ choose (0,p-1))

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
    "example #0: mod 4, square 3"
    $ Puzzle
      4
      (Square, 3)
      [ [0, 2, 1]
      , [3, 2, 0]
      , [2, 2, 3]
      ]
  solvable
    "example #1: mod 6, hexagon 4"
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
  solvable
    "example #2: mod 6, hexagon 4"
    $ Puzzle
      6
      (Hexagon, 4)
      [ [4, 1, 2, 1]
      , [2, 1, 5, 3, 0]
      , [4, 4, 4, 2, 2, 3]
      , [4, 4, 2, 0, 0, 2, 2]
      , [5, 1, 5, 4, 2, 5]
      , [2, 3, 3, 5, 1]
      , [3, 4, 5, 3]
      ]
  solvable
    "example #3: mod 4, square 4"
    $ Puzzle
      4
      (Square, 4)
      [ [2, 2, 3, 3]
      , [3, 2, 3, 3]
      , [1, 0, 3, 0]
      , [3, 2, 2, 0]
      ]
  solvable
    "example #4: mod 2, hexagon 4"
    $ Puzzle
      2
      (Hexagon, 4)
      [ [0, 0, 1, 1]
      , [0, 0, 0, 1, 0]
      , [0, 0, 1, 1, 1, 1]
      , [1, 0, 1, 0, 0, 1, 1]
      , [0, 0, 0, 0, 0, 1]
      , [1, 1, 0, 0, 0]
      , [0, 0, 0, 0]
      ]
