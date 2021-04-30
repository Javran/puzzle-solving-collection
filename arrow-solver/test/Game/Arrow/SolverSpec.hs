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
    {-
      TODO: simulate and verify if the result is correct rather
      having to write out the solution
      (this adds a bit of flexibility also as
      this would allow alternative solutions as long as puzzle is solved.
     -}
    solve (Puzzle 6 (Hexagon, 4) xs)
      `shouldBe` Right
        [ [4, 1, 1, 3]
        , [4, 3, 2, 2, 2]
        , [3, 0, 0, 1, 0, 2]
        , [5, 0, 2, 2, 3, 1, 4]
        , [0, 3, 4, 3, 1, 0]
        , [0, 3, 5, 2, 1]
        , [0, 0, 0, 0]
        ]
