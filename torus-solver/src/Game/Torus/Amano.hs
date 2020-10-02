module Game.Torus.Amano where

{-
  TODO: This solution is described by Amano et al. "How to Solve the Torus Puzzle", Published Jan 13, 2012.
 -}
import Game.Torus.Board

north, south, west, east :: Int -> Int -> Move
[north, south, west, east] =
  [MoveUp, MoveDown, MoveLeft, MoveRight]

type Rotation3 =
  Int -> Int -> Int -> Int -> [Move]

cwA, ccwA, cwB, ccwB, cwC, ccwC, cwD, ccwD :: Rotation3
cwA i j p q =
  [west i q, south j p, east i q, north j p]
ccwA i j p q =
  [south j p, west i q, north j p, east i q]
cwB i j p q =
  [north j p, west i q, south j p, east i q]
ccwB i j p q =
  [west i q, north j p, east i q, south j p]
cwC i j p q =
  [east i q, north j p, west i q, south j p]
ccwC i j p q =
  [north j p, east i q, south j p, west i q]
cwD i j p q =
  [south j p, east i q, north j p, west i q]
ccwD i j p q =
  [east i q, south j p, west i q, north j p]

-- ter for "three elements rotation"
terW :: Rotation3
terW i j p q = cwD i j 1 p <> cwA i j 1 q
