module Game.Arrow.Types
  ( PuzzleShape (..)
  , SqCoord, CubeCoord
  )
where

data PuzzleShape
  = Square
  | Hexagon

type SqCoord = (Int, Int)

{-
  See: https://www.redblobgames.com/grids/hexagons/#coordinates-cube

  Although storing a 3-tuple is more than necessary, it's easier to
  distinguish this from a 2D coordinate.

  INVARIANT: (x,y,z) :: CubeCoord ==> x + y + z == 0
 -}

type CubeCoord = (Int, Int, Int)

