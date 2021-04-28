module Game.Arrow.Types
  ( PuzzleShape (..)
  , SqCoord
  , CubeCoord
  , PuzzleType
  , Puzzle (..)
  , Err (..)
  )
where

data PuzzleShape
  = Square
  | Hexagon
  deriving (Show, Eq)

type SqCoord = (Int, Int)

{-
  See: https://www.redblobgames.com/grids/hexagons/#coordinates-cube

  Although storing a 3-tuple is more than necessary, it's easier to
  distinguish this from a 2D coordinate.

  INVARIANT: (x,y,z) :: CubeCoord ==> x + y + z == 0
 -}

type CubeCoord = (Int, Int, Int)

{-
  The integer represents side length of a puzzle and must be positive.
 -}
type PuzzleType = (PuzzleShape, Int)

data Puzzle = Puzzle
  { opMod :: Int
  , pzType :: PuzzleType
  , grid :: [[Int]]
  }
  deriving (Show, Eq)

data Err i
  = NoMultInv i
  | Underdetermined
  | Todo String
  | Gaussian String
  deriving (Show, Eq)
