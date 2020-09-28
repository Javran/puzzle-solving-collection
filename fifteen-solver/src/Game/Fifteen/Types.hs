module Game.Fifteen.Types where

import qualified Data.Vector as V

type Coord = (Int, Int) -- (row, col)

data Board = Board
  { bdSize :: Int
  , bdNums :: V.Vector Coord -- size of this vector must be bdSize*bdSize-1
  , bdHole :: Coord -- position of the hole
  , bdTiles :: V.Vector (Maybe Int) -- tiles, row-major order.
  }
  deriving (Show)
