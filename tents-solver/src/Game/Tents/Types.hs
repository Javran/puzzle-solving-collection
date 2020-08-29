module Game.Tents.Types where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Cell
  = Empty
  | Tree
  | Tent
  deriving (Show, Eq)

type Coord = (Int, Int) -- (row, col)

data BoardRep = BoardRep
  { brDims :: (Int, Int) -- (rows, cols)
  , brRowTentCounts :: V.Vector Int -- # of tents in each row, must be of length rows
  , brColTentCounts :: V.Vector Int -- same but for cols
  , brBoard :: M.Map Coord Cell
  }
  deriving (Show)
