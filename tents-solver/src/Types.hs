module Types where

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
  , brRowTreeCounts :: V.Vector Int -- # of trees in each row, must be of length rows
  , brColTreeCounts :: V.Vector Int -- same but for cols
  , brBoard :: M.Map Coord Cell
  }
  deriving (Show)
