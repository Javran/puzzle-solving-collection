module Game.Minesweeper.Types where

import qualified Data.Map.Strict as M

type Coord = (Int, Int)

type Offset = (Int, Int)

-- a mine placement has no more than 8 elements,
-- mines are indicated by True, non-mines False.
type MinePlacement = M.Map Offset Bool

type MineCoords = M.Map Coord Bool

-- tiles that are confirmed mine (True) or not mine (False).
-- note that this includes number tiles as False.
type MineMap = M.Map Coord Bool

data Board = Board
  { -- rows, cols
    bdDims :: (Int, Int),
    -- note that one should avoid querying on this directly,
    -- use getTile to handle out-of-bound coords properly.
    bdMines :: MineMap,
    bdNums :: M.Map Coord Int, -- number tiles.
    -- possible ways of arranging mines so that the number tile (key) is satisfied.
    -- note that satisfied MineCoords would have some common coords discharged.
    bdCandidates :: M.Map Coord [MineCoords]
  }
  deriving (Show)

-- the intermediate representation after parsing.
type TmpBoard =
  ( -- rows, cols
    (Int, Int),
    -- num map
    M.Map Coord Int,
    -- mine map
    MineMap
  )
