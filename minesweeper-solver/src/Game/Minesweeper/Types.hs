module Game.Minesweeper.Types
  ( Coord
  , Offset
  , mkOffset
  , applyOffset
  , MinePlacement
  , MineCoords
  , MineMap
  , Board (..)
  , BoardRep (..)
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)

-- Offset is intentionally obscure to avoid misusing it as Coord.
newtype Offset = Offset (Int, Int) deriving (Eq, Ord)

mkOffset :: Int -> Int -> Offset
mkOffset x y = Offset (x, y)

applyOffset :: Offset -> Coord -> Coord
applyOffset (Offset (dx, dy)) (x, y) = (x + dx, y + dy)

-- a mine placement has no more than 8 elements,
-- mines are indicated by True, non-mines False.
type MinePlacement = M.Map Offset Bool

type MineCoords = M.Map Coord Bool

-- tiles that are confirmed mine (True) or not mine (False).
-- note that this includes number tiles as False.
type MineMap = M.Map Coord Bool

data Board = Board
  { -- rows, cols
    bdDims :: (Int, Int)
  , -- note that one should avoid querying on this directly,
    -- use getTile to handle out-of-bound coords properly.
    bdMines :: MineMap
  , bdNums :: M.Map Coord Int -- number tiles.
  -- possible ways of arranging mines so that the number tile (key) is satisfied.
  -- note that satisfied MineCoords would have some common coords discharged.
  , bdCandidates :: M.Map Coord [MineCoords]
  }
  deriving (Show)

-- visual representation of the board which, unlike Board,
-- doesn't keep track of any internal states like candidates.
-- invariant: brNums, brMines and brMissing should never contain conflicting info.
data BoardRep = BoardRep
  { brDims :: (Int, Int)
  , brNums :: M.Map Coord Int -- num tiles
  , brMines :: MineMap
  , -- set of coordinates that we don't have information.
    -- if this field is non-empty, that indicates a partial board representation.
    brMissing :: S.Set Coord
  }
  deriving (Show)
