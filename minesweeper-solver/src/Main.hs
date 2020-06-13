module Main
  ( main,
  )
where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

type Coord = (Int, Int)

type Offset = (Int, Int)

-- a mine placement has no more than 8 elements,
-- mines are indicated by True, non-mines False.
type MinePlacement = M.Map Offset Bool

type MineCoords = M.Map Coord Bool

{- ORMOLU_DISABLE -}
-- 2d offset of 8 surrounding tiles.
surroundings :: [Offset]
surroundings =
  [ (-1, -1), (-1, 0), (-1, 1)
  , (0, -1), (0, 1)
  , (1, -1), (1, 0), (1, 1)
  ]
{- ORMOLU_ENABLE -}

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
pickInOrder :: [a] -> [] (a, [a])
pickInOrder xs = do
  (y : ys) <- tails xs
  pure (y, ys)
{-# INLINEABLE pickInOrder #-}

genPlacement :: Int -> [MinePlacement]
genPlacement n0 = convert <$> genAux n0 [] surroundings
  where
    genAux 0 selected _ = pure selected
    genAux n selected candidates = do
      (s, candidates') <- pickInOrder candidates
      genAux (n -1) (s : selected) candidates'
    -- TODO: let's not worry about performance for now.
    convert :: [Offset] -> MinePlacement
    convert ms = M.fromList $ fmap (\c -> (c, c `elem` ms)) surroundings

-- every number tile will be initialized with a list of MinePlacements from here.
placementTable :: V.Vector [MinePlacement]
placementTable = V.fromList $ fmap genPlacement [0 .. 8]

-- tiles that are confirmed mine (True) or not mine (False).
-- note that this includes number tiles as False.
type MineMap = M.Map Coord Bool

data Board = Board
  { -- rows, cols
    bdDims :: (Int, Int),
    bdMines :: MineMap,
    bdNums :: M.Map Coord Int, -- number tiles.
    -- possible ways of arranging mines so that the number tile (key) is satisfied.
    -- note that
    bdCandidates :: M.Map Coord [MineCoords]
  }

checkCandidate :: MineMap -> Coord -> MineCoords -> Bool
checkCandidate mines (x, y) cs =
  -- note that it is unnecessary to check whether current tile is a mine.
  -- this is because number tiles are (will be) explicitly marked as non-mine
  -- during Board construction.
  all check coords
  where
    coords = (\(dx, dy) -> (x + dx, y + dy)) <$> surroundings
    check :: Coord -> Bool
    check curCoord =
      case (mines M.!? curCoord, cs M.!? curCoord) of
        (Nothing, _) -> True
        (_, Nothing) -> True
        (Just actual, Just expect) -> actual == expect

-- TODO: an elimination function :: [MineCoords] -> ([(Coord,Bool)], [MineCoords])
-- that discharges common mine placements from the list.

main :: IO ()
main = do
  print surroundings
  print (genPlacement 7)
