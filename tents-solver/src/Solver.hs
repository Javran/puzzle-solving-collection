{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Solver where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Types

-- a Piece is a set of coord-cell assignments
-- that has to be applied to the board at the same time.
type Piece = M.Map Coord Cell

{-

a `Candidates` is a set of alternative Piece
meant to be applied to the same set of Coords,

- if a Candidates is an empty list, it's impossible to satisfy.
- if a Candidates is a singleton list, it is the only possible set of assignment.

Note that duplication is never verified and therefore must be maintained by
contract on other functions.

 -}
type Candidates = [Piece]

-- using newtype to avoid confusion between this and Coord.
newtype Dir = Dir (Int, Int) -- diff by (row, col)

applyDir :: Dir -> Coord -> Coord
applyDir (Dir (dr, dc)) (r, c) = (r + dr, c + dc)

allDirs :: [Dir]
allDirs = Dir <$> [(0, -1), (0, 1), (-1, 0), (1, 0)]

{-

Board keeps track of info necessary for solving the puzzle.

Rules are:

Place tents in the empty squares in such a way that:

(1) no two tents are adjacent, even diagonally
(2) the number of tents in each row and column matches the numbers around the edge of the grid
(3) it is possible to match tents to trees so that each tree is orthogonally adjacent to its own tent (but may also be adjacent to other tents).

To address (2) for each unsolved row or column,
we'll use bdTodo{Row,Col}Candidates to store pieces that are (shallowly) consistent with rest of the board.

To address (3), we have `bdTodoTrees` that keep track of possible tree-tent assignments for each tree.

Note that those rules have an important implication:
if a cell is not adjacent vertically or horizontally to a tree,
it is impossible for it to be a tent, therefore it must be an empty cell.
This implication does not rely on current solving state (as tree is never introduced during solving),
and therefore can be done during preprocessing.
(TODO) we want to establish an invariant on this data type that all empty cells that can be inferred from this implication
will be inferred and put in bdCells, so in subsequent solving steps we never need to worry about it.

In addition, it is beneficial to have those cells set to empty as soon as possible,
as this process cuts down search space drastically.

 -}
data Board = Board
  { bdDims :: (Int, Int) -- rows, cols
  , bdCells :: M.Map Coord Cell
  , -- transformed from brRowTreeCounts,
    -- every unsatified row is supposed to have one entity here.
    bdTodoRowCandidates :: [Candidates]
  , -- same but for cols.
    bdTodoColCandidates :: [Candidates]
  , -- all trees whose tent assignment is not yet determined.
    bdTodoTrees :: M.Map Coord [Coord]
  }

mkBoard :: BoardRep -> Maybe Board
mkBoard BoardRep {brDims = bdDims@(rows, cols), brBoard} = do
  let allCoords = S.fromList [(r, c) | r <- [0 .. rows -1], c <- [0 .. cols -1]]
      missingCoords = allCoords `S.difference` M.keysSet brBoard
      allTrees = M.keysSet $ M.filter (== Tree) brBoard
      simpleEmptyCoords = S.toList $ S.filter (not . nearTrees) missingCoords
        where
          nearTrees c = any (\dir -> applyDir dir c `S.member` allTrees) allDirs
      bdCells = brBoard `M.union` M.fromList ((,Empty) <$> simpleEmptyCoords)
  pure Board {bdDims, bdCells}
