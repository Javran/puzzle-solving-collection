module Solver where

import qualified Data.Map.Strict as M
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


 -}
data Board = Board
  { bdDims :: (Int, Int) -- rows, cols
  , bdCells :: M.Map Coord Cell
  , -- transformed from brRowTreeCounts,
    -- every unsatified row us supposed to have one entity here.
    bdTodoRowCandidates :: [Candidates]
    -- same but for cols.
  , bdTodoColCandidates :: [Candidates]
  , -- all trees whose tent assignment is not yet determined.
    bdTodoTrees :: M.Map Coord [Coord]
  }
