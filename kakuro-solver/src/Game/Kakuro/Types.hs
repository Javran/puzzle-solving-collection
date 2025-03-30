module Game.Kakuro.Types
  ( MinMax2D (..)
  , minMax2D
  , TotCnt (..)
  , Puzzle (..)
  , Orien (..)
  , ClueInd
  , SolState (..)
  , combs
  , getCombs
  , IsPuzzleRep (..)
  , module Game.Kakuro.Uldr
  )
where

import Control.DeepSeq
import Control.Monad
import qualified Data.DList as DL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Set as S
import GHC.Generics
import qualified Game.Kakuro.DigitSet as DS
import Game.Kakuro.Uldr

newtype MinMax2D u v = MinMax2D {getMinMax2D :: ((u, u), (v, v))}
  deriving (Semigroup) via ((Min u, Max u), (Min v, Max v))
  deriving stock (Show, Generic)
  deriving newtype (NFData)

minMax2D :: (u, v) -> MinMax2D u v
minMax2D (u, v) = MinMax2D ((u, u), (v, v))

{-
  We have multiple representations of a puzzle, here is a summary:

  - Puzzle

    + a sparse representation meant to support conversion from all other formats
    + conversion to this type needs to be careful, as we assume this type would
      not contain self-contradicting info.

  - PuzzleRaw

    + supports JSON and YAML
    + this format is meant to be friendly for human input
    + however hints are separated and has to be re-constructed

  - PuzzleCompact

    + a type that is just a dimension pair and the grid with clues inside.
    + has a somewhat compact seralization to String.
    + allow storing information in a compact manner

  - SolState

    + this representation is more about state of a solution than puzzle representation,
      however one should be able to recover all information necessary.
 -}

data TotCnt = TotCnt Int Int
  deriving (Show, Generic)

instance NFData TotCnt

{-
  Double layered map for all possible combinations:

  - first index: the number we should sum up to.
  - second index: size of the set.
  - value: all alternatives, represented each as an IntSet

 -}
combs :: IM.IntMap (IM.IntMap [DS.DigitSet])
combs =
  IM.map (IM.map DL.toList . IM.fromListWith (<>) . fmap (\s -> (DS.size s, DL.singleton s)) . DL.toList)
    . IM.fromListWith (<>)
    . fmap (\xs -> (sum xs, DL.singleton (DS.fromList xs)))
    . tail -- skip first one which is the empty list
    $ filterM (\_ -> [False, True]) [1 .. 9]

getCombs :: TotCnt -> Maybe [DS.DigitSet]
getCombs (TotCnt tot cnt) = combs IM.!? tot >>= (IM.!? cnt)

{-

  A Puzzle type meant to be:

  - a bridge between all different representations.
  - a transition point to SolState.

  Note that we assume Puzzle type would not contain self-contradicting info,
  therefore little verifications are made during the processing of Puzzle to
  other types.

  Fields:

  - pzCells: blank cells to be solved.
  - pzBounds: boundary of all cells in pzCells
  - pzCluesHori: clues that goes left-to-right.
      + key is the Coord of first cell
      + value `TotCnt t c` stands for total sum being `t`, within `c` cells
  - pzCluesVert: same as pzCluesHori but goes top-to-bottom.

 -}
data Puzzle = Puzzle
  { pzCells :: S.Set Coord
  , pzBounds :: MinMax2D Int Int -- ((rMin, rMax), (cMin, cMax))
  , pzCluesHori :: M.Map Coord TotCnt
  , pzCluesVert :: M.Map Coord TotCnt
  }
  deriving (Show, Generic)

instance NFData Puzzle

class IsPuzzleRep p where
  repToPuzzle :: p -> Either String Puzzle

instance IsPuzzleRep Puzzle where
  repToPuzzle = Right

-- Orientation of a clue, either horizontal or vertical.
data Orien = Hori | Vert
  deriving (Show, Eq, Ord)

-- This allows us to refer to a specific clue.
type ClueInd = (Coord, Orien)

{-
  State of current solution.

  - ssSolved: cells already solved
  - ssCandidates: cells not yet solved.
    Value represents all possible candidates.
  - ssClues: all clues that are still relevant.
    As it progresses, a solved cell will be entirely removed
    from any clue's Coord set
  - ssCoordToClue: a reverse map from a cell to all clues
    it belongs to.
    (TODO) technically we have no need to update this one,
    since (1) solved cells are unlikely to be looked up anyway
    (2) if a ClueInd lookup ends up empty, we can simply ignore it.
    This being in SolState is tentative.

  Overall, the idea is to keep track of both cell candidates
  and possible alternatives for each clue.

  It is probably possible to just keep track of candidates,
  but I'm curious to see if we can come up with a solution that
  is also driven by searchspace of clues.

  INVARIANTS:

  - `ssSolved` and `ssCandidates` are disjoint and
    covers all cells in the puzzle.
  - every Set / IntSet in `ssClues` is non-empty, and of the same size
    if they are indexed by the same key (ClueInd)

 -}
data SolState = SolState
  { ssSolved :: M.Map Coord Int
  , ssCandidates :: M.Map Coord DS.DigitSet
  , ssClues :: M.Map ClueInd (S.Set Coord, [DS.DigitSet])
  , ssCoordToClue :: M.Map Coord [ClueInd]
  }
  deriving (Show, Eq)
