{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  #-}
module Game.Takuzu.Solver where

import Control.Monad
import Data.Ix
import Data.Maybe
import Data.Monoid

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

type Cell = Bool

cBlue, cRed :: Cell
[cBlue, cRed] = [False, True]

-- NOTE: more compact representation can be achieved through a bit vector impl,
-- or just Word64 or something shorter
-- since we've never planned to solve a large scale problem
-- by preprocessing a large table.
-- but for now let's focus on correctness first.
type CompleteLine = VU.Vector Cell

type Coord = (Int, Int) -- (row, col), 0-based

{-
  bdXXXCandidates should be updated appropriately whenever a cell is set.
  summarizeLines might not have any effect on the row/col executing it, but
  as the cell narrows down to a fixed value, that col/row might start to have fewer candidates.
  meaning that we should have a primitive for updating a single cell to a fixed value (i.e. from Nothing to Just _)
  also we should also update bdXXXCandidates when a line is fully completed - candidates other that that row/col
  now needs to exclude that specific CompleteLine following game rule.
 -}
data Board vec
  = Board
  { bdLen :: Int -- | n, total length of the board.
  , bdToFlatInd :: Coord -> Int -- | function to convert from coordinate to to linear index
  , bdTodos :: S.Set Coord -- | coords of those not yet filled cells.
  , bdCells :: vec (Maybe Cell) -- | vector of size n * n, use Data.Ix for indexing.
  , bdRowCandidates :: vec (S.Set CompleteLine) -- | candidates that can be filled to that row, size=n
  , bdColCandidates :: vec (S.Set CompleteLine) -- | same as bdRowCandidates but for columns.
  }

mkEmptyBoard :: Int -> Board V.Vector
mkEmptyBoard halfN = Board {..}
  where
    bdLen = halfN * 2
    bdToFlatInd = index ((0,0), (bdLen-1,bdLen-1))
    indices = [0..bdLen-1]
    bdTodos = S.fromList [(r,c) | r <- indices, c <- indices]
    bdCells = V.fromListN (bdLen * bdLen) (repeat Nothing)
    tbl = S.fromList (mkTable halfN)
    bdRowCandidates = V.fromListN bdLen (repeat tbl)
    bdColCandidates = V.fromListN bdLen (repeat tbl)

-- TODO: reduce duplication

-- Update a unknown cell in the board while still keep board fields valid.
updateCell :: Coord -> Cell -> Board V.Vector -> Maybe (Board V.Vector)
updateCell coord@(row,col) cVal bd@Board{..} = do
  let ind = bdToFlatInd coord
      indices = [0 .. bdLen-1]
      rowCoords = [(row,c) | c <- indices]
      colCoords = [(r,col) | r <- indices]
      bdCells' = bdCells V.// [(ind, Just cVal)]
      getCompleteLine :: [Maybe Cell] -> Maybe CompleteLine
      getCompleteLine = fmap (VU.fromListN bdLen) . sequence
      -- eliminate candidate of the current line.
      rowCandidate = S.filter (\ln -> ln VU.! col == cVal) (bdRowCandidates V.! row)
      colCandidate = S.filter (\ln -> ln VU.! row == cVal) (bdColCandidates V.! col)
      -- TODO: we can totally avoid vector creation by extracting CompleteLine from candidate.
      rowComplete = getCompleteLine (fmap ((bdCells' V.!) . bdToFlatInd) rowCoords)
      colComplete = getCompleteLine (fmap ((bdCells' V.!) . bdToFlatInd) colCoords)
      bdRowCandidates' = V.imap upd bdRowCandidates
        where
          upd r cs =
            if r == row
              then rowCandidate
              else
                -- Eliminate current line from other lines if current line is complete
                case rowComplete of
                  Nothing -> cs
                  Just cl -> S.delete cl cs
      bdColCandidates' = V.imap upd bdColCandidates
        where
          upd c cs =
            if c == col
              then colCandidate
              else
                case colComplete of
                  Nothing -> cs
                  Just cl -> S.delete cl cs
  guard $ coord `S.member` bdTodos
  guard $ V.all (not . S.null) bdRowCandidates'
  guard $ V.all (not . S.null) bdColCandidates'
  pure bd
    { bdTodos = S.delete coord bdTodos
    , bdCells = bdCells'
    , bdRowCandidates = bdRowCandidates'
    , bdColCandidates = bdColCandidates'
    }


{-
  Total number of valid cell placements in a single line
  can be calculated following https://oeis.org/A177790

  "Digits" are generated backwards since that's the most efficient way,
  this does not effect correctness given this problem's symmetric nature.
 -}
genLineAux :: Int -> Int -> [Bool] -> [] [Bool]
genLineAux 0 0 xs = [xs]
genLineAux rCount bCount xs = case xs of
    x0:x1:_
      | x0 == x1 ->
        if x0
          then newR
          else newB
    _ -> newB <> newR
  where
    newB = do
      True <- [bCount > 0]
      genLineAux rCount (bCount-1) (cRed : xs)
    newR = do
      True <- [rCount > 0]
      genLineAux (rCount-1) bCount (cBlue : xs)

mkTable :: Int -> [] CompleteLine
mkTable n = VU.fromListN (n+n) <$> genLineAux n n []

-- extract common features from lines.
-- input must be non-empty, and all elements are assumed to have the same length.
summarizeLines :: [] CompleteLine -> [] (S.Set Cell)
summarizeLines ls = extractInd <$> [0 .. size-1]
  where
    extractInd :: Int -> S.Set Cell
    extractInd i = S.fromList ((VU.! i) <$> ls)
    size = VU.length (head ls)

mkBoard :: Int -> [[Maybe Cell]] -> Maybe (Board V.Vector)
mkBoard halfN rawMatPre =
    foldM go (mkEmptyBoard halfN) (zip [(r,c) | r <- indices, c <- indices] (concat rawMat))
  where
    n = halfN * 2
    go bd (coord, mCell) = case mCell of
      Nothing -> pure bd
      Just cVal -> updateCell coord cVal bd
    indices = [0..n-1]
    -- making it n x n, filling in Nothing.
    rawMat =
      take n $
        fmap (take n . (<> repeat Nothing)) rawMatPre
        <> repeat (replicate n Nothing)

-- Improve a specific row by applying summarizeLines on it.
improveRowAux :: Int -> Board V.Vector -> Board V.Vector
improveRowAux row bd@Board{..} = bd'
  where
    summarized :: [] (Coord, S.Set Cell)
    summarized =
        zip
          [(row, c) | c <- [0..bdLen-1]] $
          summarizeLines $ S.toList $ bdRowCandidates V.! row
    bd' = foldl go bd summarized
      where
        go :: Board V.Vector -> (Coord, S.Set Cell) -> Board V.Vector
        go curBd (coord, cs)
          | [val] <- S.elems cs = fromMaybe curBd (updateCell coord val curBd)
          | otherwise = curBd

improveColAux :: Int -> Board V.Vector -> Board V.Vector
improveColAux col bd@Board{..} = bd'
  where
    summarized :: [] (Coord, S.Set Cell)
    summarized =
        zip
          [(r, col) | r <- [0..bdLen-1]] $
          summarizeLines $ S.toList $ bdColCandidates V.! col
    bd' = foldl go bd summarized
      where
        go :: Board V.Vector -> (Coord, S.Set Cell) -> Board V.Vector
        go curBd (coord, cs)
          | [val] <- S.elems cs = fromMaybe curBd (updateCell coord val curBd)
          | otherwise = curBd

-- as the candidate can only be eliminated but never added, we can tell whether
-- a Board is actually be updated by looking at whether candidateCount changes.
candidateCount :: Board V.Vector -> Int
candidateCount Board{..} =
    getSum $ collect bdRowCandidates <> collect bdColCandidates
  where
    collect :: V.Vector (S.Set CompleteLine) -> Sum Int
    collect = foldMap (Sum . S.size)

-- Extract all incomplete rows and cols, and try update lines in that order.
-- for now update order is not very specific and only guarantees completeness.
-- in other words, if there's a line that can be updated, a single round of tryImprove
-- should at least reduce the amount of candidates.
tryImprove :: Board V.Vector -> Maybe (Board V.Vector)
tryImprove bd = do
  let todoRows = S.toList $ S.map fst (bdTodos bd)
      todoCols = S.toList $ S.map snd (bdTodos bd)
      bd' =
        foldr improveColAux
          (foldr improveRowAux bd todoRows)
          todoCols
  guard $ candidateCount bd /= candidateCount bd' || bdTodos bd /= bdTodos bd'
  pure bd'

-- improve a Board repeated until it cannot be improved further
-- For a board that has single solution, we should be able to find the solution this way,
-- if not, there might be some bug in the algorithm.
trySolve :: Board V.Vector -> Board V.Vector
trySolve bd = maybe bd trySolve (tryImprove bd)
