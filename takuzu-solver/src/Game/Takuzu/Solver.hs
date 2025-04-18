{-# LANGUAGE RecordWildCards #-}

module Game.Takuzu.Solver where

import Control.Monad
import Data.Ix
import Data.Maybe
import Data.Monoid
import System.Console.Terminfo

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
data Board = Board
  { bdLen :: Int
  , bdTodos :: S.Set Coord
  -- ^ n, total length of the board.
  , bdCells :: V.Vector (Maybe Cell)
  -- ^ coords of those not yet filled cells.
  , bdRowCandidates :: V.Vector (S.Set CompleteLine)
  -- ^ vector of size n * n, use Data.Ix for indexing.
  , bdColCandidates :: V.Vector (S.Set CompleteLine)
  -- ^ candidates that can be filled to that row, size=n
  }

-- \| same as bdRowCandidates but for columns.

getCell :: Board -> Coord -> Maybe Cell
getCell Board {bdLen, bdCells} coord = bdCells V.! toFlatInd coord
  where
    toFlatInd = index ((0, 0), (bdLen - 1, bdLen - 1))

allCoords :: Board -> [Coord]
allCoords Board {bdLen} = [(r, c) | r <- indices, c <- indices]
  where
    indices = [0 .. bdLen - 1]

mkEmptyBoard :: Int -> Board
mkEmptyBoard halfN = bd
  where
    bd = Board {..}
    bdLen = halfN * 2
    bdTodos = S.fromList (allCoords bd)
    bdCells = V.fromListN (bdLen * bdLen) (repeat Nothing)
    tbl = S.fromList (mkTable halfN)
    bdRowCandidates = V.fromListN bdLen (repeat tbl)
    bdColCandidates = V.fromListN bdLen (repeat tbl)

-- TODO: reduce duplication

-- Update a unknown cell in the board while still keep board fields valid.
updateCell :: Coord -> Cell -> Board -> Maybe Board
updateCell coord@(row, col) cVal bd@Board {..} = do
  let ind = index ((0, 0), (bdLen - 1, bdLen - 1)) coord
      indices = [0 .. bdLen - 1]
      rowCoords = [(row, c) | c <- indices]
      colCoords = [(r, col) | r <- indices]
      bdCells' = bdCells V.// [(ind, Just cVal)]
      getCompleteLine :: [Maybe Cell] -> Maybe CompleteLine
      getCompleteLine = fmap (VU.fromListN bdLen) . sequence
      -- eliminate candidate of the current line.
      rowCandidate = S.filter (\ln -> ln VU.! col == cVal) (bdRowCandidates V.! row)
      colCandidate = S.filter (\ln -> ln VU.! row == cVal) (bdColCandidates V.! col)
      -- TODO: we can totally avoid vector creation by extracting CompleteLine from candidate.
      rowComplete = getCompleteLine (fmap (getCell bd') rowCoords)
      colComplete = getCompleteLine (fmap (getCell bd') colCoords)
      bdRowCandidates' = V.imap upd bdRowCandidates
        where
          upd r cs =
            if r == row
              then rowCandidate
              else -- Eliminate current line from other lines if current line is complete
              case rowComplete of
                Nothing -> cs
                Just cl -> S.delete cl cs
      bdColCandidates' = V.imap upd bdColCandidates
        where
          upd c cs =
            if c == col
              then colCandidate
              else case colComplete of
                Nothing -> cs
                Just cl -> S.delete cl cs
      bd' =
        bd
          { bdTodos = S.delete coord bdTodos
          , bdCells = bdCells'
          , bdRowCandidates = bdRowCandidates'
          , bdColCandidates = bdColCandidates'
          }
  guard $ coord `S.member` bdTodos
  guard $ V.all (not . S.null) bdRowCandidates'
  guard $ V.all (not . S.null) bdColCandidates'
  pure bd'

{-
  Total number of valid cell placements in a single line
  can be calculated following https://oeis.org/A177790

  "Digits" are generated backwards since that's the most efficient way,
  this does not effect correctness given this problem's symmetric nature.
 -}
genLineAux :: Int -> Int -> [Bool] -> [] [Bool]
genLineAux 0 0 xs = [xs]
genLineAux rCount bCount xs = case xs of
  x0 : x1 : _
    | x0 == x1 ->
        if x0
          then newR
          else newB
  _ -> newB <> newR
  where
    newB = do
      True <- [bCount > 0]
      genLineAux rCount (bCount - 1) (cRed : xs)
    newR = do
      True <- [rCount > 0]
      genLineAux (rCount - 1) bCount (cBlue : xs)

mkTable :: Int -> [] CompleteLine
mkTable n = VU.fromListN (n + n) <$> genLineAux n n []

-- extract common features from lines.
-- input must be non-empty, and all elements are assumed to have the same length.
summarizeLines :: [] CompleteLine -> [] (S.Set Cell)
summarizeLines ls = extractInd <$> [0 .. size - 1]
  where
    extractInd :: Int -> S.Set Cell
    extractInd i = S.fromList ((VU.! i) <$> ls)
    size = VU.length (head ls)

mkBoard :: Int -> [[Maybe Cell]] -> Maybe Board
mkBoard halfN rawMatPre =
  foldM go bdInit (zip coords (concat rawMat))
  where
    bdInit = mkEmptyBoard halfN
    coords = allCoords bdInit
    n = halfN * 2
    go bd (coord, mCell) = case mCell of
      Nothing -> pure bd
      Just cVal -> updateCell coord cVal bd
    -- making it n x n, filling in Nothing.
    rawMat =
      take n $
        fmap (take n . (<> repeat Nothing)) rawMatPre
          <> repeat (replicate n Nothing)

-- Improve a specific row by applying summarizeLines on it.
improveRowAux :: Int -> Board -> Board
improveRowAux row bd@Board {..} = bd'
  where
    summarized :: [] (Coord, S.Set Cell)
    summarized =
      zip
        [(row, c) | c <- [0 .. bdLen - 1]]
        $ summarizeLines
        $ S.toList
        $ bdRowCandidates V.! row
    bd' = foldl go bd summarized
      where
        go :: Board -> (Coord, S.Set Cell) -> Board
        go curBd (coord, cs)
          | [val] <- S.elems cs = fromMaybe curBd (updateCell coord val curBd)
          | otherwise = curBd

improveColAux :: Int -> Board -> Board
improveColAux col bd@Board {..} = bd'
  where
    summarized :: [] (Coord, S.Set Cell)
    summarized =
      zip
        [(r, col) | r <- [0 .. bdLen - 1]]
        $ summarizeLines
        $ S.toList
        $ bdColCandidates V.! col
    bd' = foldl go bd summarized
      where
        go :: Board -> (Coord, S.Set Cell) -> Board
        go curBd (coord, cs)
          | [val] <- S.elems cs = fromMaybe curBd (updateCell coord val curBd)
          | otherwise = curBd

-- as the candidate can only be eliminated but never added, we can tell whether
-- a Board is actually be updated by looking at whether candidateCount changes.
candidateCount :: Board -> Int
candidateCount Board {..} =
  getSum $ collect bdRowCandidates <> collect bdColCandidates
  where
    collect :: V.Vector (S.Set CompleteLine) -> Sum Int
    collect = foldMap (Sum . S.size)

-- Extract all incomplete rows and cols, and try update lines in that order.
-- for now update order is not very specific and only guarantees completeness.
-- in other words, if there's a line that can be updated, a single round of tryImprove
-- should at least reduce the amount of candidates.
tryImprove :: Board -> Maybe Board
tryImprove bd = do
  let todoRows = S.toList $ S.map fst (bdTodos bd)
      todoCols = S.toList $ S.map snd (bdTodos bd)
      bd' =
        foldr
          improveColAux
          (foldr improveRowAux bd todoRows)
          todoCols
  guard $ candidateCount bd /= candidateCount bd' || bdTodos bd /= bdTodos bd'
  pure bd'

-- improve a Board repeated until it cannot be improved further
-- For a board that has single solution, we should be able to find the solution this way,
-- if not, there might be some bug in the algorithm.
trySolve :: Board -> Board
trySolve bd = maybe bd trySolve (tryImprove bd)

toSolution :: Board -> Maybe [[Cell]]
toSolution bd@Board {bdLen} = do
  let indices = [0 .. bdLen - 1]
  forM indices $ \row ->
    forM indices $ \col ->
      getCell bd (row, col)

solveBoard :: Int -> [[Maybe Cell]] -> Maybe [[Cell]]
solveBoard sz bdInpRaw = do
  (halfL, 0) <- pure $ sz `quotRem` 2
  bdInp <- mkBoard halfL bdInpRaw
  toSolution $ trySolve bdInp

genMoves :: Board -> Board -> [(Coord, Cell)]
genMoves bdOrig bdAfter = concatMap toMove [(r, c) | r <- [0 .. l - 1], c <- [0 .. l - 1]]
  where
    l = bdLen bdOrig
    toMove coord = case (getCell bdOrig coord, getCell bdAfter coord) of
      (Just _, _) -> []
      (Nothing, Just b) -> [(coord, b)]
      _ -> []

boardToInput :: Board -> [String]
boardToInput bd =
  "br 12" : bdLines
  where
    -- since we are only dealing with 12x12 puzzles.
    bdLines = map mkBdLine [0 .. 11]
    mkBdLine row = map (\col -> tr $ getCell bd (row, col)) [0 .. 11]
      where
        tr Nothing = ' '
        tr (Just b) = if b == cRed then 'r' else 'b'

pprBoard :: Terminal -> Board -> IO ()
pprBoard term bd@Board {..} = do
  putStrLn $ "Side length: " <> show bdLen
  putStrLn $ "Pending cells: " <> show (S.size bdTodos)
  putStrLn $ "Row candidate counts: " <> show (V.map S.size bdRowCandidates)
  putStrLn $ "Col candidate counts: " <> show (V.map S.size bdColCandidates)
  putStrLn "++++ Board Begin ++++"
  forM_ [0 .. bdLen - 1] $ \r -> do
    forM_ [0 .. bdLen - 1] $ \c -> do
      let cell = getCell bd (r, c)
      case cell of
        Nothing -> putStr " "
        Just b ->
          -- NOTE: the fancy, colorful output can be disabled by setting "TERM="
          -- e.g. "TERM= stack exec -- demo" (note the space after TERM=)
          case getCapability term withForegroundColor of
            Nothing -> putStr $ if b then "1" else "0"
            Just useColor ->
              let color = if b then Red else Blue
                  rendered = termText "█"
               in runTermOutput term (useColor color rendered)
    putStrLn ""
  putStrLn "---- Board End ----"

translateRaw :: [[Char]] -> [[Maybe Cell]]
translateRaw = (fmap . fmap) tr
  where
    tr ' ' = Nothing
    tr 'r' = Just cRed
    tr 'b' = Just cBlue
    tr _ = undefined
