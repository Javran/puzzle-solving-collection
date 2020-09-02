{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game.Tents.Solver where

import Control.Monad
import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Vector as V
import Game.Tents.Types
import System.Console.Terminfo

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

Note that it is guaranteed that bdDims, bd{Row,Col}TentCounts never change after construction.

 -}
data Board = Board
  { bdDims :: (Int, Int) -- rows, cols
  , bdRowTentCounts :: V.Vector Int
  , bdColTentCounts :: V.Vector Int
  , bdCells :: M.Map Coord Cell
  , -- transformed from brRowTentCounts + brColTentcounts,
    -- every unsatisfied row or column is supposed to have one entity here.
    bdTodoCandidates :: [Candidates]
  , -- all trees whose tent assignment is not yet determined.
    bdTodoTrees :: M.Map Coord [Coord]
  }

-- TODO: verify that boards with impossible candidates result in Nothing.
mkBoard :: BoardRep -> Maybe Board
mkBoard
  BoardRep
    { brDims = bdDims@(rows, cols)
    , brBoard
    , brRowTentCounts = bdRowTentCounts
    , brColTentCounts = bdColTentCounts
    } = do
    let allCoords = S.fromList [(r, c) | r <- [0 .. rows -1], c <- [0 .. cols -1]]
        coordIsInside (r, c) = r >= 0 && r < rows && c >= 0 && c < cols
        missingCoords = allCoords `S.difference` M.keysSet brBoard
        allTrees = M.keysSet $ M.filter (== Tree) brBoard
        simpleEmptyCoords = S.toList $ S.filter (not . nearTrees) missingCoords
          where
            nearTrees c = any (\dir -> applyDir dir c `S.member` allTrees) allDirs
        bdCells = brBoard `M.union` M.fromList ((,Empty) <$> simpleEmptyCoords)
        genRowOrColCandidates coordss rowOrColTentCounts = do
          let result = zipWith go coordss rowOrColTentCounts
          -- ensure that every `Candidates` is not empty.
          guard $ all (not . null) result
          pure result
          where
            go coords count =
              fmap
                (M.filterWithKey (\coord _ -> isUnknown coord)
                   . M.fromList
                   . zip coords)
                filledLine
              :: Candidates
              where
                isUnknown coord = bdCells M.!? coord == Nothing
                filledLine = fillLine count $ fmap (bdCells M.!?) coords
    todoRowCandidates <-
      genRowOrColCandidates
        [[(r, c) | c <- [0 .. cols -1]] | r <- [0 ..]]
        (V.toList bdRowTentCounts)
    todoColCandidates <-
      genRowOrColCandidates
        [[(r, c) | r <- [0 .. rows -1]] | c <- [0 ..]]
        (V.toList bdColTentCounts)
    bdTodoTrees <- do
      let candidateTentCoords :: Coord -> Maybe [Coord]
          candidateTentCoords coord = do
            let result =
                  filter
                    (\coord' ->
                       let r = bdCells M.!? coord'
                        in r == Nothing || r == Just Tent)
                    . filter coordIsInside
                    $ fmap (\d -> applyDir d coord) allDirs
            guard $ not . null $ result
            pure result
      M.fromList
        <$> (forM (S.toList allTrees) $ \tCoord ->
               (tCoord,) <$> candidateTentCoords tCoord)
    -- TOOD: tentRepel is not performed on this initial board.
    pure
      Board
        { bdDims
        , bdRowTentCounts
        , bdColTentCounts
        , bdCells
        , bdTodoCandidates =
            -- TODO: investigate whether it is desirable
            -- to merge candidates when their sets of coordinates are exactly the same.
            -- this might or might not happen when putting row and col candidates together.
            todoRowCandidates <> todoColCandidates
        , bdTodoTrees
        }

{-
  Given a line (row or col) of incomplete board,
  complete that line nondeterministically.

  Line completion is done by replacing all Nothings with either Empty or Tent
  so that resulting line has # of tents specified
  and no two tents are adjacent to each other.
 -}
fillLine :: Int -> [Maybe Cell] -> [] [Cell]
fillLine tentCount = fillLineAux tentCount Empty []
  where
    {-
      fillLineAux n prevCell revAcc xs
      - n: # of tents required
      - prevCell
      - revAcc: accumulated result, in reverse order
      - xs: remaining current line
     -}
    fillLineAux n prevCell revAcc xs = case xs of
      [] ->
        -- end of list, verify, reverse and return.
        [reverse revAcc | n == 0]
      hd : tl -> case hd of
        Just v -> do
          let n' = if v == Tent then n -1 else n
          guard $ n' >= 0
          fillLineAux n' v (v : revAcc) tl
        Nothing ->
          -- it is always possible to place an empty one.
          fillLineAux n Empty (Empty : revAcc) tl
            -- when previous cell is a Tent, we are forced to place an Empty cell,
            -- otherwise two Tent will be adjacent to each other.
            -- when previous cell is Tree or Empty, next one is free to pick from Tent or Empty.
            <> do
              guard $ prevCell /= Tent
              guard $ n > 0
              fillLineAux (n -1) Tent (Tent : revAcc) tl

{-
  It is guaranteed that getCell will never access any field with Todo in name.
  We need this property so that this function can be called even during Board construction.
 -}
getCell :: Board -> Coord -> Maybe Cell
getCell Board {bdCells} coord = bdCells M.!? coord

-- TODO: to be tested
{- a tent "repels" nearby Nothings, making them all Empty -}
tentRepel :: Board -> Coord -> Maybe Board
tentRepel bd coord@(r, c) = case getCell bd coord of
  Just Tent -> do
    let Board {bdDims = (rows, cols), bdCells} = bd
        coordsToEmpty = do
          r' <- [0 .. rows -1]
          c' <- [0 .. cols -1]
          let coord' = (r', c')
          guard $ abs (r - r') <= 1 && abs (c - c') <= 1
          guard $ getCell bd coord' == Nothing
          pure coord'
        bdCells' = M.union (M.fromList $ fmap (,Empty) coordsToEmpty) bdCells
        bdResult = bd {bdCells = bdCells'}
    foldM tidyBoard bdResult coordsToEmpty
  _ -> pure bd

{-
  remove candidates that are inconsistent with one particular cell of the board.
 -}
tidyBoard :: Board -> Coord -> Maybe Board
tidyBoard bd coord = case getCell bd coord of
  Nothing -> Just bd
  Just Tree -> Just bd
  Just cell -> do
    -- here cell is either Empty or Tent
    let Board {bdTodoCandidates, bdTodoTrees} = bd
        updateCandidates :: Candidates -> Maybe Candidates
        updateCandidates cs = do
          -- must be non-empty
          (p : _) <- pure cs
          -- only need to examine one piece as the key set is shared in a single `Candidates`.
          case p M.!? coord of
            Nothing -> Just cs
            Just _ -> do
              let updatePiece :: Piece -> Maybe Piece
                  updatePiece pc =
                    if pc M.! coord == cell
                      then -- remove record of that cell since this value is now set.
                        Just $ M.delete coord p
                      else -- remove this piece from candidate as it is no longer consistent.
                        Nothing
              -- if we have filtered out all candidates, this board is impossible to solve.
              -- TODO: this is not an efficient way to de-dup.
              -- TODO: find out how does duplication show up in the first place?
              cs'@(_ : _) <- pure $ nub $ mapMaybe updatePiece cs
              pure cs'
        bdTodoTrees' :: M.Map Coord [Coord]
        bdTodoTrees' =
          if cell == Empty
            then -- an Empty cell cannot be paired with a tree.
              M.map (delete coord) bdTodoTrees
            else bdTodoTrees
        satCandidates = all M.null -- TODO: dedup after discharging coord from pieces.
    bdTodoCandidates' <-
      filter (not . satCandidates)
        <$> mapM updateCandidates bdTodoCandidates
    guard $ all (not . null) bdTodoTrees'
    pure $ bd {bdTodoCandidates = bdTodoCandidates', bdTodoTrees = bdTodoTrees'}

{-
  set coord to a cell value, internal use only (for now),
  since we don't have much check on things on this function.
 -}
setCoordInternal :: Coord -> Cell -> Board -> Maybe Board
setCoordInternal coord cell bd@Board {bdCells} = do
  bd' <- tidyBoard bd {bdCells = M.insert coord cell bdCells} coord
  case cell of
    Tent -> tentRepel bd' coord
    _ -> pure bd'

fillPiece :: Piece -> Board -> Maybe Board
fillPiece p bd = do
  let Board {bdCells} = bd
      (pExisting, pNew) = M.partitionWithKey (\coord _ -> M.member coord bdCells) p
  -- ideally pExisting should be empty, but if there is any kind of overlap,
  -- bdCells must agree.
  guard $ pExisting == M.restrictKeys bdCells (M.keysSet pExisting)
  foldM (\curBd (coord, cell) -> setCoordInternal coord cell curBd) bd (M.toList pNew)

pprBoard :: Terminal -> Board -> IO ()
pprBoard
  term
  bd@Board
    { bdDims
    , bdTodoTrees
    , bdRowTentCounts
    , bdColTentCounts
    , bdTodoCandidates
    } = do
    let (rows, cols) = bdDims
        (fg, bg) =
          fromMaybe (const id, const id) $
            (,)
              <$> getCapability term (withForegroundColor @TermOutput)
              <*> getCapability term (withBackgroundColor @TermOutput)
    putStrLn $ "(rows,cols): " <> show bdDims
    let renderCell getCell' (r, c) =
          case getCell' (r, c) of
            Nothing -> termText "?"
            Just Empty -> bg White $ termText " "
            Just Tree -> bg Green $ fg White $ termText "R"
            Just Tent -> bg Blue $ fg White $ termText "E"

    forM_ [0 .. rows -1] $ \r -> do
      let rendered =
            foldMap (renderCell (getCell bd) . (r,)) [0 .. cols -1]
              <> termText (show (bdRowTentCounts V.! r))
              <> termText "\n"
      runTermOutput term rendered
    let tr n =
          -- let's not worry about what to do when n > 9 for now, it's very rare.
          if n > 9 then "-" else show n
    putStrLn (concatMap tr $ V.toList bdColTentCounts)
    putStrLn "Tree candidates counts:"
    forM_ [0 .. rows -1] $ \r -> do
      forM_ [0 .. cols -1] $ \c ->
        putStr $
          case getCell bd (r, c) of
            Nothing -> "?"
            Just Empty -> " "
            Just Tree ->
              case bdTodoTrees M.!? (r, c) of
                Nothing ->
                  -- this can only happen when the tree is question
                  -- is considered solved therefore discharged.
                  "!"
                Just v -> show (length v)
            Just Tent -> "E"
      putStrLn ""
    forM_ (zip [0 :: Int ..] bdTodoCandidates) $ \(idx, cs) -> do
      let coords =
            -- the invariant is that pieces in `cs` share the same set of coordinates.
            case cs of
              v : _ | v' <- M.keys v, not . null $ v' -> v'
              _ -> error "invalid: candidates or pieces cannot be empty"
          ( (Just (Min minRow), Just (Max maxRow))
            , (Just (Min minCol), Just (Max maxCol))
            ) =
              foldMap (let f v = (Just (Min v), Just (Max v)) in bimap f f) coords

      putStrLn $
        show idx <> ": " <> show (length cs) <> " pieces, with range: "
          <> show (minRow, minCol)
          <> "-"
          <> show (maxRow, maxCol)
      forM_ cs $ \p -> do
        if minRow == maxRow
          then do
            forM_ [minRow .. maxRow] $ \r -> do
              let rendered =
                    termText "  "
                      <> foldMap (renderCell (\coord -> p M.!? coord) . (r,)) [minCol .. maxCol]
                      <> termText "\n"
              runTermOutput term rendered
            putStrLn ""
          else do
            -- transpose before printing out to make it easier to compare candidates against each other.
            putStrLn "  (transposed)"
            forM_ [minCol .. maxCol] $ \c -> do
              let rendered =
                    termText "  "
                      <> foldMap (renderCell (\coord -> p M.!? coord) . (,c)) [minRow .. maxRow]
                      <> termText "\n"
              runTermOutput term rendered
            putStrLn ""

{-
Few tactics we can implement:

- a tent "repels" nearby Nothings, making them all Empty
- look at a single candidate and find commit assignments and set them to board.
- DFS starting from a set of least populated candidates, and extract what is common.
  (this process will require we have a way to "settle" current Board
  into either Nothing or a consistent state)

primitive / basic operations:

- tidyBoard :: Board -> Coord -> Maybe Board, this function looks
  at a specifc coordinate of the board and figure out if it is possible
  to fill in the value. if so, that value will be filled in with related
  candidate structures removed from the board.

 -}
