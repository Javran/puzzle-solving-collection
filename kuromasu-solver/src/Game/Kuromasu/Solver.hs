{-# LANGUAGE
    NamedFieldPuns
  , TypeApplications
  #-}
module Game.Kuromasu.Solver
  ( Cell, Coord, HintMap, ColorMap
  , cBlue, cRed
  , Board(..)
  , bdGet
  , mkBoard
  , updateCell
  , solve
  , solveAndShow
  , pprBoard
  , extractAnswer
  ) where

import Control.Monad
import Data.Ix
import Data.List
import Data.Maybe
import Data.MemoTrie
import System.Console.Terminfo

import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Cell = Bool

cBlue, cRed :: Cell
[cBlue, cRed] = [False, True]

type Coord = (Int, Int) -- (<row>, <col>), 0-based.

type Candidate = M.Map Coord Cell

data Board
  = Board
    { bdDims :: (Int, Int) -- (rows, cols)
    , bdTodos :: !(S.Set Coord) -- not yet filled cells
    , bdCells :: !(M.Map Coord Cell) -- known cells
      -- Every unsolved number cell is listed here.
      -- each of the value is a list of possible "overlaps"
      -- with the board. This allows us to:
      -- - answer the question of what's common in all possible candidates
      -- - eliminate candidates that are not possible.
    , bdCandidates :: !(M.Map Coord [M.Map Coord Cell])
    }

{-
  The following type defines a building block for building "blueprint"s given a number
  and the original coordinate.

  Assume a cell is at (0,0), Placement 1 2 3 0 will mean the following setup:

  - (0,0): blue
  - (-1,0): blue, (-2,0): red
  - (0,1) & (0,2): blue, (0,3): red
  - (1,0) & (2,0) & (3,0): blue, (4,0): red
  - (0,-1): red

  note that we have negative numbers, which is fine since we are only building the blueprint,
  but the generated candidates (i.e. sets of coord-color pairs) will be less at runtime.

  also note that there's another way of eliminating an candidate: if it's dimension is too long
  to be fit in a board (e.g. a 9x9 board can have maximum number of 16,
  but a 1x17 Placement is simply impossible to fit). This kind of elimination
  can be done when building up the blueprint.

 -}
data Placement =
  Placement !Int !Int !Int !Int {- up, right, down, left. in this order -}

{-
  pick up items in that order. one item can be pick up multiple times.
 -}
pickInOrder' :: [a] -> [] (a,[a])
pickInOrder' = fmap (\(x:xs) -> (x,x:xs)) . init . tails

precomputePlacements :: (Int, Int) -> Int -> [Placement]
precomputePlacements (rows, cols) count =
    gen initMods count (Placement 0 0 0 0)
  where
    initMods =
      [ \(Placement u r d l) -> Placement (u+1) r d l
      , \(Placement u r d l) -> Placement u (r+1) d l
      , \(Placement u r d l) -> Placement u r (d+1) l
      , \(Placement u r d l) -> Placement u r d (l+1)
      ]
    gen :: [Placement -> Placement] -> Int -> Placement -> [Placement]
    gen _ 0 cur = [cur]
    gen mods todoCount cur = do
      (f, mods') <- pickInOrder' mods
      let cur'@(Placement u r d l) = f cur
      -- TODO: knowing the max number will help reducing the amount of unnecessary processing.
      guard $ u + d < rows && l + r < cols
      gen mods' (todoCount-1) cur'

{-
  Create an empty board with candidates populated by clues.
 -}
mkBoard :: (Int, Int) -> [(Coord, Int)] -> Board
mkBoard bdDims@(rows, cols) clues = Board
    { bdDims
    , bdTodos = S.fromList [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
    , bdCells = M.empty
    , bdCandidates = M.fromList $ uncurry mkCandidate <$> clues
    }
  where
    -- memoize by count.
    -- every different count can share the initial list of candidates
    -- and eliminate some based on position and other information as the board improves.
    genMemoed = memo $ precomputePlacements (rows,cols)
    mkCandidate :: Coord -> Int -> (Coord, [M.Map Coord Cell])
    mkCandidate cCoord@(row,col) count =
        (cCoord, mapMaybe placementToCandidate ps)
      where
        -- generate initial possible placements
        -- without knowing the location of the center coord
        ps = genMemoed count -- gen bdDims mods count (Placement 0 0 0 0)
        placementToCandidate :: Placement -> Maybe (M.Map Coord Cell)
        placementToCandidate (Placement u r d l) = do
          let centerPair = (cCoord, cBlue)
              pUpCells =
                [ ((row-df, col), cBlue) | df <- [1..u]]
              pRightCells =
                [ ((row, col+df), cBlue) | df <- [1..r]]
              pDownCells =
                [ ((row+df, col), cBlue) | df <- [1..d]]
              pLeftCells =
                [ ((row, col-df), cBlue) | df <- [1..l]]
              pRedCells =
                [ (c, cRed)
                | c <-
                    [ (row-u-1, col)
                    , (row, col+r+1)
                    , (row+d+1, col)
                    , (row, col-l-1)
                    ]
                ]
              pairs = centerPair : concat [pUpCells, pRightCells, pDownCells, pLeftCells, pRedCells]
              isInRange = inRange ((0,0), (rows-1,cols-1))
              checkPair v@(coord', color)
                | isInRange coord' = Just (v:)
                | color == cRed = Just id
                | otherwise = Nothing
          Just pairk <- pure $ mapM checkPair pairs
          pure $ M.fromList $ foldr (.) id pairk []

pprBoard :: Terminal -> [(Coord, Int)] -> Board -> IO ()
pprBoard term hints Board{bdDims, bdTodos, bdCells, bdCandidates} = do
  putStrLn $ "Board dimensions: " <> show bdDims
  putStrLn "++++ Board Begin"
  let mRenderFs :: TermStr s => Maybe (Color -> s -> s, Color -> s -> s)
      mRenderFs = (,)
        <$> getCapability term withForegroundColor
        <*> getCapability term withBackgroundColor
      (rows, cols) = bdDims
  case mRenderFs @TermOutput of
    Nothing ->
      forM_ [0..rows-1] $ \r -> do
        let coordToChar coord = case bdCells M.!? coord of
              Nothing -> ' '
              Just c -> if c == cBlue then 'B' else 'R'
        putStr ((\c -> coordToChar (r,c)) <$> [0..cols-1])
    Just (fg, bg) ->
      forM_ [0..rows-1] $ \r -> do
        let ln = foldMap render [0..cols-1]
            sp = termText " "
            render :: Int -> TermOutput
            render c = case bdCells M.!? coord of
                Nothing -> sp
                Just color ->
                  if color == cRed
                    then bg Red sp
                    else bg Blue $ case lookup coord hints of
                      Nothing -> sp
                      Just v -> fg White $
                        if v > 9
                          then termText "."
                          else termText (show v)
              where
                coord = (r,c)
        runTermOutput term $ ln <> termText "\n"
  putStrLn "---- Board End"
  putStrLn $ "Todos: " <> show (length bdTodos)
  unless (M.null bdCandidates) $ do
    putStr "Candidates: "
    let candidatesDisplay =
          intercalate ", "
          . fmap (\(coord,xs) -> show coord <> ":" <> show (length xs))
          $ M.toAscList bdCandidates
    putStrLn candidatesDisplay

{-
  Basic operation on a board that fills one cell,
  simplifies candidates, and remove contradiction candidates.
 -}
updateCell :: Board -> Coord -> Cell -> Maybe Board
updateCell Board{bdDims, bdTodos, bdCells, bdCandidates} coord color = do
  guard $ coord `S.member` bdTodos
  let bdTodos' = S.delete coord bdTodos
      bdCells' = M.insert coord color bdCells
      -- check current candidate, simplify or remove it
      checkAndElim :: Candidate -> Maybe Candidate
      checkAndElim cs = case cs M.!? coord of
        Nothing -> Just cs -- coord have nothing to do with this candidate, move on.
        Just c -> do
          -- color must not contradict.
          guard $ c == color
          -- remove this coord from candidate list.
          -- this removal is not necessary but it reduces the amount of cells we need to visit for each update.
          pure $! M.delete coord cs
      bdCandidates' =
        M.filter (not . all M.null)
        . M.map (mapMaybe checkAndElim)
        $ bdCandidates
  guard $ all (not . null) bdCandidates'
  pure Board
    { bdDims
    , bdTodos = bdTodos'
    , bdCells = bdCells'
    , bdCandidates = bdCandidates'
    }

{-
  Try to improve current board by eliminating candidates based on a specific hint.
  Hints are indexed by their coordinates.
 -}
improve :: Board -> Coord -> Maybe Board
improve bd@Board{bdCandidates} coord = do
  cs <- bdCandidates M.!? coord
  let doMerge =
        M.merge
          M.dropMissing
          M.dropMissing
          (M.zipWithMaybeAMatched $
           \_k l r -> pure $ if l == r then Just l else Nothing)
      commons =
        concatMap (\(k, mv) -> case mv of
                      Nothing -> []
                      Just v -> [(k,v)]
                  )
        . M.toList
        -- note that cs shouldn't be empty if the result comes from "updateCell",
        -- therefore the use of foldl1 is safe.
        . foldl1 doMerge
        . (fmap . M.map) Just
        $ cs
  guard $ not . null $ commons
  foldM (\curBd (coord',cell) -> updateCell curBd coord' cell) bd commons

improveStep :: Board -> Maybe Board
improveStep bd@Board{bdCandidates} = do
  let candidateCounts = sortOn (length . snd) $ M.toList bdCandidates
      bds = mapMaybe (improve bd . fst) candidateCounts
  -- choose first successful improvement
  (bd':_) <- pure bds
  pure bd'

bdGet :: Board -> Coord -> Maybe Cell
bdGet Board{bdCells, bdDims = (rows, cols)} coord =
  if inRange ((0,0), (rows-1,cols-1)) coord
    then bdCells M.!? coord
    else Just cRed

{-
  The final step is to fill in red if that cell is surrounded by red.
  If the solution is guaranteed to be unique, this step hopefully will fill in all remaining blanks.
 -}
finalStep :: Board -> Board
finalStep bd@Board{bdTodos} =
    foldr go bd bdTodos
  where
    go coord curBd =
      if surroundedByRed curBd coord
        then fromMaybe curBd (updateCell curBd coord cRed)
        else curBd
    surroundedByRed curBd (r,c) =
      all
        (\coord' -> bdGet curBd coord' == Just cRed)
        [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

solve :: Board -> Board
solve bd =
  case improveStep bd of
    Just bd' -> solve bd'
    Nothing -> finalStep bd

type ColorMap = [(Coord, Cell)]
type HintMap = [(Coord, Int)]

solveAndShow :: Terminal -> Board -> HintMap -> IO ()
solveAndShow term bd hints = do
  pprBoard term hints bd
  pprBoard term hints (solve bd)

{-
  Extracts answer from current board.
  This will only succeed when the board is solved.
  Note that this function only checks input board for shape:
  the way our algorithm works should guarantee that
  all coloring are done following game rules.

  We rely on unit tests to make sure that all outputs
  are indeed following game rules.
 -}
extractAnswer :: Board -> Maybe [[Cell]]
extractAnswer bd@Board{bdDims, bdTodos} = do
  guard $ S.null bdTodos
  let (rows, cols) = bdDims
      coords = [ [ (r,c) | c <- [0..cols-1] ] | r <- [0 .. rows-1] ]
  (mapM . mapM) (bdGet bd) coords
