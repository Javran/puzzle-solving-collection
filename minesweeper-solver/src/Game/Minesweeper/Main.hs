{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Minesweeper.Main
  ( main,
  )
where

import Control.Monad
import Control.Monad.Writer.Strict
import qualified Data.DList as DL
import Data.Ix
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import Game.Minesweeper.Parser
import System.Environment

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
    -- note that one should avoid querying on this directly,
    -- use getTile to handle out-of-bound coords properly.
    bdMines :: MineMap,
    bdNums :: M.Map Coord Int, -- number tiles.
    -- possible ways of arranging mines so that the number tile (key) is satisfied.
    -- note that satisfied MineCoords would have some common coords discharged.
    bdCandidates :: M.Map Coord [MineCoords]
  }
  deriving (Show)

getTile :: Board -> Coord -> Maybe Bool
getTile Board {bdDims = (rows, cols), bdMines} coord =
  if inRange ((0, 0), (rows -1, cols -1)) coord
    then bdMines M.!? coord
    else Just False

-- check a candidate against current board to ensure consistency
checkCandidate :: Board -> Coord -> MineCoords -> Bool
checkCandidate bd (x, y) cs =
  -- note that it is unnecessary to check whether current tile is a mine.
  -- this is because number tiles are (will be) explicitly marked as non-mine
  -- during Board construction.
  all check coords
  where
    coords = (\(dx, dy) -> (x + dx, y + dy)) <$> surroundings
    check :: Coord -> Bool
    check curCoord =
      case (getTile bd curCoord, cs M.!? curCoord) of
        (Nothing, _) -> True
        (_, Nothing) -> True
        (Just actual, Just expect) -> actual == expect

eliminateCommon :: Coord -> [MineCoords] -> ([(Coord, Bool)], [MineCoords])
eliminateCommon _ [] = ([], [])
eliminateCommon (x, y) ms@(t : ts) = (commons, fmap (`M.withoutKeys` commonKeys) ms)
  where
    commons = mapMaybe ((\c -> (c,) <$> isCommon c) . anchor) surroundings
      where
        anchor (dx, dy) = (x + dx, y + dy)
    commonKeys = S.fromList $ fmap fst commons
    isCommon :: Coord -> Maybe Bool
    isCommon c = do
      val <- t M.!? c
      vals <- mapM (M.!? c) ts
      guard $ all (== val) vals
      pure val

-- try to make bdCandidates "smaller" by looking at a particular tile,
-- and eliminate inconsistent candidates.
-- fails if the resulting board is obviously unsolvable (run out of candidates)
tidyBoard :: Board -> Coord -> Maybe (DL.DList (Coord, Bool), Board)
tidyBoard bd@Board {bdCandidates = bdCandidates0} coord = do
  -- aoi for "area of interest"
  let (aoiCandidates1, bdCandidates1) =
        M.partitionWithKey (\k _ -> isClose k coord) bdCandidates0
      aoiCandidates2 = M.map (filter (checkCandidate bd coord)) aoiCandidates1
  guard $ not (any null aoiCandidates2)
  let (aoiCandidates3, ks) =
        runWriter (foldM upd aoiCandidates2 (M.keys aoiCandidates2))
        where
          upd ::
            M.Map Coord [MineCoords] ->
            Coord ->
            Writer (DL.DList (Coord, Bool)) (M.Map Coord [MineCoords])
          upd m coord' = M.alterF alt coord' m
            where
              alt Nothing = pure Nothing
              alt (Just ms) = do
                let (xs, ms') = eliminateCommon coord' ms
                tell (DL.fromList xs)
                pure (Just ms')
      aoiCandidates4 = M.filter (not . canDischarge) aoiCandidates3
        where
          -- a candidate (a number hint with all possible MineCoords) can be discharged when:
          -- (1) there is only one possibility in it
          -- (2) the single possibility no longer has any coord left.
          canDischarge [x] = M.null x
          canDischarge _ = False
  pure (ks, bd {bdCandidates = M.union aoiCandidates4 bdCandidates1})
  where
    isClose :: Coord -> Coord -> Bool
    isClose (x0, y0) (x1, y1) = abs (x0 - x1) <= 1 && abs (y0 - y1) <= 1

-- TODO: the plan is to use "tidyBoard" for all known tiles and borders,
-- after which is done, we'll end up with a list of more (Coord, Bool)s that
-- we can update and improve further.
mkBoard :: TmpBoard -> Maybe (DL.DList (Coord, Bool), Board)
mkBoard (bdDims@(rows, cols), bdNums, bdMines) = do
  let initCandidates :: M.Map Coord [MineCoords]
      initCandidates =
        M.mapWithKey
          ( \(cX, cY) v ->
              let placement = placementTable V.! v
               in fmap (M.mapKeysMonotonic (\(offX, offY) -> (cX + offX, cY + offY))) placement
          )
          bdNums
      bd0 = Board {bdDims, bdNums, bdMines, bdCandidates = initCandidates}
      edgeCoords =
        [(r, c) | r <- [-1, rows], c <- [0 .. cols -1]]
          <> [(r, c) | r <- [-1 .. rows], c <- [-1, cols]]
      tidyCoords = edgeCoords <> M.keys bdMines
  foldM
    ( \(xs0, curBd) coord -> do
        (xs1, bd') <- tidyBoard curBd coord
        pure (xs0 <> xs1, bd')
    )
    (DL.empty, bd0)
    tidyCoords

improveBoard :: Board -> DL.DList (Coord, Bool) -> Maybe (DL.DList (Coord, Bool), Board)
improveBoard bdPre xsPre = do
  let bd = bdPre {bdMines = M.union (M.fromList xs) (bdMines bdPre)}
      xs = DL.toList xsPre
  -- make sure that union doesn't merge conflicting results.
  guard $ and $ M.elems $ M.intersectionWith (==) (M.fromList xs) (bdMines bdPre)
  foldM
    ( \(xs0, curBd) coord -> do
        (xs1, bd') <- tidyBoard curBd coord
        pure (xs0 <> xs1, bd')
    )
    (DL.empty, bd)
    (fmap fst xs)

-- fully apply a MineCoords, the idea here is to see if we can find contradictions this way.
applyMineCoords :: Board -> MineCoords -> Maybe (DL.DList (Coord, Bool), Board)
applyMineCoords bd0 mc = do
  bd1 <- solveBoardStage0 bd0 (DL.fromList (M.toList mc))
  pure (DL.empty, bd1)

makingProgress :: Board -> Board -> Bool
makingProgress before after =
  bdCandidates before /= bdCandidates after
    || bdMines before /= bdMines after

solveBoardStage0 :: Board -> DL.DList (Coord, Bool) -> Maybe Board
solveBoardStage0 bd xs = do
  (xs', bd') <- improveBoard bd xs
  -- note that if there's no change of candidate or mines between bd and bd'
  -- xs' will not contain anything.
  -- therefore checking whether xs' is empty is not necessary.
  if makingProgress bd bd'
    then solveBoardStage0 bd' xs'
    else Just bd'

-- stage1 is more expensive to do so we only do this when stage0 is no longer making progress.
solveBoardStage1 :: Board -> Maybe Board
solveBoardStage1 bd@Board {bdCandidates} = do
  let failedAttempts :: [(Coord, MineCoords)]
      failedAttempts =
        filter (isNothing . applyMineCoords bd . snd)
          . concatMap (\(k, vs) -> fmap (k,) vs)
          -- start trying on least amount of alternatives, reducing branching factor.
          . sortOn (length . snd)
          $ M.toList bdCandidates
  case failedAttempts of
    [] -> Just bd
    (k, mc) : _ -> do
      -- TODO: this is messy, consider simplifying it.
      let bdInit = bd {bdCandidates = M.adjust (delete mc) k bdCandidates}
      (xs, bd') <-
        foldM
          ( \(curXs, curBd) c -> do
              (ys, nextBd) <- tidyBoard curBd c
              pure (curXs <> ys, nextBd)
          )
          (DL.empty, bdInit)
          (M.keys mc)
      solveBoardStage0 bd' xs

solveBoard :: Board -> DL.DList (Coord, Bool) -> Maybe Board
solveBoard bd0 xs = do
  bd1 <- solveBoardStage0 bd0 xs
  bd2 <- solveBoardStage1 bd1
  if makingProgress bd1 bd2
    then solveBoardStage0 bd2 DL.empty
    else Just bd2

pprBoard :: Board -> IO ()
pprBoard bd@Board {bdDims = (rows, cols), bdNums, bdCandidates} = do
  putStrLn "===="
  forM_ [0 .. rows -1] $ \r -> do
    forM_ [0 .. cols -1] $ \c -> do
      let coord = (r, c)
      putStr $ case getTile bd coord of
        Nothing -> "?"
        Just b ->
          if b
            then "*"
            else maybe " " show (bdNums M.!? coord)

    putStrLn ""
  putStrLn "===="
  unless (M.null bdCandidates) $ do
    putStrLn "Candidates:"
    forM_ (M.toAscList bdCandidates) $
      \(coord, cs) -> do
        let n = bdNums M.! coord
        putStrLn $ show n <> " on " <> show coord <> ":"
        mapM_ (putStrLn . ("  " ++) . show) cs

main :: IO ()
main = do
  args <- getArgs
  raw <- case args of
    [fs] -> readFile fs
    _ -> pure sampleRaw
  let tmpBd = parseBoard raw
      Just (xs, bd) = mkBoard tmpBd
      Just bdFin = solveBoard bd xs
  pprBoard bdFin
