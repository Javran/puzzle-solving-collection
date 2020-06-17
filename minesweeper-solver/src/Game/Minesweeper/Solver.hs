{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Minesweeper.Solver where

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
import Game.Minesweeper.Types
import System.Environment

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
  TODO: somehow we will end up having some out-of-bound coordinates on board,
  this should not happen.
 -}

{-
  Like "pick", but whenever an element picked,
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

getTile :: Board -> Coord -> Maybe Bool
getTile Board {bdDims = (rows, cols), bdMines} coord =
  if inRange ((0, 0), (rows -1, cols -1)) coord
    then bdMines M.!? coord
    else Just False

-- check a candidate against current board to ensure consistency
-- the argument given as Coord must be one of those number tiles.
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
    -- a coordinate is common when all candidates have the same value
    -- in this particular coordinate.
    isCommon :: Coord -> Maybe Bool
    isCommon c = do
      val <- t M.!? c
      vals <- mapM (M.!? c) ts
      guard $ all (== val) vals
      pure val

-- try to make bdCandidates "smaller" by looking at a particular tile,
-- and eliminate inconsistent candidates.
-- fails if the resulting board is obviously unsolvable (run out of candidates)
-- note that this step might generate out-of-bound future updates.
tidyBoard :: Board -> Coord -> Maybe (DL.DList (Coord, Bool), Board)
tidyBoard bd@Board {bdCandidates = bdCandidates0} coord = do
  let {-
        aoi for "area of interest"
        here we want to extract the affected part out,
        make modification on it and then put it back.
      -}
      (aoiCandidates1, bdCandidates1) =
        M.partitionWithKey (\k _ -> isClose k coord) bdCandidates0
      -- invalid candidates are eliminated here.
      aoiCandidates2 =
        M.map (filter (checkCandidate bd coord)) aoiCandidates1
  -- if any of those number tiles end up having no candidate
  -- to pick from, that means the current solution is not possible.
  guard $ not (any null aoiCandidates2)
  -- at this point elimination is done,
  -- following steps are all about reducing candidates.
  let (aoiCandidates3, ks) =
        -- this step is to reduce # of tiles need to be considered within a candidate.
        -- the idea is to simply look for things in common across all possibilities
        -- and extract those out.
        runWriter (foldM go aoiCandidates2 (M.keys aoiCandidates2))
        where
          go ::
            M.Map Coord [MineCoords] ->
            Coord ->
            Writer (DL.DList (Coord, Bool)) (M.Map Coord [MineCoords])
          go m coord' = M.alterF alt coord' m
            where
              alt Nothing = pure Nothing
              alt (Just ms) = do
                let (xs, ms') = eliminateCommon coord' ms
                tell (DL.fromList xs)
                pure (Just ms')
      -- this step removes a number tile from candidate mapping if
      -- it is no longer necessary to keep.
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

{-
  Create Board from TmpBoard. This only sets up everything other than bdCandidates.
  This is because setting bdCandidates can trigger a chain reaction that will end up
  other fields as well. So here I figure it's best to separate out future updates
  by having a difflist.
 -}
mkBoard :: TmpBoard -> Maybe (DL.DList (Coord, Bool), Board)
mkBoard (bdDims@(rows, cols), bdNums, bdMines) = do
  let initCandidates :: M.Map Coord [MineCoords]
      -- initial candidates are just blindly copied from precomputed data,
      -- invalid candidates are to be eliminated by tidyBoard.
      initCandidates =
        M.mapWithKey
          ( \(cX, cY) v ->
              let placement = placementTable V.! v
               in fmap (M.mapKeysMonotonic (\(offX, offY) -> (cX + offX, cY + offY))) placement
          )
          bdNums
      bd0 = Board {bdDims, bdNums, bdMines, bdCandidates = initCandidates}
      -- those are "just-out-of-bound" coordinates that we intend to perform "tidyBoard" on.
      -- this way we'll eliminate candidates that doesn't fit into the board.
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
  -- TODO: set mine one step at a time, and eliminate invalid ones.
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
