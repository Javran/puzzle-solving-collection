{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Minesweeper.Solver
  ( getTile,
    mkBoard,
    isCoordInRange,
    solveBoardFromRaw,
    solveBoard,
  )
where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer.Strict
import qualified Data.DList as DL
import Data.List
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.UnionFind.ST as UF
import qualified Data.Vector as V
import Game.Minesweeper.Parser
import Game.Minesweeper.Types

-- 2d offset of 8 surrounding tiles.
surroundings :: [Offset]
surroundings =
  [ -- top
    (-1, -1),
    (-1, 0),
    (-1, 1),
    -- middle
    (0, -1),
    (0, 1),
    (1, -1),
    -- bottom
    (1, 0),
    (1, 1)
  ]

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

isCoordInRange :: Board -> Coord -> Bool
isCoordInRange Board {bdDims = (rows, cols)} (r, c) =
  r >= 0 && r < rows && c >= 0 && c < cols

getTile :: Board -> Coord -> Maybe Bool
getTile bd@Board {bdMines} coord =
  if isCoordInRange bd coord
    then bdMines M.!? coord
    else Just False

setMineMap :: Board -> Coord -> Bool -> Maybe Board
setMineMap bd coord m =
  if isCoordInRange bd coord
    then case bdMines bd M.!? coord of
      Nothing -> do
        let bd' = bd {bdMines = M.insert coord m (bdMines bd)}
        improveBoardFix bd' (DL.singleton (coord, m))
      Just m' ->
        bd <$ guard (m' == m)
    else bd <$ guard (m == False)

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
        M.partitionWithKey (\k _ -> isCoordClose k coord) bdCandidates0
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

isCoordClose :: Coord -> Coord -> Bool
isCoordClose (x0, y0) (x1, y1) = abs (x0 - x1) <= 1 && abs (y0 - y1) <= 1

{-
  Create Board from BoardRep. This only sets up everything other than bdCandidates.
  This is because setting bdCandidates can trigger a chain reaction that will end up
  other fields as well. So here I figure it's best to separate out future updates
  by having a difflist.
 -}
mkBoard :: BoardRep -> Maybe (DL.DList (Coord, Bool), Board)
mkBoard
  BoardRep
    { brDims = bdDims@(rows, cols),
      brNums = bdNums,
      brMines = bdMines,
      brMissing
    } = do
    guard $ S.null brMissing
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
  let xs = DL.toList xsPre
  {-
    merge pairs in xs into MineMap of the board.
    - invalid assignments result in failure
    - out-of-bound assignments are ignored
      (but in order for this assignment to be valid, the value must be False)
   -}
  bd <- foldM (\curBd (k, v) -> setMineMap curBd k v) bdPre xs
  foldM
    ( \(xs0, curBd) coord -> do
        (xs1, bd') <- tidyBoard curBd coord
        pure (xs0 <> xs1, bd')
    )
    (DL.empty, bd)
    (fmap fst xs)

-- keep improving until there is nothing to do.
improveBoardFix :: Board -> DL.DList (Coord, Bool) -> Maybe Board
improveBoardFix bd xs = do
  (ys, bd') <- improveBoard bd xs
  DL.list
    -- null case
    (pure bd')
    -- cons case
    (\_ _ -> improveBoardFix bd' ys)
    -- dlist
    ys

{-
  Perform depth first search on a set of coordinates
  (those coordinates should usually be key of bdCandidates).
 -}
applyMineCoordsDeep :: Board -> [Coord] -> [Board]
applyMineCoordsDeep bd = \case
  [] -> [bd]
  (coord : cs) ->
    case bdCandidates bd M.!? coord of
      Just candidates -> do
        mcs <- candidates
        Just bd' <- pure $ improveBoardFix bd (DL.fromList (M.toList mcs))
        applyMineCoordsDeep bd' cs
      Nothing ->
        -- It is not necessary that all coords given has bdCandidates,
        -- this is because those coordinates could be eliminated by applying
        -- other candidate coordinates.
        applyMineCoordsDeep bd cs

clusterUF :: Ord a => [(UF.Point s a, b)] -> ST s (M.Map a (DL.DList b))
clusterUF pairs =
  -- note the flip on <>, as we want to construct the list in reversed order
  -- so that the original order is kept.
  M.fromListWith
    (flip (<>))
    <$> mapM
      ( \(uf, c) -> do
          cRep <- UF.descriptor =<< UF.repr uf
          pure (cRep, DL.singleton c)
      )
      pairs

clusterCoords :: M.Map Coord [MineCoords] -> M.Map Coord (DL.DList Coord)
clusterCoords mc = runST $ do
  ufs <- mapM UF.fresh sortedCoords
  forM_ (tails (zip sortedCoords ufs)) $ \case
    [] -> pure ()
    (y, ySet) : ys ->
      mapM_
        ( \(_, ySet') ->
            -- make sure ySet is on the RHS,
            -- as that is the descriptor we want to keep.
            UF.union ySet' ySet
        )
        $ filter (isCoordClose y . fst) ys
  clusterUF (zip ufs sortedCoords)
  where
    -- sort by length first
    -- by doing so we can always pick one with least candidate
    -- as representative for union-find set
    sortedCoords :: [Coord]
    sortedCoords =
      fmap fst . sortOn (length . snd) . M.toList $ mc

makingProgress :: Board -> Board -> Bool
makingProgress before after =
  bdCandidates before /= bdCandidates after
    || bdMines before /= bdMines after

{-
  Performs DFS therefore is more expensive to run.
  We only do this when improveBoardFix is no longer making progress.
 -}
solveBoardStageDeep :: Board -> Maybe Board
solveBoardStageDeep bd@Board {bdCandidates} =
  fix
    ( \tryNext clusters ->
        case clusters of
          [] -> Just bd
          cluster : clusters' -> do
            let boardsMissing :: [M.Map Coord Bool]
                boardsMissing =
                  -- only keep those missing from bd (current input board)
                  (\curBd -> M.difference (bdMines curBd) (bdMines bd))
                    <$> applyMineCoordsDeep bd cluster
            case boardsMissing of
              [] -> tryNext clusters'
              _ : _ -> do
                let intersectAux ::
                      M.Map Coord Bool -> M.Map Coord Bool -> M.Map Coord Bool
                    intersectAux =
                      M.merge
                        M.dropMissing
                        M.dropMissing
                        (M.zipWithMaybeMatched compatible)
                      where
                        compatible _k x y = x <$ guard (x == y)
                    common = foldr1 intersectAux boardsMissing
                if M.null common
                  then tryNext clusters'
                  else improveBoardFix bd (DL.fromList (M.toList common))
    )
    initClusters
  where
    initClusters :: [[Coord]]
    initClusters = fmap DL.toList . M.elems $ clusterCoords bdCandidates

solveBoard :: Board -> DL.DList (Coord, Bool) -> Maybe Board
solveBoard bd0 xs = do
  bd1 <- improveBoardFix bd0 xs
  bd2 <- solveBoardStageDeep bd1
  if makingProgress bd1 bd2
    then solveBoard bd2 DL.empty
    else Just bd2

solveBoardFromRaw :: String -> Maybe Board
solveBoardFromRaw raw = do
  tmpBd <- parseBoard raw
  (xs, bd) <- mkBoard tmpBd
  solveBoard bd xs
