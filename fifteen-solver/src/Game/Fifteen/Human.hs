{-# LANGUAGE NamedFieldPuns #-}

module Game.Fifteen.Human where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS
import Data.Bifunctor
import qualified Data.DList as DL
import Data.Function
import Data.List
import qualified Data.Set as S
import Data.Tuple
import qualified Data.Vector as V
import Debug.Trace
import Game.Fifteen.Common
import Game.Fifteen.Types

{-
  A mechanical and human-comprehensible way of solving the puzzle.
  This approach is inspired by http://www.kopf.com.br/kaplof/how-to-solve-any-slide-puzzle-regardless-of-its-size

  Basic idea: consider a 5x5 puzzle:

  AAAAA
  ABBBB
  ABBBB
  ABBBB
  ABBB_

  We first solve horizontal A, and then vertical A.
  Then we effectly just have a 4x4 puzzle to deal with.
  Recursively, we only have to solve a 3x3 puzzle, in which we do have
  an optimal solver to deal with it.

  So this method allows solving any n x n puzzles where n >= 3.

 -}

{-
  Top-left coord and bottom-right coord.
  the height and width of a Rect must be >= 2.
 -}
type Rect = (Coord, Coord)

minMax :: Ord a => a -> a -> (a, a)
minMax a b = if a <= b then (a, b) else (b, a)

{-
  In order to rotate a tile from cur to target,
  we first need to find a rectangle for this movement.
  If cur and target share a row or a column, we extend
  the rectangle in right or down direction,
  which is always possible to do unless the rectangle
  is already at last row or column.
 -}
findRotatingRect :: Coord -> Coord -> Rect
findRotatingRect target@(tr, tc) cur@(cr, cc)
  | target == cur =
    error "This function should not be called with identical coords"
  | tr == cr =
    let (minR, maxR) = (tr, tr + 1)
        (minC, maxC) = minMax tc cc
     in ((minR, minC), (maxR, maxC))
  | tc == cc =
    let (minR, maxR) = minMax tr cr
        (minC, maxC) = (tc, tc + 1)
     in ((minR, minC), (maxR, maxC))
  | otherwise =
    let (minR, maxR) = minMax tr cr
        (minC, maxC) = minMax tc cc
     in ((minR, minC), (maxR, maxC))

{-
  Protected coords are coordinates that have been solved,
  any procedure touching those coords must recover them to
  their original places.
 -}
type ProtectedCoords = S.Set Coord

rectToCoords :: Rect -> S.Set Coord
rectToCoords ((rMin, cMin), (rMax, cMax)) =
  S.fromList $
    [(rMin, c) | c <- [cMin .. cMax]]
      <> [(rMax, c) | c <- [cMin .. cMax]]
      <> [(r, cMin) | r <- [rMin .. rMax]]
      <> [(r, cMax) | r <- [rMin .. rMax]]

-- plan a path for hole to move to any targets of a Set without touching
-- any protected coords.
findPathForHole :: Board -> S.Set Coord -> ProtectedCoords -> [[Coord]]
findPathForHole bd@Board {bdHole} targetCoordsPre pCoords =
  findPath [(bdHole, [])] S.empty
  where
    targetCoords = targetCoordsPre `S.difference` pCoords
    findPath :: [(Coord, [Coord])] -> S.Set Coord -> [[Coord]]
    findPath [] _ = []
    findPath ((coord, path) : todos) visited
      | S.member coord targetCoords = pure (reverse path)
      | S.member coord visited = findPath todos visited
      | otherwise = do
        let coordsInOneDir d = unfoldr go coord
              where
                go curCoord = do
                  let nextCoord = applyDir curCoord d
                  guard $ bdInRange bd nextCoord
                  guard $ S.notMember nextCoord pCoords
                  pure (nextCoord, nextCoord)
            nextCoords =
              filter (`S.notMember` visited) $
                concatMap coordsInOneDir [DUp, DDown, DLeft, DRight]
            todos' = todos <> fmap (\x -> (x, x : path)) nextCoords
        findPath todos' (S.insert coord visited)

-- Sim for Simulator.
type Sim = RWST () (DL.DList Coord) (Board, ProtectedCoords) Maybe

{-
  A sequence of operations moving the center tile (0,0) into corner (-1,1).

  This sequence assumes an initial state:

  a b c
  E x d
  ? ? ?

  where:
  - E is the empty tile
  - x is the tile we want to move to where c currently is.
  - the position of a and b must be preserved after this sequence of operations.
  -  we don't care about where ?,c,d end up being.

 -}
rowCornerRotateSolution, colCornerRotateSolution :: [Coord]
rowCornerRotateSolution =
  [ {-
      a b c    E b c
      E x d => a x d
      ? ? ?    ? ? ?
     -}
    (-1, -1)
  , {-
      E b c    b E c
      a x d => a x d
      ? ? ?    ? ? ?
     -}
    (-1, 0)
  , {-
      b E c    b x c
      a x d => a E d
      ? ? ?    ? ? ?
     -}
    (0, 0)
  , {-
      b x c    b x c
      a E d => a d E
      ? ? ?    ? ? ?
     -}
    (0, 1)
  , {-
      b x c    b x E
      a d E => a d c
      ? ? ?    ? ? ?
     -}
    (-1, 1)
  , {-
      b x E    E b x
      a d c => a d c
      ? ? ?    ? ? ?
     -}
    (-1, -1)
  , {-
      E b x    a b x
      a d c => E d c
      ? ? ?    ? ? ?
     -}
    (0, -1)
  ]
colCornerRotateSolution = fmap swap rowCornerRotateSolution

{-
  TODO Step 1: solve horizontal "AAAAA".

  To move a tile to target position:
  - find a rectangle (both side length must be >= 2)
  - move the blank tile into rectangle
  - rotate.
 -}
solveBoard :: Board -> Board -> [[Coord]]
solveBoard goal initBoard =
  case runRWST solveAux () (initBoard, S.empty) of
    Just ((), _, moves) -> [DL.toList moves]
    Nothing -> []
  where
    solveCoord coord = do
      let Just goalTile = bdGet goal coord
      solveSimpleTile coord goalTile

    solveAux :: Sim ()
    solveAux = do
      solveCoord (0, 0)
      solveCoord (0, 1)
      solveCoord (0, 2)
      solveCoord (0, 3)

      -- note that we do want first row to be fully solved before moving to first col
      -- otherwise a tile meant for col might stuck in a corner that is tricky
      -- to get out.
      do
        -- move "5" tile to right position
        tryMoveTile (3, 0) (1, 3)

      do
        -- move hole to right place
        (bd, pCoords) <- get
        let path : _ = findPathForHole bd (S.singleton (1, 2)) (S.insert (1, 3) pCoords)
        mapM_ play path

      do
        -- do corner rotate.
        let rotateMoves = fmap tr rowCornerRotateSolution
            tr (r, c) = (r + 1, c + 3)
        mapM_ play rotateMoves
        -- trivial solve, just marking it protected
        solveCoord (0, 4)

      solveCoord (1, 0)
      solveCoord (2, 0)
      solveCoord (3, 0)

      do
        -- move "21" tile to right position
        tryMoveTile (1, 3) (3, 1)

      do
        -- move hole to right place
        (bd, pCoords) <- get
        let path : _ = findPathForHole bd (S.singleton (2, 1)) (S.insert (3, 1) pCoords)
        mapM_ play path
        pure ()
      do
        let rotateMoves = fmap tr colCornerRotateSolution
            tr (r, c) = (r + 3, c + 1)
        mapM_ play rotateMoves
        -- trivial solve, just marking it protected
        solveCoord (4, 0)

play :: Coord -> Sim ()
play move = do
  allMoves <- gets (possibleMoves . fst)
  bd' <- lift $ lookup move allMoves
  modify (first (const bd'))
  tell $ DL.singleton move

-- rotate until coord is set to a certain tile
rotateUntilFit :: Rect -> Coord -> Int -> Sim ()
rotateUntilFit rect coord expectedTile = do
  let ((rMin, cMin), (rMax, cMax)) = rect
      initMoves :: [Coord]
      initMoves = cycle [(rMin, cMin), (rMin, cMax), (rMax, cMax), (rMax, cMin)]
  fix
    (\loop (move : moves) -> do
       bd <- gets fst
       let tile = bdGet bd coord
       if tile == Just expectedTile
         then pure ()
         else case lookup move (possibleMoves bd) of
           Nothing -> loop moves
           Just _ -> play move >> loop moves)
    initMoves

solveSimpleTile :: Coord -> Int -> Sim ()
solveSimpleTile goalCoord@(gR, gC) goalTile = do
  (bd, _) <- get
  let tileCoord = bdNums bd V.! goalTile
  unless (goalCoord == tileCoord) $ do
    tryMoveTile tileCoord goalCoord
      <|> do
        let goalCoord' = (gR + 1, gC + 1)
        tryMoveTile tileCoord goalCoord'
        tryMoveTile goalCoord' goalCoord
  modify (second (S.insert goalCoord))

-- this function is guaranteed to not do anything if rect hits protected coords.
tryMoveTile :: Coord -> Coord -> Sim ()
tryMoveTile srcCoord dstCoord
  | srcCoord == dstCoord = pure ()
  | otherwise = do
    (bd, pCoords) <- get
    srcTile <- lift $ bdGet bd srcCoord
    let rotatingRect = findRotatingRect srcCoord dstCoord
        rectCoords = rectToCoords rotatingRect
    guard $ S.null (S.intersection rectCoords pCoords)
    -- move the hole somewhere into the Rect without moving source tile.
    moves : _ <- pure $ findPathForHole bd rectCoords (S.insert srcCoord pCoords)
    mapM_ play moves
    -- TODO: do CW or CCW rotation?
    rotateUntilFit rotatingRect dstCoord srcTile
