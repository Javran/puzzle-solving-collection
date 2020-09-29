{-# LANGUAGE NamedFieldPuns #-}

module Game.Fifteen.Human where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS
import qualified Data.DList as DL
import Data.Function
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V
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
    error "This function should not be called"
  | tr == cr =
    let (minR, maxR) = (tr, tr + 1)
        (minC, maxC) = (tc, cc)
     in ((minR, minC), (maxR, maxC))
  | tc == cc =
    let (minR, maxR) = (tr, cr)
        (minC, maxC) = (tc, tc + 1)
     in ((minR, minC), (maxR, maxC))
  | otherwise =
    let (minR, maxR) = (tr, cr)
        (minC, maxC) = (tc, cc)
     in ((minR, minC), (maxR, maxC))

{-
  Protected coords are coordinates that have been solved,
  any procedure touching those coords must recover them to
  their original places.
 -}
type ProtectedCoords = S.Set Coord

-- plan a path for hole to move to a Rect without touching
-- any protected coords.
findPathForHole :: Board -> Rect -> ProtectedCoords -> [[Coord]]
findPathForHole bd@Board {bdHole} ((rMin, cMin), (rMax, cMax)) pCoords =
  findPath [(bdHole, [])] S.empty
  where
    targetCoords =
      S.fromList [(r, c) | r <- [rMin .. rMax], c <- [cMin .. cMax]]
        `S.difference` pCoords
    findPath :: [(Coord, [Coord])] -> S.Set Coord -> [[Coord]]
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
type Sim = RWST () (DL.DList Coord) Board Maybe

{-
  TODO Step 1: solve horizontal "AAAAA".

  To move a tile to target position:
  - find a rectangle (both side length must be >= 2)
  - move the blank tile into rectangle
  - rotate.
 -}
solveBoard :: Board -> Board -> [[Coord]]
solveBoard goal initBoard =
  if initTileCoord == (0, 0)
    then []
    else case runRWST solveAux () initBoard of
      Just ((), _, moves) -> [DL.toList moves]
      Nothing -> []
  where
    -- TODO: let's just solve top-left corner first.
    Just goalTile = bdGet goal (0, 0)
    initTileCoord = bdNums initBoard V.! goalTile

    play :: Coord -> Sim ()
    play move = do
      allMoves <- gets possibleMoves
      bd' <- lift $ lookup move allMoves
      put bd'
      tell $ DL.singleton move

    -- rotate until coord is set to a certain tile
    rotateUntilFit :: Rect -> Coord -> Int -> Sim ()
    rotateUntilFit rect coord expectedTile = do
      let ((rMin, cMin), (rMax, cMax)) = rect
          initMoves :: [Coord]
          initMoves = cycle [(rMin, cMin), (rMin, cMax), (rMax, cMax), (rMax, cMin)]
      fix
        (\loop (move : moves) -> do
           bd <- get
           let tile = bdGet bd coord
           if tile == Just expectedTile
             then pure ()
             else
               case lookup move (possibleMoves bd) of
                 Nothing -> loop moves
                 Just _ -> play move >> loop moves)
        initMoves

    solveAux :: Sim ()
    solveAux = do
      bd <- get
      let tileCoord = bdNums bd V.! goalTile
          rotatingRect =
            findRotatingRect (0, 0) tileCoord
      moves : _ <-
        pure $
          findPathForHole bd rotatingRect (S.singleton tileCoord)
      mapM_ play moves
      rotateUntilFit rotatingRect (0,0) goalTile
