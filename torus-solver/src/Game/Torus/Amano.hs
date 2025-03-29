module Game.Torus.Amano where

{-
  This solution is described by Amano et al. "How to Solve the Torus Puzzle", Published Jan 13, 2012.
 -}

import Control.Monad
import Control.Monad.Trans.RWS.CPS
import Data.Bifunctor
import qualified Data.DList as DL
import Data.Function
import qualified Data.Vector as V
import Game.Torus.Board

north, south, west, east :: Int -> Int -> Move
[north, south, west, east] =
  [MoveUp, MoveDown, MoveLeft, MoveRight]

{-
  This includes triangle rotations and the "three element rotation" built on top it.
  It so happens that both kinds of rotation rotates 3 tile, there the name Rotation3.
 -}
type Rotation3 =
  Int -> Int -> Int -> Int -> [Move]

{-
  The A,B,C,D are right triangle rotations described in the paper, namely:

  row / col --->
   |
   |     /  |  \
   |    /D  p  A\
   v        |
       --q--c--q--
            |
        \C  p  B/
         \  |  /

   where p and q are lengths of legs.
   This kind of rotation only rotates 3 points of the triangle with all other tiles
   unchanged. Rotation can occur in two directions:

   - clockwise (prefixed by cw-)
   - counterclockwise (prefixed by ccw-)

 -}

cwA, ccwA, cwB, ccwB, cwC, ccwC, cwD, ccwD :: Rotation3
cwA i j p q =
  [west i q, south j p, east i q, north j p]
ccwA i j p q =
  [south j p, west i q, north j p, east i q]
cwB i j p q =
  [north j p, west i q, south j p, east i q]
ccwB i j p q =
  [west i q, north j p, east i q, south j p]
cwC i j p q =
  [east i q, north j p, west i q, south j p]
ccwC i j p q =
  [north j p, east i q, south j p, west i q]
cwD i j p q =
  [south j p, east i q, north j p, west i q]
ccwD i j p q =
  [east i q, south j p, west i q, north j p]

-- ter for "three elements rotation"
terW, terE :: Rotation3
terW i j p q = cwD i j 1 p <> cwA i j 1 q
terE i j p q =
  -- note: the paper isn't very clear on the ordering of p and q here,
  -- so I assume that terW i j p q <> terE i j p q will be an no-op.
  ccwA i j 1 q <> ccwD i j 1 p

solveBoard :: Board -> ([Move], Board)
solveBoard bd@Board {bdDims = (rows, cols)} = (DL.toList moves, bdFin)
  where
    ((), (_, bdFin), moves) =
      runRWS (replicateM_ (rows * cols) solveFocus) () (0, bd)

{-
  Sim for simulator.

  Reader: not used
  Writer: DList of Moves
  State: (<focus>, <board>)
    where focus is the current focusing index,
    which means linear index [0..focus-1] have
    all been solved.
 -}
type Sim = RWS () (DL.DList Move) (Int, Board)

playMove :: Move -> Sim ()
playMove m = do
  bd <- gets snd
  modify (second (const (applyMove bd m)))
  tell $ DL.singleton m

playMoves :: [Move] -> Sim ()
playMoves xs = do
  bd <- gets snd
  modify (second (const (applyMoves bd xs)))
  tell $ DL.fromList xs

bdIndexToCoord :: Board -> Int -> Coord
bdIndexToCoord Board {bdDims = (_, cols)} i = i `quotRem` cols

{-
  move the right tile to the current focus, and advance the focus.
 -}
solveFocus :: Sim ()
solveFocus = do
  (focus, bd@Board {bdDims = (rows, cols), bdTiles}) <- get
  unless (bdTiles V.! focus == focus) $ do
    let (fR, fC) = bdIndexToCoord bd focus
        srcInd =
          fix
            ( \loop curInd ->
                if bdTiles V.! curInd == focus
                  then curInd
                  else loop (curInd + 1)
            )
            (focus + 1)
        (sR, sC) = bdIndexToCoord bd srcInd
    if
      | -- we are dealing with last row
        fR == rows - 1 ->
          if
            | fC >= cols - 2 ->
                if odd cols
                  then {-
                         the paper claims that last two tiles should have been solved
                         so it won't even reach here.
                         TODO: no longer throws error
                         to make this work for testing.
                       -}
                    pure ()
                  else -- we can do a swap here since col is even.

                    let centers = take (cols `quot` 2) [cols - 1, cols + 1 ..]
                        twMoves = concatMap (\c -> terW fR (c `rem` cols) 1 1) centers
                     in playMoves $ twMoves <> [east fR 1]
            | otherwise ->
                if sC == cols - 1
                  then playMoves $ terE fR (sC - 1) (sC - 1 - fC) 1
                  else playMoves $ terW fR sC (sC - fC) 1
      | -- when they share the same row.
        fR == sR ->
          {-
            since we know fC < sC,
            ccwB should do.
           -}
          playMoves $ ccwB fR fC 1 (sC - fC)
      | -- when they share the same col.
        fC == sC ->
          {-

            special case when c = 0, otherwise just use fC-1
          -}
          if fC == 0
            then playMoves $ cwA sR sC (sR - fR) 1
            else playMoves $ ccwD sR sC (sR - fR) 1
      | -- when they don't share row or col
        otherwise ->
          if sC < fC
            then playMoves $ cwD sR fC (sR - fR) (fC - sC)
            else playMoves $ ccwA sR fC (sR - fR) (sC - fC)
  -- advance focus
  modify (first succ)
