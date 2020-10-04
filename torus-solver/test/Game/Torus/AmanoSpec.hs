{-# LANGUAGE ScopedTypeVariables #-}

module Game.Torus.AmanoSpec where

import qualified Data.Vector as V
import Game.Torus.Amano
import Game.Torus.Board
import Game.Torus.BoardSpec
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- the board could be unsolvable when # of cols is odd,
-- if this happens, the last two tiles will be flipped
-- note that it is intentional that if cols is even,
-- we don't attempt to fix it.
unsolvableWorkaround :: Board -> Board
unsolvableWorkaround bd =
  if odd cols
    then
      bd
        { bdTiles =
            vs
              V.// [ (lastInd -1, vs V.! lastInd)
                   , (lastInd, vs V.! (lastInd -1))
                   ]
        }
    else bd
  where
    lastInd = rows * cols -1
    (rows, cols) = bdDims bd
    vs = bdTiles bd

spec :: Spec
spec =
  describe "solveBoard" $
    prop "SmallBoard" $
      \(SmallBoard bd) ->
        let (moves, bdFin) = solveBoard bd
         in label
              "moves are consistent"
              (applyMoves bd moves === bdFin)
              .&&. label
                "final state is solved (or can be fixed to solved)"
                (isSolved bdFin || isSolved (unsolvableWorkaround bdFin))
