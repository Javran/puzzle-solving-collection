module Game.Tents.Verify where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Game.Tents.Types

{-
  Verify that a game board is solved
 -}

verifyBoard :: (Int, Int) -> M.Map Coord Cell -> Bool
verifyBoard dims cells = isJust $ do
  let allCoords =
        S.fromList
          [ (r, c)
          | r <- [0 .. rows -1]
          , c <- [0 .. cols -1]
          ]
      (treeCoords, tentCoords) = foldMap go (M.toList cells)
        where
          go (x, Tent) = (S.empty, S.singleton x)
          go (x, Tree) = (S.singleton x, S.empty)
          go _ = (S.empty, S.empty)
      surroundingCells (r, c) =
        S.fromList
          [ (r + dr, c + dc)
          | dr <- [-1, 0, 1]
          , dc <- [-1, 0, 1]
          , (dr, dc) /= (0, 0)
          ]
  -- make sure all coords are assigned Cell values.
  guard $ allCoords == M.keysSet cells
  -- make sure the count is correct to make pairs.
  guard $ S.size treeCoords == S.size tentCoords
  -- trees should not near each other.
  guard $
    all
      (\coord -> S.null (S.union (surroundingCells coord) treeCoords))
      treeCoords
  -- TODO: pairing to be implemented
  Just ()
  where
    (rows, cols) = dims
