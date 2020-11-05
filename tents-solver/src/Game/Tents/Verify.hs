module Game.Tents.Verify where

import qualified Data.Map.Strict as M
import Data.Maybe
import Game.Tents.Types

{-
  Verify that a game board is solved
 -}

verifyBoard :: (Int, Int) -> M.Map Coord Cell -> Bool
verifyBoard dims cells = isJust $ undefined
  where
    (rows, cols) = dims
