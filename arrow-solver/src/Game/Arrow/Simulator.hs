{-
  Simulates moves performed on a puzzle.
 -}

module Game.Arrow.Simulator
  ( applyMoves
  )
where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy
import Game.Arrow.CoordSystem
import Game.Arrow.Types

applyMoves :: Puzzle -> [[Int]] -> [[Int]]
applyMoves Puzzle {opMod, pzType = (pzShape, sz), grid} shapedMoves =
  withShape
    pzShape
    ( \(pty :: Proxy k) ->
        let (_, sCoords) = gCoords pty sz
            moves :: M.Map (Coord k) Int
            moves = M.fromList $ zip (concat sCoords) (concat shapedMoves)
            initGridMap :: M.Map (Coord k) Int
            initGridMap = M.fromList $ zip (concat sCoords) (concat grid)
            getResult :: Coord k -> Int
            getResult c = (initGridMap M.! c + moveCount) `mod` opMod
              where
                moveCount = getSum $ foldMap (Sum . (moves M.!)) (surrounding pty sz c)
         in (fmap . fmap) getResult sCoords
    )
