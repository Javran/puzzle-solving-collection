{-# LANGUAGE NamedFieldPuns #-}

module Game.Arrow.Solver
  (
  )
where

import Data.List.Split
import Game.Arrow.CoordSystem
import Game.Arrow.Gaussian
import Game.Arrow.Types

-- Split a list into lines of a flat-top hexagon whose side length is n.
hexSplit :: Int -> [a] -> [[a]]
hexSplit n = splitPlaces $ [n .. n + n -1] <> reverse (init splits)
  where
    splits = [n .. n + n -1]

solve :: Puzzle -> Either (Err Int) [[Int]]
solve Puzzle {opMod, pzType = (Hexagon, sz), grid} = do
  let inp =
        (fmap . fmap)
          (\v -> (- v) `mod` opMod)
          grid
      (matLhs, _) = hexCoords sz
      mat = zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)
  case solveMatOne opMod mat of
    Left e -> Left e
    Right xs -> Right $ hexSplit sz xs
