{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications, ExistentialQuantification #-}

module Game.Arrow.Solver
  (
  )
where

import Data.List.Split
import Data.Proxy
import Game.Arrow.CoordSystem
import Game.Arrow.Gaussian
import Game.Arrow.Types

-- Split a list into lines of a flat-top hexagon whose side length is n.
hexSplit :: Int -> [a] -> [[a]]
hexSplit n = splitPlaces $ [n .. n + n -1] <> reverse (init splits)
  where
    splits = [n .. n + n -1]

data SingPuzzleShape (s :: PuzzleShape) where
  SHexagon :: SingPuzzleShape 'Hexagon
  SSquare :: SingPuzzleShape 'Square

class SingPuzzleShapeI (s :: PuzzleShape) where
  sing :: SingPuzzleShape s

instance SingPuzzleShapeI 'Hexagon where
  sing = SHexagon

instance SingPuzzleShapeI 'Square where
  sing = SSquare

solve :: Puzzle -> Either (Err Int) [[Int]]
solve Puzzle {opMod, pzType = (pzShape@Hexagon, sz), grid} = do
  let inp =
        (fmap . fmap)
          (\v -> (- v) `mod` opMod)
          grid
      (matLhs, _) = hexCoords sz
      mat = zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)
  case solveMatOne opMod mat of
    Left e -> Left e
    Right xs -> Right $ toChunks (Proxy @'Hexagon) sz xs
