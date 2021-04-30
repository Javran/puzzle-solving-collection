{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Arrow.Solver
  (
  )
where

import Data.List.Split
import Data.Proxy
import Data.Tagged
import Game.Arrow.CoordSystem
import Game.Arrow.Gaussian
import Game.Arrow.Types

-- Split a list into lines of a flat-top hexagon whose side length is n.
hexSplit :: Int -> [a] -> [[a]]
hexSplit n = splitPlaces $ [n .. n + n -1] <> reverse (init splits)
  where
    splits = [n .. n + n -1]

-- https://stackoverflow.com/a/67319945/315302
promote :: forall r. PuzzleShape -> (forall k. (CoordSystem k, Ord (Coord k)) => Tagged k r) -> r
promote Square a = unTagged (a @Square)
promote Hexagon a = unTagged (a @Hexagon)

solve :: Puzzle -> Either (Err Int) [[Int]]
solve Puzzle {opMod, pzType = (pzShape, sz), grid} =
  promote
    pzShape
    (unproxy $ \pty -> do
       let inp =
             (fmap . fmap)
               (\v -> (- v) `mod` opMod)
               grid
           (matLhs, _) = gCoords pty sz
           mat = zipWith (\xs rhs -> foldr (:) [rhs] xs) matLhs (concat inp)
       case solveMatOne opMod mat of
         Left e -> Left e
         Right xs -> Right $ toChunks pty sz xs)
