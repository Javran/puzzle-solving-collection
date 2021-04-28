{-
  This module tries to unify the interface for working with
  squares and hexagons.
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Arrow.CoordSystem
  ( gCoords
  , sqCoords
  , hexCoords
  )
where

import qualified Data.Map.Strict as M
import Data.Proxy
import Game.Arrow.Types

class CoordSystem (k :: PuzzleShape) where
  type Coord k

  {-
    For this function, it is assumed that: input coord is valid
    (i.e. satisfy type invariants) and is already inside.
   -}
  surrounding :: forall p. p k -> Int -> Coord k -> [Coord k]
  shapedCoords :: forall p. p k -> Int -> [[Coord k]]

instance CoordSystem 'Square where
  type Coord 'Square = SqCoord
  surrounding _ sz (x, y) =
    [ c
    | i <- [x -1 .. x + 1]
    , j <- [y -1 .. y + 1]
    , let c = (i, j)
    , isInside c
    ]
    where
      isInside (u, v) = u >= 0 && u < sz && v >= 0 && v < sz

  shapedCoords _ sz = [[(r, c) | c <- [0 .. sz -1]] | r <- [0 .. sz -1]]

instance CoordSystem 'Hexagon where
  type Coord 'Hexagon = CubeCoord
  surrounding _ sz c@(x, y, z) =
    c :
    filter
      isInside
      [ (x, y + 1, z -1)
      , (x, y -1, z + 1)
      , (x + 1, y, z -1)
      , (x -1, y, z + 1)
      , (x + 1, y -1, z)
      , (x - 1, y + 1, z)
      ]
    where
      isInside (i, j, k) = abs i + abs j + abs k < sz

  shapedCoords _ sz =
    [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
      <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    where
      mx = sz -1

gCoords
  :: forall p cs.
  (CoordSystem cs, Ord (Coord cs))
  => p cs
  -> Int
  -> ([[Int]], [[Coord cs]])
gCoords ty sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    nestedAllCoords :: [[Coord cs]]
    nestedAllCoords = shapedCoords ty sz
    allCoords = concat nestedAllCoords
    surrounding' c = surrounding ty sz c
    coordEqns :: M.Map (Coord cs) [Coord cs]
    coordEqns = M.fromList $ fmap (\c -> (c, surrounding' c)) allCoords
    mkEqn c =
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs = coordEqns M.! c

hexCoords :: Int -> ([[Int]], [[CubeCoord]])
hexCoords = gCoords (Proxy :: Proxy 'Hexagon)

sqCoords :: Int -> ([[Int]], [[(Int, Int)]])
sqCoords = gCoords (Proxy :: Proxy 'Square)
