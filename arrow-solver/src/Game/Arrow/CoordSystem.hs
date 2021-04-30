{-
  This module tries to unify the interface for working with
  squares and hexagons.
 -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Arrow.CoordSystem
  ( gCoords
  , sqCoords
  , hexCoords
  , CoordSystem (..)
  , withShape
  )
where

import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Proxy
import Game.Arrow.Types
import qualified Data.Set as S

class CoordSystem (k :: PuzzleShape) where
  type Coord k

  {-
    For this function, it is assumed that: input coord is valid
    (i.e. satisfy type invariants) and is already inside.
   -}
  surrounding :: forall p. p k -> Int -> Coord k -> [Coord k]
  shapedCoords :: forall p. p k -> Int -> [[Coord k]]
  toChunks :: forall p e. p k -> Int -> [e] -> [[e]]

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
  toChunks _ = chunksOf

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
      isInside (i, j, k) =
        abs i + abs j + abs k < sz + sz

  shapedCoords _ sz =
    [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
      <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    where
      mx = sz -1

  -- Split a list into lines of a flat-top hexagon whose side length is sz.
  toChunks _ sz = splitPlaces $ [sz .. sz + sz -1] <> reverse (init splits)
    where
      splits = [sz .. sz + sz -1]

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

hexCoordsImpl :: Int -> ([[Int]], [[CubeCoord]])
hexCoordsImpl sz = (fmap mkEqn allCoords, nestedAllCoords)
  where
    mx = sz -1
    nestedAllCoords :: [[CubeCoord]]
    nestedAllCoords =
      [[(x, y, z) | y <- [3, 2 .. (-3 - z)], let x = - y - z] | z <- [- mx .. 0]]
        <> [[(x, y, z) | x <- [-3 .. 3 - z], let y = - x - z] | z <- [1 .. mx]]
    allCoords = concat nestedAllCoords
    allCoords' = S.fromList allCoords
    surrounding c@(x, y, z) =
      c :
      filter
        (`S.member` allCoords')
        [ (x, y + 1, z -1)
        , (x, y -1, z + 1)
        , (x + 1, y, z -1)
        , (x -1, y, z + 1)
        , (x + 1, y -1, z)
        , (x - 1, y + 1, z)
        ]
    coordEqns :: M.Map CubeCoord [CubeCoord]
    coordEqns = M.fromList $ fmap (\c -> (c, surrounding c)) allCoords
    mkEqn :: CubeCoord -> [Int]
    mkEqn c =
      -- TODO: we might want to do something more efficient than this.
      fmap (\c' -> if c' `elem` xs then 1 else 0) allCoords
      where
        xs :: [CubeCoord]
        xs = coordEqns M.! c

sqCoords :: Int -> ([[Int]], [[(Int, Int)]])
sqCoords = gCoords (Proxy :: Proxy 'Square)

-- https://stackoverflow.com/a/67319945/315302
withShape
  :: forall r.
     PuzzleShape
  -> (forall k. (CoordSystem k, Ord (Coord k)) => Proxy k -> r)
  -> r
withShape sp a = case sp of
  Square -> a (Proxy @'Square)
  Hexagon -> a (Proxy @'Hexagon)
