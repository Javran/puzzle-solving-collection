{-# LANGUAGE MultiWayIf #-}

module Game.Fifteen.SimplePartialBoard where

import Game.Fifteen.Types

{-
  A SimplePartialBoard is simply
  a tile coord and the current coord of the hole
  in the context of a normal Board.

  When we want to move a tile to certain places,
  the state of the Board can be simplified into this
  as we really care no tile but those two.
  (Those moves have to avoid moving certain tiles
  that are already solved, but that can be easily checked prior to
  making this move)
 -}
type SimplePartialBoard = (Coord, Coord)

makeMove :: SimplePartialBoard -> Coord -> Maybe SimplePartialBoard
makeMove (curCoord@(cR, cC), holeCoord@(hR, hC)) mCoord@(mR, mC)
  | mCoord == holeCoord =
    -- invalid move if hole is already at that coord.
    Nothing
  | mR /= hR && mC /= hC =
    -- move must share either row or col with the hole
    Nothing
  | mR == hR =
    -- same row
    if
        | hC < cC ->
          -- hole is on the left side of cur tile.
          if mC < cC
            then Just (curCoord, mCoord)
            else Just ((cR, cC -1), mCoord)
        | cC < hC ->
          -- hole is on the right side of cur tile.
          if cC < mC
            then Just (curCoord, mCoord)
            else Just ((cR, cC + 1), mCoord)
        | otherwise -> error "unreachable"
  | mC == hC =
    if
        | hR < cR ->
          -- hole is on the up side of cur tile.
          if mR < cR
            then Just (curCoord, mCoord)
            else Just ((cR -1, cC), mCoord)
        | cR < hR ->
          -- hole is on the right side of cur tile.
          if cR < mR
            then Just (curCoord, mCoord)
            else Just ((cR + 1, cC), mCoord)
        | otherwise -> error "unreachable"
  | otherwise = error "unreachable"
