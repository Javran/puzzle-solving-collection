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


{-
  The implementation is a bit involved when curCoord and holdCoord share a row or col.
  let's explain one case, and all other cases can be produced by mirror.

  Notation:
  - `C` for cur coord, `H` for hole coord, `M` for coord.
  - `X` is the tile `M` points at before this move.
    (note that we don't actually care about the number on `X`)
  - `<-- x -->`: there are x tiles in between.

  Say the move happens in a row, and hole is on the left side of cur tile.

     H           C
   ^     ^       ^  ^
   1     2       3  4

  There are 4 places M can go, which are marked above.
  (note that M=H is not possible)

  Note that in all cases H is always moved to M so we don't
  need to mention that.

  case 1: M is on the left side of H.

    before:
    X <-- u --> H <-- v --> C
    ^
    M

    after:
    H X <-- u --> <-- v --> C
    ^
    M

    therefore no change to C

  case 2: M is between H and C

    before:
    H <-- u --> X <-- v --> C
                ^
                M

    after:
    <-- u --> X H <-- v --> C
                ^
                M

    therefore no change to C

  case 3: M = C

    before:
    H <-- u --> C
                ^
                M

    after
    <-- u --> C H
                ^
                M

    therefore C moved one unit towards left

  case 3: M is on the right side of C

    before:
    H <-- u --> C <-- v --> X
                            ^
                            M

    after
    <-- u --> C <-- v --> X H
                            ^
                            M

    therefore C moved one unit towards left

 -}
makeMove :: SimplePartialBoard -> Coord -> Maybe SimplePartialBoard
makeMove (curCoord@(cR, cC), holeCoord@(hR, hC)) mCoord@(mR, mC)
  | curCoord == holeCoord =
    -- impossible case
    Nothing
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
        | otherwise ->
          -- the move is orthogonal
          Just (curCoord, mCoord)
  | mC == hC =
    -- same col
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
        | otherwise ->
          -- the move is orthogonal
          Just (curCoord, mCoord)
  | otherwise = error "unreachable"
