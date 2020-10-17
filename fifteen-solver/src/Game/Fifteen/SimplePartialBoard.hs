{-# LANGUAGE MultiWayIf #-}

module Game.Fifteen.SimplePartialBoard where

import Game.Fifteen.Types
import qualified Data.Set as S
import Game.Fifteen.Human
import Control.Monad

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
type SPBoard = (Coord, Coord)

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

  To summarize:

  - no complication if:
    + H and C does not share row or col.
    + if H and C share a row but the move happens in col
    + if H and C share a col but the move happens in row
  - if H and C share a row or col:
    let's say moving from H to C is the "positive direction".
    if moving from C to M is "non-negative" (i.e. C=M or M is more "positive" than C),
    then C needs to be shifted one unit in negative direction,
    otherwise there is no complication

 -}
applyMove :: SPBoard -> Coord -> Maybe SPBoard
applyMove (curCoord@(cR, cC), holeCoord@(hR, hC)) mCoord@(mR, mC)
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

{-
  Search to find a way inside boundingRect, that avoids all coords in pCoords
  and moves tile in srcCoord to dstCoord.
 -}
searchMoveTile :: Rect -> S.Set Coord -> Coord -> Coord -> Coord -> [] [Coord]
searchMoveTile boundingRect pCoords srcCoord initHoleCoord dstCoord =
  doSearch [((srcCoord, initHoleCoord), [])] S.empty
  where
    ((minR, minC), (maxR, maxC)) = boundingRect
    doSearch :: [] (SPBoard, [Coord]) -> S.Set SPBoard -> [] [Coord]
    doSearch [] _ = []
    doSearch ((coordPair, revMoves) : todos) visited =
      if coordPair `elem` visited
        then doSearch todos visited
        else do
          let (curCoord, holeCoord) = coordPair
          if curCoord == dstCoord
            then pure (reverse revMoves)
            else do
              let directTileMoves = do
                    guard $ distance curCoord holeCoord == 1
                    -- tap current coord, effectively swaping the state pair.
                    pure ((holeCoord, curCoord), curCoord : revMoves)
                  holeMoves = do
                    -- TODO: well this turns out to be complicated when curCoord and holeCoord share
                    -- a row or col.
                    undefined
                  extraTodos = directTileMoves <> holeMoves
              doSearch (todos <> extraTodos) (S.insert coordPair visited)
