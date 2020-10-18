{-# LANGUAGE MultiWayIf #-}

module Game.Fifteen.SimplePartialBoard where

import Control.Monad
import qualified Data.DList as DL
import Data.Maybe
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S
import Game.Fifteen.Human
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
        | hR /= cR ->
          -- curCoord and holeCoord is not on the same row
          Just (curCoord, mCoord)
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
          error "unreachable"
  | mC == hC =
    -- same col
    if
        | hC /= cC ->
          -- curCoord and holeCoord is not on the same col
          Just (curCoord, mCoord)
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
          error "unreachable"
  | otherwise = error "unreachable"

type PQ = PQ.MinPQueue Int PQElem

type PQElem = (SPBoard, Int {- length of moves -}, DL.DList Coord)

{-
  Search to find a way inside boundingRect, that avoids all coords in pCoords
  and moves tile in srcCoord to dstCoord.
 -}
searchMoveTile :: Rect -> S.Set Coord -> Coord -> Coord -> Coord -> Maybe [Coord]
searchMoveTile boundingRect pCoords srcCoord initHoleCoord dstCoord =
  doSearch initQ S.empty
  where
    initQ = PQ.singleton (distance srcCoord dstCoord) ((srcCoord, initHoleCoord), 0, DL.empty)
    ((minR, minC), (maxR, maxC)) = boundingRect
    doSearch :: PQ -> S.Set SPBoard -> Maybe [Coord]
    doSearch q visited = do
      ((spBoard@(curCoord, holeCoord), moveLen, moves), todos) <- PQ.minView q
      if
          | spBoard `elem` visited ->
            doSearch todos visited
          | curCoord == dstCoord ->
            pure (DL.toList moves)
          | otherwise -> do
            let (hR, hC) = holeCoord
                ext :: PQ
                ext = PQ.fromList $ do
                  {-
                    a move is allowed if and only if
                    it's within boundingRect and is not in pCoords.
                    note that the definition does not involve the validity of a move,
                    which should be taken care of by `applyMove`.
                   -}
                  move <- [(hR, c) | c <- [minC .. maxC]] <> [(r, hC) | r <- [minR .. maxR]]
                  guard $ move `notElem` pCoords
                  spBoard'@(curCoord', _) <- maybeToList (applyMove spBoard move)
                  let dist' = distance curCoord' dstCoord
                  pure (dist' + moveLen + 1, (spBoard', moveLen + 1, DL.snoc moves move))
            doSearch (PQ.union todos ext) (S.insert spBoard visited)
