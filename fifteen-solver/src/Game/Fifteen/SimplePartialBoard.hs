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
