module Game.Torus.Main
  ( main
  )
where

import Game.Torus.Amano
import Game.Torus.Board
import Game.Torus.Parser

main :: IO ()
main = do
  let Just bd = parseBoard demo0 >>= mkBoard
  pprBoard bd
  pprBoard
    (applyMoves
       bd
       $ ccwA 3 0 3 4
         <> ccwA 4 1 4 3
         <> ccwB 0 2 1 3
         <> ccwD 4 3 4 1
         <> cwA 4 4 4 1
         <> cwD 3 5 3 4)
