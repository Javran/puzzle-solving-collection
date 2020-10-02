module Game.Torus.Main
  ( main
  )
where

import Game.Torus.Board
import Game.Torus.Parser

main :: IO ()
main = do
  let Just bd = parseBoard demo0 >>= mkBoard
      bd1 = applyMove bd (MoveLeft 3 2)
      bd2 = applyMove bd1 (MoveDown 5 4)
  pprBoard bd
  pprBoard bd1
  pprBoard bd2
