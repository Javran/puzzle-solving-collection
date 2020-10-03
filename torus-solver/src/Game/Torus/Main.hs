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
      (moves, bd') = solveBoard bd
  pprBoard bd
  putStrLn $ "Moves: " <> show (length moves)
  pprBoard bd'
