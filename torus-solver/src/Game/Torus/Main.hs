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
  pprBoard (applyMoves bd' $ terW 5 5 1 1 <> terW 5 1 1 1 <> terW 5 3 1 1 <> [east 5 1])
