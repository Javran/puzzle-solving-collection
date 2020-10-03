module Game.Torus.Main
  ( main
  )
where

import Control.Monad
import Game.Torus.Amano
import Game.Torus.Board
import Game.Torus.Parser
import System.Exit

main :: IO ()
main = do
  let Just bd = parseBoard demo1 >>= mkBoard
      (moves, bd') = solveBoard bd
      moves' = simplifyMoves bd moves
  pprBoard bd
  putStrLn $ "Moves: " <> show (length moves)
  putStrLn $ "Moves after simplification: " <> show (length moves')
  pprBoard bd'
  unless (bd' == applyMoves bd moves') $ do
    putStrLn "simplification is not valid."
    exitFailure
