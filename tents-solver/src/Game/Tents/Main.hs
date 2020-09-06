module Game.Tents.Main
  ( main
  )
where

import System.Console.Terminfo
import Game.Tents.Parser
import Game.Tents.Solver

main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just br = parseBoard rawPuzzle1
      Just bd = mkBoard br
      Just bd' = solve bd
  pprBoard term bd'
