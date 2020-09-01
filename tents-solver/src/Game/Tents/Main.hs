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
  let Just br = parseBoard rawPuzzle0
      Just bd = mkBoard br
      Just bd' = tentRepel bd (8,0)
  pprBoard term bd'
