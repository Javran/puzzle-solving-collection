module Main
  ( main
  )
where

import System.Console.Terminfo
import Parser
import Solver

main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just br = parseBoard rawPuzzle0
      Just bd = mkBoard br
  pprBoard term bd
