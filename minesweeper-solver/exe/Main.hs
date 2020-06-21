module Main (main) where

import Game.Minesweeper.Parser
import Game.Minesweeper.Pretty
import Game.Minesweeper.Solver
import System.Console.Terminfo
import System.Environment

main :: IO ()
main = do
  term <- setupTermFromEnv
  args <- getArgs
  raw <- case args of
    [fs] -> readFile fs
    _ -> pure sampleRaw

  case solveBoardFromRaw raw of
    Nothing -> putStrLn "Nothing"
    Just bdFin ->
      pprBoard term True bdFin
