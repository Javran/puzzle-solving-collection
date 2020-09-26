module Game.Tents.Main
  ( main
  )
where

import Game.Tents.BatchReorganize
import Game.Tents.BatchSolve
import Game.Tents.Parser
import Game.Tents.Solver
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  term <- setupTermFromEnv
  case args of
    [] -> do
      -- give no argument to test builtin puzzles
      let Just br = parseBoard rawPuzzle4
          Just bd = mkBoard br
          Just bd' = solve bd
      pprBoard term bd'
    ["stdin"] -> do
      -- stdin mode reads data from stdin
      -- and output tent positions in a format that can be easily parsed.
      raw <- getContents
      Just br <- pure (parseBoard raw)
      hPutStrLn stderr "Parse succeeded."
      Just bd <- pure (mkBoard br)
      hPutStrLn stderr "Board creation succeeded."
      Just bd' <- pure (solve bd)
      hPutStrLn stderr "Solver executed."
      printTentPositions bd'
    ["batch-reorg", fPath] -> batchReorganize fPath
    ["batch-solve", fPath] -> batchSolve fPath
    _ -> do
      hPutStrLn stderr "solver <no arg>: execute builtin puzzle"
      hPutStrLn stderr "solver stdin: take one puzzle from stdin, return tent positions."
      hPutStrLn stderr "solver batch-reorg: reorganize a bundle of puzzles."
      exitFailure
