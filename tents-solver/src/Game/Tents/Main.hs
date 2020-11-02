module Game.Tents.Main
  ( main
  )
where

import Game.Tents.BatchPick
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
      let Just br = parseBoard rawPuzzle2
          Just bd = mkBoard br
          Just bd' = solve bd
      -- pprBoard term bd'
      loadPuzzles >>= mapM_ print
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
    ["batch-pick", fPath, puzzleId] -> batchPick term fPath puzzleId
    _ -> do
      hPutStrLn stderr "solver <no arg>: execute builtin puzzle"
      hPutStrLn stderr "solver stdin: take one puzzle from stdin, return tent positions."
      hPutStrLn stderr "solver batch-reorg <file>: reorganize a bundle of puzzles."
      hPutStrLn stderr "solver batch-solve <file>: try to solve a bundle of puzzles."
      hPutStrLn stderr "solver batch-pick <file> <puzzle-id>: pick one puzzle to solve."
      exitFailure
