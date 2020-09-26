module Game.Tents.BatchSolve where

import Game.Tents.Parser

batchSolve :: FilePath -> IO ()
batchSolve fp = do
  raw <- readFile fp
  let boards = parseBatchBoards raw
  putStrLn $ "Parsed " <> show (length boards) <> " boards from this batch."
  print boards
  -- TODO: perform solving
