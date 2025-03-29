module Game.Tents.BatchSolve where

import Control.Monad
import Control.Monad.Except
import Game.Tents.Parser
import Game.Tents.Solver
import Game.Tents.Types
import Game.Tents.Utils

processBoard :: Monad m => BoardRep -> ExceptT String m ()
processBoard br = do
  bd0 <- m2e "Board failed to initialize." $ mkBoard br
  bd1 <- m2e "Failure while solving." $ solve bd0
  unless (isSolved bd1) $
    throwError "Result is incomplete."

batchSolve :: FilePath -> IO ()
batchSolve fp = do
  raw <- readFile fp
  let boards = parseBatchBoards raw
  putStrLn $ "Parsed " <> show (length boards) <> " boards from this batch."
  forM_ boards $ \(bId, br) -> do
    r <- runExceptT (processBoard br)
    case r of
      Left e ->
        putStrLn $ "Board " <> bId <> " failed due to reason: " <> e
      Right _ ->
        putStrLn $ "Board " <> bId <> " is successfully solved."
