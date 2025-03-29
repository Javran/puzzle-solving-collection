module Game.Tents.BatchPick where

import Control.Monad.Except
import Control.Monad.IO.Class
import Game.Tents.Parser
import Game.Tents.Solver
import Game.Tents.Types
import Game.Tents.Utils
import System.Console.Terminfo
import System.Exit

batchPick :: Terminal -> FilePath -> String -> IO ()
batchPick term fp puzzleId = do
  raw <- readFile fp
  let boards = parseBatchBoards raw
  case lookup puzzleId boards of
    Nothing -> do
      putStrLn "puzzle not found."
      exitFailure
    Just br -> do
      () <$ runExceptT (processBoard br)
  where
    processBoard :: BoardRep -> ExceptT String IO ()
    processBoard br = do
      bd0 <- m2e "Board failed to initialize." $ mkBoard br
      bd1 <- m2e "Failure while solving." $ solve bd0
      liftIO $
        if (isSolved bd1)
          then putStrLn "This puzzle is solved without problem."
          else do
            putStrLn "This puzzle is not solved yet."
            pprBoard term bd1
