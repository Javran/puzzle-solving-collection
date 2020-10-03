module Game.Torus.Main
  ( main
  )
where

import Control.Monad
import Game.Torus.Amano
import Game.Torus.Board
import Game.Torus.Parser
import System.Exit
import System.IO

solveAndVerifyFromRaw :: String -> IO ([Move], (Board, Board))
solveAndVerifyFromRaw raw = do
  let parsed = parseBoard raw >>= mkBoard
  case parsed of
    Nothing -> do
      hPutStrLn stderr "Failed during parsing"
      exitFailure
    _ -> pure ()
  let Just bd = parsed
      (moves, bd') = solveBoard bd
      moves' = simplifyMoves bd moves
  unless (bd' == applyMoves bd moves') $ do
    hPutStrLn stderr "simplification is not valid."
    exitFailure
  unless (isSolved bd') $ do
    hPutStrLn stderr "board is not in the solved state after moves."
    exitFailure
  pure (moves', (bd, bd'))

main :: IO ()
main = do
  solveAndVerifyFromRaw demo0
  solveAndVerifyFromRaw demo1
  solveAndVerifyFromRaw demo2
  putStrLn "All verified."
