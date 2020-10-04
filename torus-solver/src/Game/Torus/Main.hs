module Game.Torus.Main
  ( main
  )
where

import Control.Monad
import Data.List
import Game.Torus.Amano
import Game.Torus.Board
import qualified Data.Vector as V
import Game.Torus.Parser
import System.Environment
import System.Exit
import System.IO

demo0 :: String
demo0 =
  unlines
    [ "6 6"
    , "25 22 19 31 27 3"
    , "15 32 9 33 16 14"
    , "8 29 10 28 11 18"
    , "23 6 34 21 1 26"
    , "24 5 35 4 2 36"
    , "20 17 7 13 30 12"
    ]

demo1 :: String
demo1 =
  unlines
    [ "3 7"
    , "21 20 19 18 17 16 15"
    , "14 13 12 11 10 9 8"
    , "7 6 5 4 3 2 1"
    ]

demo2 :: String
demo2 =
  unlines
    [ "4 6"
    , "24 23 22 21 20 19"
    , "17 18 16 15 14 12"
    , "13 11 10 9 8 7"
    , "5 6 4 2 3 1"
    ]

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
  args <- getArgs
  case args of
    [] -> do
      solveAndVerifyFromRaw demo0
      solveAndVerifyFromRaw demo1
      solveAndVerifyFromRaw demo2
      putStrLn "All verified."
    ["stdin"] -> do
      raw <- getContents
      (moves, _) <- solveAndVerifyFromRaw raw
      let moveToStr m = case m of
            MoveUp i s -> "u," <> show i <> "," <> show s
            MoveLeft i s -> "l," <> show i <> "," <> show s
      putStrLn $ intercalate "|" $ fmap moveToStr moves
