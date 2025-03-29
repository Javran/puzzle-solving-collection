module Main
  ( main
  )
where

import Game.Arrow.Parser
import Game.Arrow.Solver
import System.Environment
import System.Exit

main :: IO ()
main =
  getArgs >>= \case
    "stdin" : _ -> do
      raw <- getContents
      let parsed = fromRawString raw
      case parsed of
        Just pz -> do
          case solve pz of
            Left e -> print e
            Right xs ->
              mapM_ (putStrLn . unwords . fmap show) xs
        _ -> error "TODO"
    xs -> do
      putStrLn $ "Unknown: " <> show xs
      exitFailure
