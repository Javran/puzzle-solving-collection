{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  )
where

import Game.Arrow.CoordSystem
import Game.Arrow.Parser
import Game.Arrow.Solver
import Game.Arrow.Types
import System.Environment
import System.Exit

main :: IO ()
main =
  getArgs >>= \case
    "stdin" : _ -> do
      raw <- getContents
      let parsed = fromRawString raw
      case parsed of
        Just pz@Puzzle {pzType = (ps, n)} -> do
          case solve pz of
            Left e -> print e
            Right xs ->
              mapM_ (putStrLn . unwords . fmap show) $ withShape ps (\pty -> toChunks pty n xs)
        _ -> error "TODO"
    xs -> do
      putStrLn $ "Unknown: " <> show xs
      exitFailure
