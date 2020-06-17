{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game.Minesweeper.Main where

import Control.Monad
import qualified Data.Map.Strict as M
import Game.Minesweeper.Parser
import Game.Minesweeper.Solver
import Game.Minesweeper.Types
import System.Environment

pprBoard :: Board -> IO ()
pprBoard bd@Board {bdDims = (rows, cols), bdNums, bdCandidates} = do
  putStrLn "===="
  forM_ [0 .. rows -1] $ \r -> do
    forM_ [0 .. cols -1] $ \c -> do
      let coord = (r, c)
      putStr $ case getTile bd coord of
        Nothing -> "?"
        Just b ->
          if b
            then "*"
            else maybe " " show (bdNums M.!? coord)

    putStrLn ""
  putStrLn "===="
  unless (M.null bdCandidates) $ do
    putStrLn "Candidates:"
    forM_ (M.toAscList bdCandidates) $
      \(coord, cs) -> do
        let n = bdNums M.! coord
        putStrLn $ show n <> " on " <> show coord <> ":"
        mapM_ (putStrLn . ("  " ++) . show) cs

main :: IO ()
main = do
  args <- getArgs
  raw <- case args of
    [fs] -> readFile fs
    _ -> pure sampleRaw
  let Just tmpBd = parseBoard raw
      Just (xs, bd) = mkBoard tmpBd
      Just bdFin = solveBoard bd xs
  pprBoard bdFin
