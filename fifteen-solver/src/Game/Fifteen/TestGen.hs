{-# LANGUAGE NamedFieldPuns #-}

module Game.Fifteen.TestGen where

{-
  Generate random boards for testing and benchmarking.
 -}

import Control.Monad
import Data.List
import qualified Data.List.Split
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.Solvability
import Game.Fifteen.Types
import Test.QuickCheck

genBoardOfSize :: Int -> Gen Board
genBoardOfSize sz = do
  let lastNum = sz * sz -1
      tr x = if x == lastNum then Nothing else Just x
  xs <- shuffle [0 .. lastNum]
  pure $ mkBoard $ (fmap . fmap) tr $ Data.List.Split.chunksOf sz xs

genSolvableBoardOfSize :: Int -> Gen Board
genSolvableBoardOfSize sz = do
  bd <- genBoardOfSize sz
  pure $
    if isSolvable bd
      then bd
      else flipSolvability bd

testGen :: IO ()
testGen = do
  let renderBoard :: (UUID.UUID, Board) -> [String]
      renderBoard (tag, Board {bdSize, bdTiles}) = commentLine : fmap renderRow tiles
        where
          renderTile m = case m of
            Nothing -> "_"
            Just v -> show v
          renderRow = unwords . fmap renderTile
          commentLine = "# " <> UUID.toString tag
          tiles = Data.List.Split.chunksOf bdSize $ V.toList bdTiles
      genTestBoard sz =
        (,) <$> UUID.nextRandom
          <*> generate (genSolvableBoardOfSize sz)

  pairs <- fmap concat <$> forM [3 .. 32] $ \sz ->
    (replicateM 5 (genTestBoard sz))
  mapM_ putStrLn $ concatMap renderBoard pairs
