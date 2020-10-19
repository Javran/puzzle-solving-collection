{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Game.Fifteen.TestGen where

{-
  Generate random boards for testing and benchmarking.
 -}

import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.List
import qualified Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Traversable
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as V
import Game.Fifteen.Board
import Game.Fifteen.Human
import Game.Fifteen.Solvability
import Game.Fifteen.Types
import System.Environment
import System.FilePath.Posix
import System.IO
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

testGenBundle :: IO ()
testGenBundle = do
  let renderBoard :: (UUID.UUID, Board) -> [String]
      renderBoard (tag, Board {bdSize, bdTiles}) = commentLine : fmap renderRow tiles
        where
          renderTile m = case m of
            Nothing -> "_"
            Just v -> show (v + 1)
          renderRow = unwords . fmap renderTile
          commentLine = "# " <> UUID.toString tag
          tiles = Data.List.Split.chunksOf bdSize $ V.toList bdTiles
      genTestBoard sz =
        (,) <$> UUID.nextRandom
          <*> generate (genSolvableBoardOfSize sz)
  pairs <- fmap concat <$> for [3 .. 12] $ \sz -> replicateM 5 (genTestBoard sz)
  mapM_ putStrLn $ concatMap renderBoard pairs

loadPuzzleBundle :: FilePath -> IO (M.Map String Board)
loadPuzzleBundle fp = do
  rawLines <- lines <$> readFile fp
  let rawPairs :: [(String, [String])]
      rawPairs =
        extract $
          groupBy ((==) `on` isCommentLine) rawLines
        where
          extract :: [] [String] -> [(String, [String])]
          extract ([c] : xs : ys) = (drop 2 c, xs) : extract ys
          extract [] = []
          extract _ = error "input file is ill-formed"
          isCommentLine = (== "# ") . take 2
  pure . M.fromList . (fmap . second) (fromJust . mkBoardFromRaw . unlines) $ rawPairs

loadPuzzleBundleMoves :: FilePath -> IO (M.Map String Int)
loadPuzzleBundleMoves fp = do
  xs <- lines <$> readFile fp
  let parseRawLine ys = (boardId, read countRaw)
        where
          boardId : countRaw : _ = Data.List.Split.splitOn ", " ys
  pure . M.fromList . fmap parseRawLine $ xs

testGenSolveAll :: FilePath -> IO ()
testGenSolveAll fpPuzzle = do
  let (fpBase, ext) = splitExtension fpPuzzle
      fpPuzzleMoves = (fpBase <> "-moves") <.> ext
  boards <- loadPuzzleBundle fpPuzzle
  let solveFromRaw :: (String, Board) -> (String, Maybe Int)
      solveFromRaw (boardId, bd) = (boardId,) $ do
        moves <- listToMaybe $ solveBoard bd
        pure (length moves)
      (unsolvedResults, solvedResults) =
        partitionEithers . fmap (toEither . solveFromRaw) . M.toList $ boards
        where
          toEither (boardId, m) = case m of
            Nothing -> Left boardId
            Just v -> Right (boardId, v)
  hPutStrLn stderr $ "Puzzles found: " <> show (length boards)
  hPutStrLn stderr $ "Solved: " <> show (length solvedResults) <> ", unsolved: " <> show (length unsolvedResults)
  mKnownMoves <- try @IOException $ loadPuzzleBundleMoves fpPuzzleMoves
  let knownMoves :: M.Map String Int
      knownMoves = fromRight M.empty $ mKnownMoves
  forM_ solvedResults $ \(boardId, moveCount) -> do
    let mOldMoveCount = knownMoves M.!? boardId
        desc = case mOldMoveCount of
          Nothing -> show moveCount
          Just oldMoveCount ->
            show moveCount <> ", was " <> show oldMoveCount <> ", "
              <> case compare moveCount oldMoveCount of
                EQ -> "same"
                LT -> "less"
                GT -> "more"
    putStrLn $ boardId <> ", " <> desc
  pure ()

testGen :: IO ()
testGen = do
  args <- getArgs
  case args of
    ["testgen"] -> testGenBundle
    ["testgen", fPath] -> testGenSolveAll fPath
    _ -> error "unexpected args"
