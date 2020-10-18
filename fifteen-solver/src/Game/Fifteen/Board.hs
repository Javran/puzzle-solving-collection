{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Fifteen.Board where

import Control.Monad
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import qualified Data.Vector as V
import Game.Fifteen.Types

mkBoard :: [[Maybe Int]] -> Board
mkBoard tileSource =
  Board
    { bdSize
    , bdTiles
    , bdNums
    , bdHole
    }
  where
    bdSize = length tileSource
    szSq = bdSize * bdSize
    allCoords = [(r, c) | r <- [0 .. bdSize -1], c <- [0 .. bdSize -1]]
    pairs = zip allCoords (concat tileSource)
    ([(bdHole, _)], digitPairsPre) =
      partition (isNothing . snd) pairs
    digitPairs :: [(Int, Coord)]
    digitPairs = fmap (swap . second fromJust) digitPairsPre
    bdTiles = V.fromListN szSq $ concat tileSource
    bdNums = V.replicate (szSq -1) undefined V.// digitPairs

mkBoardFromRaw :: String -> Maybe Board
mkBoardFromRaw = fmap mkBoard . parseRaw

isSolved :: Board -> Bool
isSolved Board {bdTiles, bdSize, bdHole} =
  bdHole == (bdSize -1, bdSize -1)
    && and (zipWith (==) (V.toList bdTiles) (Just <$> [0 .. bdSize * bdSize -2]))

demo0Raw :: String
demo0Raw =
  unlines
    [ "5 2 8"
    , "_ 6 3"
    , "7 1 4"
    ]

demo1Raw :: String
demo1Raw =
  unlines
    [ "16 1 24 18 13"
    , "21 3 19 6 9"
    , "8 17 10 23 2"
    , "5 20 11 _ 22"
    , "7 4 12 14 15"
    ]

demo0, demo1 :: Board
Just demo0 = mkBoardFromRaw demo0Raw
Just demo1 = mkBoardFromRaw demo1Raw

parseRaw :: String -> Maybe [[Maybe Int]]
parseRaw raw = do
  let rawLines = lines raw
      size = length rawLines
      convertLine rawLine = do
        let rawTiles = words rawLine
            convertTile xs = case xs of
              "_" -> pure Nothing
              _
                | [(v, "")] <- reads xs ->
                  -- note that number starts from 0,
                  -- this is admittedly weird but allows
                  -- vector indexing to be more convenient.

                  pure (Just (pred v))
              _ -> Nothing
        guard $ length rawTiles == size
        mapM convertTile rawTiles
  mapM convertLine rawLines

bdIndex :: Board -> Coord -> Int
bdIndex Board {bdSize} (r, c) = c + bdSize * r

bdGet :: Board -> Coord -> Maybe Int
bdGet bd@Board {bdTiles} coord = bdTiles V.! bdIndex bd coord

data Dir = DUp | DDown | DLeft | DRight

applyDir :: Coord -> Dir -> Coord
applyDir (r, c) d = case d of
  DUp -> (r -1, c)
  DDown -> (r + 1, c)
  DLeft -> (r, c -1)
  DRight -> (r, c + 1)

bdInRange :: Board -> Coord -> Bool
bdInRange Board {bdSize} (r, c) =
  r >= 0 && r < bdSize && c >= 0 && c < bdSize

bdMoveHole :: Board -> Dir -> Maybe Board
bdMoveHole bd@Board {bdHole, bdNums, bdTiles} d = do
  let bdHole' = applyDir bdHole d
  guard $ bdInRange bd bdHole'
  let tn@(Just tileNum) = bdGet bd bdHole'
  pure
    bd
      { bdTiles =
          bdTiles
            V.// [ (bdIndex bd bdHole', Nothing)
                 , (bdIndex bd bdHole, tn)
                 ]
      , bdNums = bdNums V.// [(tileNum, bdHole)]
      , bdHole = bdHole'
      }

possibleMoves :: Board -> [(Coord, Board)]
possibleMoves bd =
  foldr
    (\d m -> m <> movesInOneDir d)
    []
    [DUp, DDown, DLeft, DRight]
  where
    movesInOneDir :: Dir -> [(Coord, Board)]
    movesInOneDir d = unfoldr go bd
      where
        go curBd = do
          bd' <- bdMoveHole curBd d
          pure ((bdHole bd', bd'), bd')

applyMoves :: Board -> [Coord] -> Maybe Board
applyMoves bd xs = case xs of
  [] -> Just bd
  m : ms -> do
    bd' <- lookup m (possibleMoves bd)
    applyMoves bd' ms

-- https://en.wikipedia.org/wiki/Box-drawing_character
pprBoard :: Board -> IO ()
pprBoard bd@Board {bdSize} = do
  let maxLen = length (show $ bdSize * bdSize - 1)
      renderTile Nothing = replicate maxLen ' '
      renderTile (Just v) = replicate (maxLen - length content) ' ' <> content
        where
          content = show v
      printSep lS midS rS =
        putStrLn $
          concat
            [ lS
            , "═"
            , intercalate ("═" <> midS <> "═") $
                replicate bdSize (replicate maxLen '═')
            , "═"
            , rS
            ]

  printSep "╔" "╦" "╗"
  forM_ [0 .. bdSize -1] $ \r -> do
    let lineTiles = fmap (bdGet bd . (r,)) [0 .. bdSize -1]
    putStrLn $ "║ " <> intercalate " ║ " (fmap (renderTile . fmap succ) lineTiles) <> " ║ " <> show r
    if r < bdSize -1
      then printSep "╠" "╬" "╣"
      else printSep "╚" "╩" "╝"
  putStrLn $ drop 1 $ concatMap (("   " <>) . renderTile . Just) [0 .. bdSize -1]

mkGoalBoard :: Int -> Board
mkGoalBoard sz =
  mkBoard $ chunksOf sz $ fmap Just [0 .. sz * sz -2] <> [Nothing]

pprSteps :: Board -> [Coord] -> IO (Maybe Board)
pprSteps initBd allMoves = do
  putStrLn "Initial board:"
  pprBoard initBd
  pprStepsAux (1 :: Int) initBd allMoves
  where
    pprStepsAux _ bd [] = Just bd <$ putStrLn "Done."
    pprStepsAux step bd (coord : ms) = do
      putStrLn $ "Step #" <> show step <> ": " <> show coord
      case lookup coord (possibleMoves bd) of
        Nothing -> do
          putStrLn $ "This move cannot be performed, aborted."
          pure Nothing
        Just nextBd -> do
          pprBoard nextBd
          pprStepsAux (step + 1) nextBd ms

distance :: Coord -> Coord -> Int
distance (a, b) (c, d) = abs (a - c) + abs (b - d)
