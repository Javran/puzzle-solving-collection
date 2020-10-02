{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Game.Torus.Main
  ( main
  )
where

import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V
import Game.Torus.Parser

data Board = Board
  { bdDims :: (Int, Int)
  , -- row*col, row-major.
    bdTiles :: V.Vector Int
  }
  deriving (Show)

type Coord = (Int, Int)

{-
  mIndex describes which row or col it's operation on
  mStep is, well, steps of the move.

  We only need left and up because for a Board of X rows,

  MLeft r u == MRight r (X-u)

  something similar applies to columns too.

  we can go further to enforce that:
  - 0 <= mIndex < rows for MLeft
  - 0 <= mIndex < cols for MRight
  so that all moves except no-op are uniquely represented.
 -}
data Move
  = MoveLeft {mIndex :: Int, mStep :: Int}
  | MoveUp {mIndex :: Int, mStep :: Int}

-- smart constructors. avoid using MoveLeft or MoveRight directly.
moveLeft, moveRight, moveUp, moveDown :: Board -> Int -> Int -> Move
moveLeft Board {bdDims = (rows, _)} i s =
  MoveLeft i (s `mod` rows)
moveRight Board {bdDims = (rows, _)} i s =
  MoveLeft i ((- s) `mod` rows)
moveUp Board {bdDims = (_, cols)} i s =
  MoveUp i (s `mod` cols)
moveDown Board {bdDims = (_, cols)} i s =
  MoveUp i ((- s) `mod` cols)

{-
  basic operation of rotating a list towards left,
  e.g. rotateLeft 3 "ABCDE" == "DEABC"

  only works when n >= 0.
 -}
rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = fmap fst $ zip (drop n (cycle xs)) xs

mkBoard :: BoardRep -> Maybe Board
mkBoard (bdDims@(rows, cols), tiles) = do
  let flat = concat tiles
  guard $ length tiles == rows
  guard $ all ((== cols) . length) tiles
  guard $ S.fromList flat == S.fromList [1 .. rows * cols]
  pure
    Board
      { bdDims
      , bdTiles =
          -- note that after verification
          -- we have numbers shifted to be 0-based,
          -- as it's a bit nicer to work with.
          V.fromListN (rows * cols) $ fmap pred flat
      }

bdGet :: Board -> Coord -> Int
bdGet Board {bdDims = (_, cols), bdTiles} (r, c) =
  bdTiles V.! (r * cols + c)

pprBoard :: Board -> IO ()
pprBoard bd@Board {bdDims = (rows, cols)} = do
  let maxLen = length (show $ rows * cols)
      renderTile n = replicate (maxLen - length content) ' ' <> content
        where
          content = show n
      printSep lS midS rS =
        putStrLn $
          concat
            [ lS
            , "═"
            , intercalate ("═" <> midS <> "═") $
                replicate cols (replicate maxLen '═')
            , "═"
            , rS
            ]

  printSep "╔" "╦" "╗"
  forM_ [0 .. rows -1] $ \r -> do
    let lineTiles = fmap (bdGet bd . (r,)) [0 .. cols -1]
    putStrLn $
      "║ " <> intercalate " ║ " (fmap (renderTile . succ) lineTiles)
        <> " ║ "
        <> show r
    if r < rows -1
      then printSep "╠" "╬" "╣"
      else printSep "╚" "╩" "╝"
  putStrLn $ drop 1 $ concatMap (("   " <>) . renderTile) [0 .. cols -1]

main :: IO ()
main = do
  let Just bd = parseBoard demo0 >>= mkBoard
  pprBoard bd
