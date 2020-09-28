{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main
  )
where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Vector as V

type Coord = (Int, Int) -- (row, col)

data Board = Board
  { bdSize :: Int
  , bdNums :: V.Vector Coord -- size of this vector must be bdSize*bdSize-1
  , bdHole :: Coord -- position of the hole
  , bdTiles :: V.Vector (Maybe Int) -- tiles, row-major order.
  }
  deriving (Show)

{-
  Note that number starts from 0, this is weird but
  allows vector indexing to be a bit more convenient.
  417
  _52
  603
 -}
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

demo0 :: [[Maybe Int]]
demo0 =
  [ [Just 4, Just 1, Just 7]
  , [Nothing, Just 5, Just 2]
  , [Just 6, Just 0, Just 3]
  ]

main :: IO ()
main = print (mkBoard demo0)
