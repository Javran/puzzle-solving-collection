module Main
  ( main,
  )
where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

type Coord = (Int, Int)

type Offset = (Int, Int)

type MinePlacement = S.Set Offset

{- ORMOLU_DISABLE -}
-- 2d offset of 8 surrounding tiles.
-- sorted so that we can use efficient ways of Set construction.
surroundings ::  [Offset]
surroundings = sort
  [ (-1, -1), (-1, 0), (-1, 1)
  , (0, -1), (0, 1)
  , (1, -1), (1, 0), (1, 1)
  ]
{- ORMOLU_ENABLE -}

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
pickInOrder :: [a] -> [] (a, [a])
pickInOrder xs = do
  (y : ys) <- tails xs
  pure (y, ys)
{-# INLINEABLE pickInOrder #-}

genPlacement :: Int -> [MinePlacement]
genPlacement n0 = fmap S.fromDistinctDescList $ genAux n0 [] surroundings
  where
    genAux 0 selected _ = pure selected
    genAux n selected candidates = do
      (s, candidates') <- pickInOrder candidates
      genAux (n -1) (s : selected) candidates'

-- every number tile will be initialized with a list of MinePlacements from here.
placementTable :: V.Vector [MinePlacement]
placementTable = V.fromList $ fmap genPlacement [0 .. 8]

main :: IO ()
main = do
  print surroundings
  print (genPlacement 7)
