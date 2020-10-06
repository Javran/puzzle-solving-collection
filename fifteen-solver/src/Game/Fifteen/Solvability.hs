{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Fifteen.Solvability where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Game.Fifteen.Types

{-
  Reference: https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html

  TODO: I'm still a bit doubtful about the empty cell bit, however:
  - why does it matter whether we count from *row*, rather than *colum*?
  - why does it matter we count from last row?

 -}

{-
  Actually, this is inversion counting and mergeSort is just a side-effect.
 -}
mergeSortFromListNImpl :: forall a s. Ord a => Int -> [a] -> WriterT (Sum Int) (ST s) (V.Vector a)
mergeSortFromListNImpl n xs = do
  -- actually safe to do as this is the only function holding this `v` reference.
  v <- V.unsafeThaw (V.fromListN n xs)
  tmp <- VM.unsafeNew (VM.length v)
  let sortAux l r =
        -- ranges are inclusive.
        unless (l >= r) $ do
          let mid = (l + r) `quot` 2
          -- recursively sort two parts: [l ... mid] and [mid+1 .. r],
          sortAux l mid
          sortAux (mid + 1) r
          -- the merge step. indices are always pointing to first elements that are not read from / written to.
          (leftoverInd, tmpInd0) <-
            fix
              (\loop lInd rInd tmpInd ->
                 if
                     | lInd > mid ->
                       pure (rInd, tmpInd)
                     | rInd > r ->
                       pure (lInd, tmpInd)
                     | otherwise -> do
                       vL <- VM.unsafeRead v lInd
                       vR <- VM.unsafeRead v rInd
                       if vL <= vR
                         then do
                           -- insert head element from left
                           VM.write tmp tmpInd vL
                           loop (lInd + 1) rInd (tmpInd + 1)
                         else do
                           -- insert head element from right
                           VM.write tmp tmpInd vR
                           -- count # of inversions, which is just length of the left.
                           tell $ Sum (mid - lInd + 1)
                           loop lInd (rInd + 1) (tmpInd + 1))
              l
              (mid + 1)
              l
          fix
            (\loop xInd tmpInd ->
               unless (tmpInd > r) $ do
                 VM.unsafeRead v xInd >>= VM.unsafeWrite tmp tmpInd
                 loop (xInd + 1) (tmpInd + 1))
            leftoverInd
            tmpInd0
          forM_ [l .. r] $ \i ->
            VM.unsafeRead tmp i >>= VM.unsafeWrite v i
  sortAux 0 (VM.length v -1)
  V.unsafeFreeze v

mergeSortFromListN :: forall a. Ord a => Int -> [a] -> (V.Vector a, Int)
mergeSortFromListN n xs =
  second getSum $ runST $ runWriterT $ mergeSortFromListNImpl n xs

bdParity :: Board -> Bool
bdParity Board {bdSize, bdTiles} = odd count
  where
    (_, count) =
      mergeSortFromListN (bdSize * bdSize -1) $
        catMaybes $ V.toList bdTiles

-- TODO: to be tested.
{-
  TODO: now I think I understands what wikipedia is saying:
  - if we treat the empty tile as tile bdSize*bdSize, we actually can compute the parity of permutation
    of the whole board, rather than just bdSize*bdSize-1 tiles.
  - now it makes sense:
    - every move "moves" the empty tile, which means the parity of its row+col changes
      (we are counting from bottom-right because it is empty tile's "home")
    - a left or right move changes the number of inversions by one, therefore flips parity
    - an up or down move changes ... well this is the tricky bit, as it swaps two tiles
      and I haven't figured it out why the parity changes.
 -}
isSolvable :: Board -> Bool
isSolvable bd@Board {bdSize, bdHole = (row, _c)} =
  if odd bdSize
    then -- If the grid width is odd, then the number of inversions in a solvable situation is even.
      not bp
    else
      if {-
           it's tricky to get it right since we are doing zero-based indexing, so let's do an example:
           Suppose we have a board of bdSize = 6, we have row #0, #1, #2, #3, #4, #5
           If we do `bdSize-row` to count from the bottom row:

           - last row is #5, 6-5=1, odd
           - second-last row is #4, 6-4=2, even

           But bdSize is always an even number in this branch, it doesn't affect parity in any way,
           so we can just test the parity of row, which should give the same result.
          -}
      even row
        then {-
                If the grid width is even, and the blank is on an even row
                counting from the bottom (second-last, fourth-last etc),
                then the number of inversions in a solvable situation is odd.
              -}
          not bp
        else {-
                If the grid width is even, and the blank is on an odd row
                counting from the bottom (last, third-last, fifth-last etc)
                then the number of inversions in a solvable situation is even.
             -}
          bp
  where
    bp = bdParity bd

testMergeSort :: IO ()
testMergeSort = do
  let src :: [Int]
      src = [27, 79, 25, 6, 33, 31, 95, 64, 29, 77, 23, 62, 16, 89, 20, 80, 54, 52]
      (ys, count) = mergeSortFromListN (length src) src
  print (V.toList ys == sort src)
  print count
  -- sanity check against example in https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
  let v = [12 :: Int, 1, 10, 2, 7, 11, 4, 14, 5, 9, 15, 8, 13, 6, 3]
      (_, count') = mergeSortFromListN (length v) v
  print (count' == 49)
