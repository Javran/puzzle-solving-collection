{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Fifteen.Solvability where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Writer
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- Reference: https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html

mergeSortFromListN :: forall a s. Ord a => Int -> [a] -> WriterT (Sum Int) (ST s) (V.Vector a)
mergeSortFromListN n xs = do
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

testMergeSort :: IO ()
testMergeSort = do
  let src :: [Int]
      src = [27, 79, 25, 6, 33, 31, 95, 64, 29, 77, 23, 62, 16, 89, 20, 80, 54, 52, 60, 70, 12, 64, 1, 24, 30, 2, 37, 34, 61, 37, 94, 70, 86, 7, 26, 49, 1, 79, 27, 40, 65, 25, 39, 11, 34, 58, 95, 3, 80, 43, 13, 30, 41, 13, 42, 100, 65, 84, 69, 52, 29, 67, 11, 69, 96, 51, 32, 38, 48, 17, 71, 32, 88, 98, 82, 5, 86, 34, 76, 18, 87, 47, 60, 21, 90, 55, 74, 27, 41, 26, 39, 61, 12, 43, 26, 4, 54, 97, 80, 14]
      (ys, count) = runST $ runWriterT $ mergeSortFromListN (length src) src
  print (V.toList ys == sort src)
  print count
  -- sanity check against example in https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
  let v = [12 :: Int, 1, 10, 2, 7, 11, 4, 14, 5, 9, 15, 8, 13, 6, 3]
      (_, count') = runST $ runWriterT $ mergeSortFromListN (length v) v
  print (count' == 49)
