{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Fifteen.Solvability where

import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- Reference: https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html

mergeSort :: forall a s. Ord a => V.Vector a -> ST s (V.Vector a)
mergeSort xs = do
  v <- V.thaw (xs :: V.Vector a)
  tmp <- VM.unsafeNew (VM.length v)
  let sort :: Int -> Int -> ST s ()
      sort l r =
        unless (l >= r) $ do
          let mid = (l + r) `quot` 2
          sort l mid
          sort (mid + 1) r
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
                           VM.write tmp tmpInd vL
                           loop (lInd + 1) rInd (tmpInd + 1)
                         else do
                           VM.write tmp tmpInd vR
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
  sort 0 (VM.length v -1)
  V.unsafeFreeze v

testMergeSort :: IO ()
testMergeSort = do
  let xs = V.fromList @Int [27, 79, 25, 6, 33, 31, 95, 64, 29, 77, 23, 62, 16, 89, 20, 80, 54, 52, 60, 70, 12, 64, 1, 24, 30, 2, 37, 34, 61, 37, 94, 70, 86, 7, 26, 49, 1, 79, 27, 40, 65, 25, 39, 11, 34, 58, 95, 3, 80, 43, 13, 30, 41, 13, 42, 100, 65, 84, 69, 52, 29, 67, 11, 69, 96, 51, 32, 38, 48, 17, 71, 32, 88, 98, 82, 5, 86, 34, 76, 18, 87, 47, 60, 21, 90, 55, 74, 27, 41, 26, 39, 61, 12, 43, 26, 4, 54, 97, 80, 14]
      ys = runST $ mergeSort xs
  print ys
