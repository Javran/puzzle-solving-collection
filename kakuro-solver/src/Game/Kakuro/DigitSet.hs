module Game.Kakuro.DigitSet
  ( null
  , size
  , member
  , empty
  , singleton
  , insert
  , delete
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , foldr
  , toList
  , fromList
  , DigitSet (..)
  ) where

import Data.Bits
import qualified Data.Foldable
import GHC.Exts (build)
import Prelude hiding (filter, foldr, map, null)

{-
  A bitset meant to keep a very small set (e.g. one decimal digit),
  which can be contained in a Word.

  Interface is a subset of IntSet to be a drop-in replacement.

  ref: https://hackage.haskell.org/package/bitset-1.4.8/docs/src/Data-BitSet-Generic.html

 -}

newtype DigitSet = DigitSet {getDigitSet :: Word}
  deriving newtype (Eq, Ord, Bits)
  deriving stock (Show)

null :: DigitSet -> Bool
null = (== empty)
{-# INLINE null #-}

size :: DigitSet -> Int
size = popCount
{-# INLINE size #-}

member :: Int -> DigitSet -> Bool
member x bits = testBit bits x
{-# INLINE member #-}

empty :: DigitSet
empty = DigitSet 0
{-# INLINE empty #-}

singleton :: Int -> DigitSet
singleton = bit
{-# INLINE singleton #-}

insert :: Int -> DigitSet -> DigitSet
insert x bits = setBit bits x
{-# INLINE insert #-}

delete :: Int -> DigitSet -> DigitSet
delete x bits = clearBit bits x
{-# INLINE delete #-}

union :: DigitSet -> DigitSet -> DigitSet
union = (.|.)
{-# INLINE union #-}

unions :: Foldable f => f DigitSet -> DigitSet
unions = Data.Foldable.foldl' union empty
{-# INLINE unions #-}

difference :: DigitSet -> DigitSet -> DigitSet
difference bits1 bits2 = bits1 .&. complement bits2
{-# INLINE difference #-}

(\\) :: DigitSet -> DigitSet -> DigitSet
(\\) = difference

intersection :: DigitSet -> DigitSet -> DigitSet
intersection = (.&.)
{-# INLINE intersection #-}

foldr :: (Int -> b -> b) -> b -> DigitSet -> b
foldr f acc0 bits = go (popCount bits) 0
  where
    go = \cases
      0 _b -> acc0
      !n b ->
        if testBit bits b
          then f b $ go (n - 1) (b + 1)
          else go n (b + 1)
{-# INLINE foldr #-}

toList :: DigitSet -> [Int]
toList bs = build (\k z -> foldr k z bs)
{-# INLINE [0] toList #-}

fromList :: [Int] -> DigitSet
fromList = Data.Foldable.foldl' setBit empty
{-# INLINE [0] fromList #-}

{-# RULES
"fromList/toList" forall bs. fromList (toList bs) = bs
  #-}
