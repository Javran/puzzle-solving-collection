{-# LANGUAGE NamedFieldPuns #-}

module Game.Fifteen.ThreeByThree where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.DList as DL
import Data.Function
import qualified Data.HashSet as HS
import Data.HashTable.ST.Basic as HT
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Word
import Debug.Trace
-- import Game.Fifteen.Common
import Game.Fifteen.Types (Coord)
import qualified Game.Fifteen.Types as GB -- GB for general Board
import qualified Game.Fifteen.Common as GB -- GB for general Board

{-
  a module specialized for solving 3x3 puzzles.
  Note that board size is not checked.
 -}

{-
  a 3x3 board is represented by a 36-bit integer,
  for lowest 4 bits to highest 4 bits, tiles are represented in row-major order:

  - 0-3: for coord (0,0)
  - 4-7: for coord (0,1)
  - 8-11: for coord (0,2)
  - 12-15: for coord (1,0)
  - etc.

  allowed tile digits are 0-7 and 15 (which stands for holes)

 -}
type Board3 = Word64
type Tile = Word8

index :: Coord -> Int
index (r,c) = r*3 + c

bdGet :: Board3 -> Coord -> Tile
bdGet bd coord = fromIntegral $ wide .&. 0xF
 where
   wide :: Word64
   wide = bd `shiftR` (4 * index coord)

fromBoard :: GB.Board -> Maybe Board3
fromBoard GB.Board {GB.bdSize, GB.bdTiles} = do
  guard $ bdSize == 3
  let tileToNum :: Maybe Int -> Word64
      tileToNum Nothing = 0xF
      tileToNum (Just v) = fromIntegral v
      encoded =
        foldr (.|.) 0 $
          zipWith
            (\offset n -> shiftL (tileToNum n) offset)
            [0, 4 ..]
            (V.toList bdTiles)
  pure encoded

toBoard :: Board3 -> GB.Board
toBoard bd = bd'
  where
    convert r c = if x == 0xF then Nothing else Just (fromIntegral x)
      where
        x = bdGet bd (r,c)
    tileSource = [ [ convert r c | c<- [0..2]] | r <- [0..2] ]
    bd' = GB.mkBoard tileSource
