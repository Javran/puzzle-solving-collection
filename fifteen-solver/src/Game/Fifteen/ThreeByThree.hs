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
import Game.Fifteen.Common
import Game.Fifteen.Types

{-
  a module specialized for solving 3x3 puzzles.
  Note that board size is not checked.
 -}

-- encode r and c by taking the 2 least significant bits of them
-- and concatenate into 4 bits
coordEncode :: Coord -> Word32
coordEncode (r, c) = fromIntegral (r * 4 + c)

bdEncode :: Board -> Word32
bdEncode Board {bdNums} =
  foldr
    (.|.)
    0
    $ zipWith
      (\s coord -> shiftL (coordEncode coord) s)
      [0, 4 .. 28]
      (V.toList bdNums)

bdStrSimple :: Board -> String
bdStrSimple bd@Board {bdSize} =
  unlines $ fmap (\r -> concatMap (\c -> go r c) [0 .. bdSize -1]) [0 .. bdSize -1]
  where
    go r c = case bdGet bd (r, c) of
      Nothing -> "_"
      Just v -> show (v + 1)

solveBoard :: Board -> Board -> [[Coord]]
solveBoard goal initBoard = runST $ do
  visited <- HT.new
  let initQ = [(initBoard, [])]
  fix
    (\loop dl -> case dl of
       [] -> pure []
       (bd, path) : todos -> do
         let encoded = bdEncode bd
         if encoded == encodedGoal
           then pure [path]
           else do
             r <- HT.lookup visited encoded
             case r of
               Nothing -> do
                 HT.insert visited encoded ()
                 let nextMoves :: [(Coord, Board)]
                     nextMoves = possibleMoves bd
                 expanded <- fmap catMaybes <$> forM nextMoves $ \(coord, nextBd) -> do
                   r' <- HT.lookup visited (bdEncode nextBd)
                   pure $ case r' of
                     Nothing -> Just (nextBd, coord : path)
                     Just () -> Nothing
                 loop (todos <> expanded)
               Just () ->
                 loop todos)
    initQ
  where
    encodedGoal = bdEncode goal

{-
case dl of
  DL.Nil -> []
  DL.Cons (bd, path) todos -> do
    let encoded = bdEncode bd
        encoded' = fromIntegral encoded :: Int
    if encoded == encodedGoal
      then pure path
      else do
        if not (encoded' `IS.member` visited)
          then do
            let visited' = IS.insert encoded' visited
                nextMoves = possibleMoves bd
                expanded =
                  fmap
                    (\(coord, nextBd) ->
                       (nextBd, coord : path))
                    nextMoves
            solveBoardAux (DL.fromList todos <> DL.fromList expanded) visited'
          else solveBoardAux (DL.fromList todos) visited
        -}
