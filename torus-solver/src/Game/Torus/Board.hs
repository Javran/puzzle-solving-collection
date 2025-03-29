module Game.Torus.Board
  ( Board (..)
  , Move (..)
  , Coord
  , normalizeMove
  , mkBoard
  , simplifyMoves
  , rotateLeft
  , applyMove
  , applyMoves
  , bdIndex
  , bdGet
  , isSolved
  , pprBoard
  , operateOnIndices
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
    -- note that stored numbers are ranged from
    -- [0 .. row*col-1], rather than [1 .. row*col].
    -- so pretty printing function is actually
    -- performing +1 to each tile when rendering.
    bdTiles :: V.Vector Int
  }
  deriving (Show, Eq)

type Coord = (Int, Int)

{-
  mIndex describes which row or col it's operation on
  mStep is, well, steps of the move.
 -}
data Move
  = MoveLeft {mIndex :: Int, mStep :: Int}
  | MoveRight {mIndex :: Int, mStep :: Int}
  | MoveUp {mIndex :: Int, mStep :: Int}
  | MoveDown {mIndex :: Int, mStep :: Int}
  deriving (Show)

{-
  Normalize a Move in preparation of performing action on the board.
  Note that we only need left and up because for a Board of X rows,

  MLeft r u == MRight r (X-u)

  something similar applies to columns too.

  we can go further to enforce that:
  - 0 <= mIndex < rows for MLeft
  - 0 <= mIndex < cols for MRight
  so that all moves except no-op are uniquely represented.

  results from normalizeMove always meet those criteria.
 -}
normalizeMove :: Board -> Move -> Move
normalizeMove Board {bdDims = (rows, cols)} m = case m of
  MoveLeft i s ->
    MoveLeft i (s `mod` cols)
  MoveRight i s ->
    MoveLeft i ((-s) `mod` cols)
  MoveUp i s ->
    MoveUp i (s `mod` rows)
  MoveDown i s ->
    MoveUp i ((-s) `mod` rows)

{-
  Try to pack a sequence of moves into smaller sequences.
  This is done by
  - normalization (so we only have MoveLeft and MoveUp to deal with)
  - combining consecutive moves that operate on the same row or col into one.
    (currently resulting moves are order-preserving, for now I don't want to
    go the extra mile to deal with merge-able moves that happens to multiple rows or columns.
  - remove moves that end up having 0 steps.
 -}
simplifyMoves :: Board -> [Move] -> [Move]
simplifyMoves bd = go [] . fmap (normalizeMove bd)
  where
    Board {bdDims = (rows, cols)} = bd
    go acc [] = reverse acc
    go acc (m : ms)
      | mStep m == 0 = go acc ms
      | otherwise =
          case acc of
            [] -> go (m : acc) ms
            x : xs -> case (x, m) of
              (MoveLeft i a, MoveLeft j b)
                | i == j ->
                    let s = (a + b) `rem` cols
                     in if s == 0
                          then go xs ms
                          else go (MoveLeft i s : xs) ms
              (MoveUp i a, MoveUp j b)
                | i == j ->
                    let s = (a + b) `rem` rows
                     in if s == 0
                          then go xs ms
                          else go (MoveUp i s : xs) ms
              _ -> go (m : acc) ms

{-
  basic operation of rotating a list towards left,
  e.g. rotateLeft 3 "ABCDE" == "DEABC"

  only works when n >= 0. and xs non-empty.

  (while it's quite easy to support empty list,
  i decided to go against it as it's just an extra branching
  that we'll never go)
 -}
rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = fmap fst $ zip (drop n (cycle xs)) xs

{-
  focus on a sequence of linear indices and apply action on them.
  This action is only allowed to re-order existing elements.
  note that element presence and list length after the action is not checked.
 -}
operateOnIndices :: Board -> [Int] -> (forall a. [a] -> [a]) -> Board
operateOnIndices bd@Board {bdTiles} linearIndices action =
  bd {bdTiles = bdTiles V.// zip linearIndices xs'}
  where
    -- from (r, 0) to (r, cols-1)
    -- linearIndices =
    -- [r * cols .. r * cols + cols -1]
    xs = fmap (bdTiles V.!) linearIndices
    xs' = action xs

applyMove :: Board -> Move -> Board
applyMove bd@Board {bdDims = (rows, cols)} mPre = case m of
  MoveLeft r s ->
    let
      -- from (r, 0) to (r, cols-1)
      linearIndices =
        [r * cols .. r * cols + cols - 1]
     in
      operateOnIndices bd linearIndices (rotateLeft s)
  MoveUp c s ->
    let
      -- from (0, c) to (rows-1, c)
      linearIndices =
        [c, c + cols .. c + cols * (rows - 1)]
     in
      operateOnIndices bd linearIndices (rotateLeft s)
  _ -> error "unreachable."
  where
    m = normalizeMove bd mPre

applyMoves :: Board -> [Move] -> Board
applyMoves = foldl' applyMove

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

bdIndex :: Board -> Coord -> Int
bdIndex Board {bdDims = (_, cols)} (r, c) = r * cols + c

bdGet :: Board -> Coord -> Int
bdGet bd@Board {bdTiles} (r, c) = bdTiles V.! bdIndex bd (r, c)

isSolved :: Board -> Bool
isSolved Board {bdTiles} =
  and (zipWith (==) [0 ..] (V.toList bdTiles))

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
  forM_ [0 .. rows - 1] $ \r -> do
    let lineTiles = fmap (bdGet bd . (r,)) [0 .. cols - 1]
    putStrLn $
      "║ "
        <> intercalate " ║ " (fmap (renderTile . succ) lineTiles)
        <> " ║ "
        <> show r
    if r < rows - 1
      then printSep "╠" "╬" "╣"
      else printSep "╚" "╩" "╝"
  putStrLn $ drop 1 $ concatMap (("   " <>) . renderTile) [0 .. cols - 1]
