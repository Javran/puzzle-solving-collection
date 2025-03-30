module Game.Kakuro.Main
  ( main
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Either
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Game.Kakuro.Parse
  ( PuzzleRaw
  , fromYamlFileOrDie
  , puzzleToSolState
  )
import Game.Kakuro.PrettyPrint (pprPuzzle)
import Game.Kakuro.PuzzleCompact as Pc
import Game.Kakuro.Solver (isSolved, solve)
import Game.Kakuro.Types
import System.CPUTime
import System.Console.Terminfo (setupTermFromEnv)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO
import Text.Printf

type Bundle = IM.IntMap PuzzleRaw

type Bundl a = (M.Map (Int, T.Text) a, IM.IntMap (IM.IntMap a))

newtype Bundle2 a
  = Bundle2 (Bundl a)
  deriving stock (Functor, Foldable, Show)
  deriving newtype (FromJSON)

newtype GoodBadUgly a = GoodBadUgly (a, a, a)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum a, Sum a, Sum a)
  deriving newtype (NFData)

theGood, theBad, theUgly :: GoodBadUgly Int
[theGood, theBad, theUgly] = fmap GoodBadUgly [(1, 0, 0), (0, 1, 0), (0, 0, 1)]

solveAll :: (IsPuzzleRep pr, Functor f, Foldable f) => f pr -> IO ()
solveAll xs0 = do
  (parseFailed, xs1) <- evaluate do
    force . partitionEithers . fmap repToPuzzle $ toList xs0
  unless (null parseFailed) do
    printf "Puzzles failed during conversion: %d\n" (length parseFailed)
  t0 <- getCPUTime
  GoodBadUgly (solved, partial, failed) <- evaluate do
    let
      trySolve p = fromMaybe theUgly do
        let ss = puzzleToSolState p
        ssFin <- solve ss
        pure if isSolved ssFin then theGood else theBad
    force $ foldMap trySolve xs1
  t1 <- getCPUTime
  printf "solved: %d, partial: %d, failed: %d\n" solved partial failed
  printf "total CPU time elapsed: %d ms\n" (quot (t1 - t0) 1_000_000_000)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  term <- setupTermFromEnv

  {-
    TODO: messy subcommands, do better.
   -}
  getArgs >>= \case
    ["solve", fp] -> do
      (puzzle, ss) <- fromYamlFileOrDie fp
      let ss' = solve ss
      pprPuzzle term puzzle ss'
    ["solve-bundle", fp] -> do
      Just bundle <- decodeFileStrict' @Bundle fp
      solveAll bundle
    ["solve-bundle2", fp] -> do
      Just bundle <- decodeFileStrict' @(Bundle2 PuzzleCompact) fp
      solveAll bundle
    xs -> die $ "unknown args: " <> unwords xs
