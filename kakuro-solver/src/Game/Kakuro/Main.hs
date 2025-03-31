module Game.Kakuro.Main
  ( main
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.DList as DL
import Data.Either
import Data.Foldable (toList)
import Data.Functor.Identity
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Game.Kakuro.Parse
  ( PuzzleRaw
  , fromYaml
  , puzzleToSolState
  )
import Game.Kakuro.PuzzleCompact as Pc
import Game.Kakuro.Solver (isSolved, solve)
import Game.Kakuro.Types
import System.CPUTime

-- import System.Console.Terminfo (setupTermFromEnv)
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

convertPuzzles :: (Foldable f, IsPuzzleRep pr) => f pr -> ([String], [Puzzle])
convertPuzzles = partitionEithers . fmap repToPuzzle . toList

data EPz = forall f pr. (Foldable f, IsPuzzleRep pr) => EPz (FilePath -> IO (Either String (f pr)))

puzzleLoaders :: [(String, EPz)]
puzzleLoaders =
  [ ("yaml", EPz @Identity @PuzzleRaw $ (fmap . fmap . fmap) Identity fromYaml)
  , ("bd0", EPz @IM.IntMap @PuzzleRaw $ eitherDecodeFileStrict' @Bundle)
  , ("bd1", EPz @Bundle2 @PuzzleCompact $ eitherDecodeFileStrict')
  ]

loadBundles :: String -> [FilePath] -> IO [Puzzle]
loadBundles ty fs = do
  EPz loader <- case lookup ty puzzleLoaders of
    Just f -> pure f
    Nothing -> die $ "Cannot find puzzle loader for type " <> ty

  (ls0, rs0) <-
    mconcat <$> forM fs \fp -> do
      r <- loader fp
      case r of
        Left err -> pure (DL.singleton (fp, err), mempty)
        Right r1 -> pure (mempty, DL.fromList $ toList r1)

  let failed = DL.toList ls0
  unless (null failed) do
    putStrLn $ "Failed to load from following sources: " <> show failed

  let (ls1, rs1) = convertPuzzles rs0
  unless (null ls1) do
    printf "Puzzles failed during conversion: %d\n" (length ls1)
  pure rs1

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- TODO: restore the command for solving just one puzzle and pretty print.
  -- _term <- setupTermFromEnv

  getArgs >>= \case
    "solve-bundle" : ty : fps -> do
      pzs <- loadBundles ty fps
      solveAll pzs
    xs -> die $ "unknown args: " <> unwords xs
