module Game.Takuzu.SolverSpec where

import Control.Monad
import Data.List
import Data.Monoid
import Test.Hspec

import qualified Data.List.Match as LMatch

import Game.Takuzu.Parser
import Game.Takuzu.Solver
import Paths_takuzu_solver

exampleRaw0 :: [] ([] Char)
exampleRaw0 =
  [ "br 12"
  , "    rr  br  "
  , "      r  b b"
  , "  br    r  b"
  , " r r        "
  , "b     r b b "
  , "  b b     b "
  , " r  br  r   "
  , "r    r      "
  , "r   r   bb  "
  , "  r     b   "
  , "      r   rb"
  , "  r  r      "
  ]

exampleRaw1 :: [] ([] Char)
exampleRaw1 =
  [ "br 12"
  , "  b b b     "
  , "b  r   r   b"
  , "     b     r"
  , " b     r  b "
  , "  bb   rr b "
  , "        r   "
  , " b  r      r"
  , "     b    r "
  , "bb  rb  b   "
  , "   r     r  "
  , "r r    rb  b"
  , "r   b     r "
  ]

-- check whether input board and output board are compatible,
-- in other words, if one cell of the input board is known,
-- it should never be changed in the output board.
-- this function assumes that both input and output board are of the same size
-- in every dimension.
areCompatible :: [[Maybe Cell]] -> [[Cell]] -> Bool
areCompatible inpBd outBd = and $ zipWith rowCompatible inpBd outBd
  where
    rowCompatible :: [Maybe Cell] -> [Cell] -> Bool
    rowCompatible xs ys = and $ zipWith cmp xs ys
    cmp Nothing _ = True
    cmp (Just b0) b1 = b0 == b1

expectSolution :: Int -> [[Cell]] -> Expectation
expectSolution n board = do
  let lengthMatches :: forall a. [a] -> Bool
      lengthMatches = LMatch.equalLength (replicate n ())
      expectRow :: [Cell] -> Expectation
      expectRow row = do
        -- "same # of red / blue cell" rule.
        blueCount `shouldBe` redCount
        (blueCount + redCount) `shouldBe` n
        -- "no more than two consecutive of the same color" rule.
        group row
          `shouldSatisfy` all (`LMatch.lessOrEqualLength` [(), ()])
        where
          (Sum blueCount, Sum redCount) = foldMap go row
          go c
            | c == cBlue = (1, 0)
            | c == cRed = (0, 1)
            | otherwise = mempty

  -- verify that resulting board is of n x n.
  board `shouldSatisfy` lengthMatches
  board `shouldSatisfy` all lengthMatches
  let board' = transpose board
      noDup xs = xs `LMatch.equalLength` nub xs
  mapM_ expectRow board
  mapM_ expectRow (transpose board)
  -- "no two row/cols are the same" rule.
  board `shouldSatisfy` noDup
  board' `shouldSatisfy` noDup

spec :: Spec
spec =
  describe "solveBoard" $ do
    let solveAndVerifyBoard sz bd = do
          sz `shouldSatisfy` even
          sz `shouldSatisfy` (> 0)
          Just solved <- pure $ solveBoard sz bd
          expectSolution sz solved
          -- verify that we do build the solution respecting input board.
          (bd `areCompatible` solved) `shouldBe` True
        mkExample name rawInp =
          specify name $ do
            Just (sz, bd) <- pure $ parseBoard (unlines rawInp)
            solveAndVerifyBoard sz bd
    mkExample "example0" exampleRaw0
    mkExample "example1" exampleRaw1
    specify "data/puzzles.txt" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      let parsed = parseBoards content
      forM_ parsed $ \(sz, bd) ->
        solveAndVerifyBoard sz bd
