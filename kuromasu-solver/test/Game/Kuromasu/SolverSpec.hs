module Game.Kuromasu.SolverSpec where

import Test.Hspec

import Control.Monad
import Data.Maybe
import Data.Monoid

import qualified Data.Map.Strict as M

import Game.Kuromasu.Parser
import Game.Kuromasu.Solver
import Paths_kuromasu_solver

{-
  TODO: it turns out 0hn0 does not exactly
  follow the game rule of kuromasu. namely:

  - red cells are not allowed to border each other
    horizontally or vertically.

  - the number on that dot include that dot itself.

  For now this issue isn't very important,
  but in future this is definitely worth pointing out explicitly.
 -}

exampleRaw0 :: [String]
exampleRaw0 =
  [ "9 9"
  , "? ? 3 2 ? ? r 1 1"
  , "r 4 ? r ? ? ? ? ?"
  , "? ? ? ? ? ? 7 ? ?"
  , "? ? ? ? 8 8 ? 9 8"
  , "5 ? ? 6 ? ? ? ? 4"
  , "5 r ? ? ? r 5 ? ?"
  , "? ? 6 ? 3 ? ? ? r"
  , "? r 5 ? r ? 4 r 1"
  , "? ? ? 3 ? 5 ? 3 ?"
  ]

exampleRaw1 :: [String]
exampleRaw1 =
  [ "9 9"
  , "1 ? 3 ? ? ? ? 3 ?"
  , "? ? ? ? 4 4 ? ? r"
  , "? 9 8 ? 8 ? ? ? ?"
  , "2 ? ? ? ? ? ? ? 2"
  , "? ? ? ? 2 ? ? 6 4"
  , "? r ? 8 ? ? 8 ? 7"
  , "2 ? ? ? ? r ? 4 ?"
  , "? ? 5 ? ? ? ? ? 3"
  , "? ? 7 ? ? ? r ? r"
  ]

exampleRaw2 :: [String]
exampleRaw2 =
  [ "9 9"
  , "? r 2 r ? r ? 6 2"
  , "r 4 ? 6 4 ? 2 ? ?"
  , "r ? ? 7 ? ? r 5 ?"
  , "? ? ? ? ? ? ? ? 7"
  , "r 4 r ? r ? ? 6 6"
  , "4 ? ? r 4 1 r ? ?"
  , "? ? ? 6 ? ? ? ? ?"
  , "? ? ? ? 7 ? 6 6 r"
  , "4 5 ? 7 ? ? ? ? ?"
  ]

countBlues :: M.Map Coord Cell -> Coord -> Int
countBlues m coord =
  getSum $
    foldMap
      countInDir
      [ \(r, c) -> (r - 1, c)
      , \(r, c) -> (r + 1, c)
      , \(r, c) -> (r, c - 1)
      , \(r, c) -> (r, c + 1)
      ]
  where
    countInDir next =
      Sum
        . length
        . takeWhile ((== cBlue) . fromMaybe cRed . (m M.!?))
        . tail -- exclude that cell itself.
        $ iterate next coord

{-
  TODO: rules are (according to 0hn0 rather than the original game
  as described in wikipedia page):

  - a number n indicates that cell is blue,
    and it can see n blues in 4 directions (excluding itself)
  - blue dots sees at least one.
    (in other words, blue dots cannot be surrounded by red dots).
 -}
spec :: Spec
spec =
  describe "solve" $ do
    let solveAndVerifyBoard bdRep@((rows, cols), parsedBoard) = do
          Just (bd, hints) <- pure $ mkBoardFromRep bdRep
          Just solved <- pure $ extractAnswer (solve bd)
          -- Verify that outout has the same rows and cols as input.
          length solved `shouldBe` rows
          all ((== cols) . length) solved `shouldBe` True
          let coords = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
              toMap :: forall v. [[v]] -> M.Map (Int, Int) v
              toMap = M.fromList . zip coords . concat
              inputBoard = toMap parsedBoard
              solvedBoard = toMap solved
          -- Verify that output and input are "compatible",
          -- meaning that the solution should preserve all colored dots from its input.
          forM_ coords $ \coord ->
            case (inputBoard M.! coord, solvedBoard M.! coord) of
              (Nothing, _) -> pure ()
              (Just (Left cInp), cOut) -> cOut `shouldBe` cInp
              (Just (Right _), cOut) -> cOut `shouldBe` cBlue
          -- Verify all blue dots.
          forM_ coords $ \coord ->
            when (solvedBoard M.! coord == cBlue) $ do
              let blueCount = countBlues solvedBoard coord
              case lookup coord hints of
                Nothing ->
                  -- if this blue dot does not belong to the original hint set,
                  -- we just verify that it has at least one neighbooring blue dots.
                  blueCount `shouldSatisfy` (>= 1)
                Just count ->
                  -- if this blue dot is attached with a number,
                  -- we can do a stronger verification to confirm that the count matches.
                  blueCount `shouldBe` count
        mkExample name rawInp =
          specify name $ do
            Just bdRep <- pure $ parseBoard (unlines rawInp)
            solveAndVerifyBoard bdRep
    mkExample "example0" exampleRaw0
    mkExample "example1" exampleRaw1
    mkExample "example2" exampleRaw2
    specify "data/puzzles.txt" $ do
      puzzlesFilePath <- getDataFileName "data/puzzles.txt"
      content <- readFile puzzlesFilePath
      let parsed = parseBoards content
      mapM_ solveAndVerifyBoard parsed
