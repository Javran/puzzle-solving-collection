{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Game.Minesweeper.SolverSpec where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Game.Minesweeper.Parser
import Game.Minesweeper.Solver
import Game.Minesweeper.Types
import Test.Hspec
import Text.ParserCombinators.ReadP hiding (many)
import qualified Text.RawString.QQ as Q

rawTests :: String
rawTests =
  [Q.r|= #0
7 7
???????
??1122?
??1__1?
?21_13?
?1__1??
?1122??
???????
----
?___**_
?_1122_
_*1__1_
*21_13*
_1__1**
_1122_?
__*_*_?
= #1
3 5
?????
12221
_____
----
??*??
12221
_____
= #2
4 5
*????
?42?2
?3111
?2___
----
*??_*
*42*2
*3111
*2___
= #3
3 5
?**1_
?4421
?????
----
?**1_
?4421
?*_*_
= #4
3 4
????
12??
_13?
----
??_?
12**
_13*
= #5
3 5
___2?
1223?
????_
----
___2*
1223*
_**__
= #6
5 6
??????
??????
??1???
?112??
?___??
----
??????
?___??
??1???
?112??
?___??
= #7
5 6
??????
???1??
?3211_
?*111_
?___*_
----
??___?
???1??
?3211_
?*111_
?___*_
|]

tests :: [(String, (BoardRep, BoardRep))]
tests = case readP_to_S (many fullTestCaseP <* eof) rawTests of
  [(xs, "")] ->
    let len = length xs
     in if len == expectedTestCount
          then xs
          else
            error $
              "Expecting "
                <> show expectedTestCount
                <> " tests but getting "
                <> show len
  _ -> error "Parse error."
  where
    expectedTestCount :: Int
    expectedTestCount =
      getSum
        . foldMap
          ( \xs ->
              if "=" `isPrefixOf` xs then 1 else 0
          )
        $ lines rawTests

{-
  the format for a full test case is:
  - first line begins with "= ", followed by a test label (anything but newline),
    then a newline
  - exact same format as a board
  - "----\n"
  - exact same format as a board, but without the row and col count.
 -}
fullTestCaseP :: ReadP (String, (BoardRep, BoardRep))
fullTestCaseP = do
  label <- string "= " *> munch1 (/= '\n') <* char '\n'
  bdBefore@BoardRep {brDims} <- fullBoardP
  _ <- string "----\n"
  bdAfter <- boardP brDims
  pure (label, (bdBefore, bdAfter))

{-
  a tile could be:
  - '?' <==> Nothing
  - '_' <==> Just (Left False)
  - '*' <==> Just (Left True)
  - <num> <==> Just (Right <num>)
 -}
type Tile = Maybe (Either Bool Int)

isSameOrImproved :: Tile -> Tile -> Bool
isSameOrImproved x y = x == y || isNothing x

getTile' :: Board -> Coord -> Tile
getTile' bd coord =
  (Right <$> bdNums bd M.!? coord)
    <|> (Left <$> bdMines bd M.!? coord)

spec :: Spec
spec =
  describe "mkBoard & solveBoard" $ forM_ tests $
    \(label, (bdBeforeTmp, bdAfterTmp)) ->
      specify ("Label " <> label) $ do
        let Just bdSolved = do
              (xs, bd) <- mkBoard bdBeforeTmp
              solveBoard bd xs
            Just bdAfter@Board {bdDims = (rows, cols)} =
              -- as we only care about the parsed board,
              -- we don't need to do anything with bdCandidates.
              snd <$> mkBoard bdAfterTmp
            allCoords =
              [(r, c) | r <- [0 .. rows -1], c <- [0 .. cols -1]]
        bdDims bdSolved `shouldBe` bdDims bdAfter

        when (bdMines bdSolved /= bdMines bdAfter) $ do
          putStrLn "Solver founds a different solution:"
          forM_ [0 .. rows -1] $ \r -> do
            let f c = case getTile' bdSolved (r, c) of
                  Nothing -> "?"
                  Just (Left False) -> "_"
                  Just (Left True) -> "*"
                  Just (Right v) -> show v
            putStrLn $ concatMap f [0 .. cols -1]

        do
          -- verify that solver doesn't add out-of-range assignments
          -- into the MineMap.
          let assignedCoords = M.keys (bdMines bdSolved)
          assignedCoords `shouldSatisfy` all (isCoordInRange bdSolved)
        forM_ allCoords $ \coord -> do
          let tSolved = getTile' bdSolved coord
              tAfter = getTile' bdAfter coord
          -- verify that solver does come up with a solution that is
          -- at least as good as the one given in this test case.
          (tAfter, tSolved) `shouldSatisfy` uncurry isSameOrImproved
