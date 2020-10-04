{-# LANGUAGE ScopedTypeVariables #-}

module Game.Torus.AmanoSpec where

import Control.Monad
import qualified Data.Vector as V
import Game.Torus.Amano
import Game.Torus.Board
import Game.Torus.BoardSpec
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- the board could be unsolvable when # of cols is odd,
-- if this happens, the last two tiles will be flipped
-- note that it is intentional that if cols is even,
-- we don't attempt to fix it.
unsolvableWorkaround :: Board -> Board
unsolvableWorkaround bd =
  if odd cols
    then operateOnIndices bd [lastInd -1, lastInd] reverse
    else bd
  where
    lastInd = rows * cols -1
    (rows, cols) = bdDims bd

data Triangle = TA | TB | TC | TD deriving (Show)

fixCoord :: Board -> Coord -> Coord
fixCoord Board {bdDims = (rows, cols)} (r, c) = (r `mod` rows, c `mod` cols)

intendedRotationEffect :: Board -> Bool -> Int -> Int -> Int -> Int -> Triangle -> Board
intendedRotationEffect bd clockwise r c p q t
  | p `rem` rows == 0 || q `rem` cols == 0 =
    {-
      We need to make a special case here as when either effective p and q are 0, the rotation
      is effectively no-op. and it is unexpected for operateOnIndices to handle cases
      with duplicated indices.
     -}
    bd
  | otherwise = operateOnIndices bd (fmap (bdIndex bd) coords) op
  where
    Board {bdDims = (rows, cols)} = bd
    [center, up, down, left, right] =
      fmap
        (fixCoord bd)
        [(r, c), (r - p, c), (r + p, c), (r, c - q), (r, c + q)]
    {-
      3 coordinates of the triangle in clockwise order,
      center is always the 2nd one.
     -}
    coords = case t of
      TA -> [up, center, right]
      TB -> [right, center, down]
      TC -> [down, center, left]
      TD -> [left, center, up]
    op = rotateLeft (if clockwise then 1 else 2)

newtype LargerBoard
  = LargerBoard Board
  deriving (Show)

instance Arbitrary LargerBoard where
  arbitrary = LargerBoard <$> mkArbitraryBoard (11, 17)

spec :: Spec
spec = do
  describe "triangle rotations" $ do
    describe "small examples" $ do
      {-
        examples on a fixed-size board to verify that
        those rotations do achieve intended effects.
        also to confirm that intendedRotationEffect is implemented correctly.
       -}
      let initBd = Board (3, 5) $ V.fromListN 15 [0 ..]
          bdExample xs = Board (3, 5) $ V.fromListN 15 (concat xs)
      forM_
        [ ( TA
          , cwA
          , ccwA
          , bdExample
              [ [0, 1, 7, 3, 4]
              , [5, 6, 9, 8, 2]
              , [10, 11, 12, 13, 14]
              ]
          )
        , ( TB
          , cwB
          , ccwB
          , bdExample
              [ [0, 1, 2, 3, 4]
              , [5, 6, 12, 8, 7]
              , [10, 11, 9, 13, 14]
              ]
          )
        , ( TC
          , cwC
          , ccwC
          , bdExample
              [ [0, 1, 2, 3, 4]
              , [12, 6, 5, 8, 9]
              , [10, 11, 7, 13, 14]
              ]
          )
        , ( TD
          , cwD
          , ccwD
          , bdExample
              [ [0, 1, 5, 3, 4]
              , [7, 6, 2, 8, 9]
              , [10, 11, 12, 13, 14]
              ]
          )
        ]
        $ \(t, cwOp, ccwOp, cwExpected') -> do
          describe ("triangle " <> drop 1 (show t)) $ do
            let cwExpected = intendedRotationEffect initBd True 1 2 1 2 t
            specify "intendedRotationEffect" $ do
              cwExpected' `shouldBe` cwExpected
              intendedRotationEffect cwExpected False 1 2 1 2 t `shouldBe` initBd
            specify "clockwise" $
              applyMoves initBd (cwOp 1 2 1 2) `shouldBe` cwExpected
            specify "counterclockwise" $
              applyMoves cwExpected (ccwOp 1 2 1 2) `shouldBe` initBd
    describe "properties" $
      forM_
        [ (TA, cwA, ccwA)
        , (TB, cwB, ccwB)
        , (TC, cwC, ccwC)
        , (TD, cwD, ccwD)
        ]
        $ \(t, cwOp, ccwOp) -> do
          let tagPre = "triangle " <> drop 1 (show t)
          forM_
            [ (cwOp, True, tagPre <> ", clockwise")
            , (ccwOp, False, tagPre <> ", counterclockwise")
            ]
            $ \(rOp, isCw, tag) ->
              prop tag $
                \(LargerBoard bd) (rPre :: Int) (cPre :: Int) (p :: Int) (q :: Int) ->
                  let (r, c) = fixCoord bd (rPre, cPre)
                   in applyMoves bd (rOp r c p q) === intendedRotationEffect bd isCw r c p q t
  describe "solveBoard" $
    prop "SmallBoard" $
      \(SmallBoard bd) ->
        let (moves, bdFin) = solveBoard bd
         in label
              "moves are consistent"
              (applyMoves bd moves === bdFin)
              .&&. (label "solvable" (isSolved bdFin)
                      .||. label
                        "not solvable but can be fixed"
                        (isSolved (unsolvableWorkaround bdFin)))
