{-# LANGUAGE PatternSynonyms #-}

module Game.Kakuro.PuzzleCompact
  ( Cell (.., Wall, ClueH, ClueV, ClueHV)
  , PuzzleCompact (..)
  , encodeToString
  , decodeFromString
  , puzzleCompactP
  , fromPuzzle
  , toPuzzle
  ) where

import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Data.Aeson
import qualified Data.DList as DL
import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Game.Kakuro.Types
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (readP_to_Prec)
import Text.Read (readPrec)

{-
  An alternative representation of a puzzle for:

  - A somewhat compact storage.
  - cells can contain clues so there is no reconstruction needed.

 -}

data Cell a
  = Blank
  | ShadeHV (Maybe a, Maybe a)
  deriving (Eq, Show, Functor)

pattern Wall :: Cell a
pattern Wall = ShadeHV (Nothing, Nothing)

pattern ClueH :: a -> Cell a
pattern ClueH a = ShadeHV (Just a, Nothing)

pattern ClueV :: a -> Cell a
pattern ClueV a = ShadeHV (Nothing, Just a)

pattern ClueHV :: a -> a -> Cell a
pattern ClueHV a b = ShadeHV (Just a, Just b)

{-# COMPLETE Blank, Wall, ClueH, ClueV, ClueHV #-}

{-
  TODO:

  - test coverage

 -}

data PuzzleCompact = PuzzleCompact
  { pcRows :: Int
  , pcCols :: Int
  , pcCells :: [[Cell Int]]
  }
  deriving (Eq)

instance Show PuzzleCompact where
  show = encodeToString

instance Read PuzzleCompact where
  readPrec = readP_to_Prec (\_ -> puzzleCompactP)

instance FromJSON PuzzleCompact where
  parseJSON = withText "PuzzleCompact" \t -> case decodeFromString (T.unpack t) of
    Nothing -> fail "no parse"
    Just v -> pure v

instance ToJSON PuzzleCompact where
  toJSON = String . T.pack . encodeToString

{-
  A compact representation:

  - always begin with `R{rows}C{cols}`
  - `|`: on end of a row (thus final char in this format)
  - `W`: a wall
  - `B{num}`: a consecutive chunk of blanks
  - hint cell, taking few forms:

    + `hV?`, where ? is a positive number, vertical hint only
    + `H?v`, where ? is a positive number, hornizontal hint only
    + `H?V?`: two `?` are positive numbers, both hints
 -}

encodeToString :: PuzzleCompact -> String
encodeToString PuzzleCompact {pcRows, pcCols, pcCells} = DL.toList $ execWriter do
  let
    tellCh ch = tell (DL.singleton ch)
    tellChNum ch n = tell (DL.singleton ch <> DL.fromList (show @Int n))
  tellChNum 'R' pcRows
  tellChNum 'C' pcCols
  forM_ pcCells do
    fix
      ( \loop -> \case
          Blank : xs -> do
            let (ls, rs) = span (== Blank) xs
            tellChNum 'B' (1 + length ls)
            loop rs
          ClueH h : xs ->
            tellChNum 'H' h >> tellCh 'v' >> loop xs
          ClueV v : xs ->
            tellCh 'h' >> tellChNum 'V' v >> loop xs
          ClueHV h v : xs ->
            tellChNum 'H' h >> tellChNum 'V' v >> loop xs
          Wall : xs ->
            tellCh 'W' >> loop xs
          [] -> tellCh '|'
      )

puzzleCompactP :: ReadP PuzzleCompact
puzzleCompactP = do
  let
    chNumP :: Char -> ReadP Int
    chNumP ch = char ch *> readS_to_P (reads @Int)
  pcRows <- chNumP 'R'
  pcCols <- chNumP 'C'
  pcCells <- replicateM pcRows do
    let
      hvP = do
        h <- (Nothing <$ char 'h') <++ (Just <$> chNumP 'H')
        v <- (Nothing <$ char 'v') <++ (Just <$> chNumP 'V')
        pure $ DL.singleton (ShadeHV (h, v))
      atomP =
        (DL.singleton Wall <$ char 'W')
          <++ ( do
                  cnt <- chNumP 'B'
                  pure $ DL.replicate cnt Blank
              )
          <++ hvP
    DL.toList . mconcat <$> manyTill atomP (char '|')

  pure PuzzleCompact {pcRows, pcCols, pcCells}

decodeFromString :: String -> Maybe PuzzleCompact
decodeFromString xs = case readP_to_S (puzzleCompactP <* eof) xs of
  [(v, "")] -> pure v
  _ -> Nothing

fromPuzzle :: Puzzle -> PuzzleCompact
fromPuzzle
  Puzzle
    { pzCells
    , pzBounds = MinMax2D ((rMin, rMax), (cMin, cMax))
    , pzCluesHori
    , pzCluesVert
    } =
    PuzzleCompact
      { pcRows = rMax - rMin + 2 -- one dead row on top for clues
      , pcCols = cMax - cMin + 2 -- one dead col on left for clues
      , pcCells
      }
    where
      getCell r c =
        if S.member (r, c) pzCells
          then Blank
          else ShadeHV (getTot <$> mClueH, getTot <$> mClueV)
        where
          getTot (TotCnt t _) = t
          mClueH = pzCluesHori M.!? (r, c + 1)
          mClueV = pzCluesVert M.!? (r + 1, c)
      pcCells =
        fmap
          (\r -> fmap (getCell r) [cMin - 1 .. cMax])
          [rMin - 1 .. rMax]

toPuzzle :: PuzzleCompact -> Maybe Puzzle
toPuzzle PuzzleCompact {pcRows, pcCols, pcCells} = do
  guard $ length pcCells == pcRows
  guard $ all ((== pcCols) . length) pcCells
  let
    pzCells = S.fromDistinctAscList do
      {-
        We begin both rows and cols with -1 because first row and col are always
        non-blank and may contain clues.

        On the other hand, Puzzle clues' start coord overlaps with first blank cells.
       -}
      (r, ln) <- zip [-1 ..] pcCells
      (c, x) <- zip [-1 ..] ln
      case x of
        Blank -> pure (r, c)
        _ -> []
    pzBounds =
      {-
        Originally row bound inclusive 0.. pcRows-1, by shifting -1,
        the actual bound is -1 .. pcRows-2.
        Ditto for col.
       -}

      MinMax2D ((0, pcRows - 2), (0, pcCols - 2))
    (hs, vs) = unzip do
      (r, ln) <- zip [-1 ..] pcCells
      (c, x) <- zip [-1 ..] ln
      let countConsecBlanks =
            length
              . takeWhile (\cur -> S.member cur pzCells)
      case x of
        Blank -> pure (M.empty, M.empty)
        ClueH h -> do
          let
            cnt =
              countConsecBlanks $ iterate (applyDir R) (r, c + 1)
          pure (M.singleton (r, c + 1) (TotCnt h cnt), M.empty)
        ClueV v -> do
          let
            cnt =
              countConsecBlanks $ iterate (applyDir D) (r + 1, c)
          pure (M.empty, M.singleton (r + 1, c) (TotCnt v cnt))
        ClueHV h v -> do
          let
            cntH =
              countConsecBlanks $ iterate (applyDir R) (r, c + 1)
            cntV =
              countConsecBlanks $ iterate (applyDir D) (r + 1, c)
          pure
            ( M.singleton (r, c + 1) (TotCnt h cntH)
            , M.singleton (r + 1, c) (TotCnt v cntV)
            )
        Wall -> pure (M.empty, M.empty)
  pure
    Puzzle
      { pzBounds
      , pzCells
      , pzCluesHori = M.unions hs
      , pzCluesVert = M.unions vs
      }

instance IsPuzzleRep PuzzleCompact where
  repToPuzzle = maybe (Left "Nothing") Right . toPuzzle
