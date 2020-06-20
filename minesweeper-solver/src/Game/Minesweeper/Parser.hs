{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Game.Minesweeper.Parser
  ( sampleRaw,
    parseBoard,
    newlineP,
    rowsAndColsP,
    tileP,
    boardP,
    fullBoardP,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Game.Minesweeper.Types
import Text.ParserCombinators.ReadP
import qualified Text.RawString.QQ as Q

{-
  Input file format:
  <rows> <cols>
  followed by mine presentation.

  valid chars:
  - '?' for unknown
  - '1'..'8' for number tiles
  - ' ' or '_' for empty space
  - '*' for mines
  - 'E' for missing tiles

  all lines end with newline.

 -}

sampleRaw :: String
sampleRaw =
  [Q.r|7 7
???????
??1122?
??1__1?
?21_13?
?1__1??
?1122??
???????
|]

parseBoard :: String -> Maybe BoardRep
parseBoard raw = do
  [(v, "")] <- pure $ readP_to_S (fullBoardP <* eof) raw
  pure v

newlineP :: ReadP ()
newlineP = void $ char '\n'

rowsAndColsP :: ReadP (Int, Int)
rowsAndColsP = do
  rowsRaw <- munch1 isDigit
  _ <- char ' '
  colsRaw <- munch1 isDigit
  pure (read rowsRaw, read colsRaw)

goodTileP :: ReadP (Maybe Int, Maybe Bool)
goodTileP =
  ((Nothing, Nothing) <$ char '?')
    <++ ((Nothing, Just False) <$ (char ' ' <|> char '_'))
    <++ ( do
            c <- satisfy (\c -> c >= '1' && c <= '8')
            let n = ord c - ord '0'
            pure (Just n, Just False)
        )
    <++ ((Nothing, Just True) <$ char '*')

-- Either <error> <good tile>
tileP :: ReadP (Either () (Maybe Int, Maybe Bool))
tileP =
  (Right <$> goodTileP)
    <++ (Left () <$ char 'E')

boardP :: (Int, Int) -> ReadP BoardRep
boardP brDims@(rows, cols) = do
  (results :: [((Int, Int), Either () (Maybe Int, Maybe Bool))]) <-
    concat
      <$> forM
        [0 .. rows -1]
        ( \row ->
            forM [0 .. cols -1] (\col -> ((row, col),) <$> tileP) <* newlineP
        )
  let extractFromResults extract = mapMaybe extract results
      brNums =
        M.fromList $
          extractFromResults
            ( \(c, r) -> do
                Right (m, _) <- pure r
                (c,) <$> m
            )
      brMines =
        M.fromList $
          extractFromResults
            ( \(c, r) -> do
                Right (_, m) <- pure r
                (c,) <$> m
            )
      brMissing =
        S.fromList $
          extractFromResults
            ( \(c, r) -> do
                Left () <- pure r
                Just c
            )
  pure $ BoardRep {brDims, brNums, brMines, brMissing}

fullBoardP :: ReadP BoardRep
fullBoardP = (rowsAndColsP <* newlineP) >>= boardP
