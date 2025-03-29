{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Minesweeper.Parser
  ( sampleRaw
  , parseBoard
  , newlineP
  , rowsAndColsP
  , tileP
  , boardP
  , fullBoardP
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
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
        [0 .. rows - 1]
        ( \row ->
            forM [0 .. cols - 1] (\col -> ((row, col),) <$> tileP) <* newlineP
        )
  let ( M.fromList . DL.toList -> brNums
        , M.fromList . DL.toList -> brMines
        , S.fromList . DL.toList -> brMissing
        ) =
          foldMap
            ( (,,)
                <$> ( \(c, r) -> case r of
                        Right (Just v, _) -> DL.singleton (c, v)
                        _ -> mempty
                    )
                <*> ( \(c, r) -> case r of
                        Right (_, Just v) -> DL.singleton (c, v)
                        _ -> mempty
                    )
                <*> ( \(c, r) -> case r of
                        Left () -> DL.singleton c
                        _ -> mempty
                    )
            )
            results
  pure $ BoardRep {brDims, brNums, brMines, brMissing}

fullBoardP :: ReadP BoardRep
fullBoardP = (rowsAndColsP <* newlineP) >>= boardP
