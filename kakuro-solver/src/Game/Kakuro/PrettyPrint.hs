module Game.Kakuro.PrettyPrint
  ( pprPuzzle
  )
where

import Control.Monad
import Data.Char
import qualified Game.Kakuro.DigitSet as DS
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Game.Kakuro.Types
import System.Console.Terminfo
import Text.Printf

pprPuzzle :: Terminal -> Puzzle -> Maybe SolState -> IO ()
pprPuzzle term Puzzle {pzCells, pzBounds, pzCluesHori, pzCluesVert} mSS = do
  let
    withFg :: forall s. TermStr s => Color -> s -> s
    withFg = fromMaybe (\_ -> id) (getCapability term withForegroundColor)
  let
    MinMax2D ((rMin, rMax), (cMin, cMax)) = pzBounds
    hSep = "+" <> intercalate "+" (replicate (cMax - cMin + 2) "---") <> "+"
  forM_ [rMin - 1 .. rMax] \r -> do
    putStrLn hSep
    rs <- forM [cMin - 1 .. cMax] \c -> do
      let cur = (r, c)
      pure
        if S.member cur pzCells
          then case mSS of
            Nothing ->
              ["   ", " ? ", "   "]
            Just SolState {ssSolved, ssCandidates} ->
              if
                | Just v <- ssSolved M.!? cur ->
                    ["   ", " " <> withFg Green (show v) <> " ", "   "]
                | Just cs <- ssCandidates M.!? cur -> do
                    let render v = if DS.member v cs then chr (ord '0' + v) else ' '
                    chunksOf 3 (fmap render [1 .. 9])
                | otherwise ->
                    ["   ", " ? ", "   "]
          else
            let
              getTot (TotCnt t _) = t
              mHori =
                ("\\" <>) . withFg Cyan . printf "%2d" . getTot
                  <$> pzCluesHori M.!? applyDir R cur
              mVert =
                (<> "\\") . withFg Magenta . printf "%-2d" . getTot
                  <$> pzCluesVert M.!? applyDir D cur
             in
              case (mHori, mVert) of
                (Nothing, Nothing) ->
                  ["###", "###", "###"]
                (Just x, Nothing) ->
                  [x, "#\\ ", "##\\"]
                (Nothing, Just y) ->
                  ["\\##", " \\#", y]
                (Just x, Just y) ->
                  [x, " \\ ", y]
    mapM_ putStrLn $ fmap (\t -> "|" <> intercalate "|" t <> "|") $ transpose rs
  putStrLn hSep
