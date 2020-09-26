module Game.Tents.BatchReorganize where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.UUID
import qualified Data.UUID.V4
import Game.Tents.Parser
import Game.Tents.Types
import qualified System.IO.Strict

{-
  this step is to make sure that:
  - #-started lines are merged into one single line
  - and if #-started line contains nothing but '#' and whitespaces,
    assign it an id.
  - board can actually be parsed. if not, Nothing is returned.
 -}
normalizePuzzle :: ([String], [String]) -> IO (Maybe (String, ((Int, Int), [String])))
normalizePuzzle (commentLines, contents) = do
  let mergedCommentLine@(_ : cm) = case commentLines of
        [xs] -> xs
        cs ->
          -- safe, as we know those lines are grouped together
          -- for the very reason that they start with '#'.
          '#' : concatMap tail cs
  outCommentLine <-
    if all (\c -> isSpace c || c == '#') cm
      then do
        puzzleId <- Data.UUID.V4.nextRandom
        let sId = Data.UUID.toString puzzleId
        putStrLn $ "Filling in " <> sId <> " for an empty-comment puzzle."
        pure $ "# " <> Data.UUID.toString puzzleId
      else pure mergedCommentLine
  case parseBoard (unlines contents) of
    Just BoardRep {brDims = dims} -> pure (Just (outCommentLine, (dims, contents)))
    Nothing -> do
      putStrLn "Warning: the following section is dropped as it doesn't contain a valid puzzle."
      putStrLn (unlines $ commentLines <> contents)
      pure Nothing

{-
  Reorganizes a batch file (a bundle of puzzles).

  - this is to make sure that all puzzles have unique ids.
  - duplicated puzzles are removed.
  - puzzles are sorted by size.
 -}
batchReorganize :: FilePath -> IO ()
batchReorganize fp = do
  -- as the intention is to overwrite it,
  -- we'd better make sure that the file is properly closed.
  raw <- System.IO.Strict.readFile fp
  let preJunk : raw1 =
        -- first round of split cuts out sections.
        split (whenElt ("#" `isPrefixOf`)) (lines raw)
      rawPuzzles :: [([String], [String])]
      (rawPuzzles, postJunk) =
        let chunks = chunksOf 2 raw1
            toPair [a, b] = (a, b)
            toPair _ = error "unreachable"
         in if even (length raw1)
              then (fmap toPair chunks, [])
              else (fmap toPair $ init chunks, last chunks)
  unless (null preJunk) $ do
    putStrLn "Warning: removed sections prior to first '#':"
    putStrLn (unlines preJunk)
  unless (null postJunk) $ do
    putStrLn "Warning: removed unexpected sections at the end of file:"
    putStrLn (unlines (concat postJunk))
  -- puzzles sorted by size
  sortedPuzzles <- sortOn (fst . snd) . catMaybes <$> mapM normalizePuzzle rawPuzzles
  let dedupedPuzzles =
        concatMap (nubBy ((==) `on` (snd . snd)))
          . groupBy ((==) `on` (fst . snd))
          $ sortedPuzzles
      removed = length sortedPuzzles - length dedupedPuzzles
  when (removed > 0) $
    putStrLn $ "Dropped " <> show removed <> " duplicated puzzles."
  writeFile fp (unlines $ concatMap (\(c, (_sz, ps)) -> c : ps) dedupedPuzzles)
  putStrLn $ "Written to: " <> fp
