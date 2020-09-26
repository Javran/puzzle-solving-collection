module Game.Tents.BatchReorganize where

import Control.Monad
import Data.List
import Data.List.Split
import qualified System.IO.Strict

{-
  (WIP)
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
      (rawPuzzles, postJunk) =
        let chunks = chunksOf 2 raw1
            toPair [a,b] = (unlines a, unlines b)
        in if even (length raw1)
          then (fmap toPair chunks, [])
          else (fmap toPair $ init chunks, last chunks)
  unless (null preJunk) $ do
    putStrLn "Warning: removed sections prior to first '#':"
    putStrLn (unlines preJunk)
  unless (null postJunk) $ do
    putStrLn "Warning: removed unexpected sections at the end of file:"
    putStrLn (unlines (concat postJunk))
  print postJunk
  pure ()
