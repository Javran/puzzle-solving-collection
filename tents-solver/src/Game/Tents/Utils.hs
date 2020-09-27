module Game.Tents.Utils where

import Control.Applicative
import Control.Monad.Except

isSingleton :: [a] -> Bool
isSingleton xs = case xs of
  [_] -> True
  _ -> False

guardNotNull :: (Alternative f, Foldable t) => t a -> f (t a)
guardNotNull xs = xs <$ guard (not . null $ xs)

m2e :: MonadError e m => e -> Maybe a -> m a
m2e errMsg v = case v of
  Nothing -> throwError errMsg
  Just r -> pure r
