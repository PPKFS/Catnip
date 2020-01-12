module Utils where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Monoid

-- | A neater if-then-else format.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- | a simpler way to call modify for states on tuples
-- TODO: find how to rewrite this with zoom
modifyR :: (a -> a) -> State (a, b) ()
modifyR f = modify (\(w, a) -> (f w, a))

-- | black magic. do a list of things in a monadic instance, taking the first just
doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . getAlt . foldMap (Alt . MaybeT . f)

zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', b')) <- runStateT stabc (a, b)
    pure (c, a')