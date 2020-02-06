module Utils where

import Control.Applicative
import Control.Lens
import Control.Lens.Zoom
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Monoid

-- | A neater if-then-else format.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- | a simpler way to call modify for states on tuples
modifyR :: (a -> a) -> State (a, b) ()
modifyR f = zoom _1 $ modify f

-- | black magic. do a list of things in a monadic instance, taking the first just
-- primarily used for looping rules until we get a definitive outcome6
doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . getAlt . foldMap (Alt . MaybeT . f)

-- | opposite of zoom, from a state over b1, run a state with b2 bolted on
zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', b')) <- runStateT stabc (a, b)
    pure (c, a')