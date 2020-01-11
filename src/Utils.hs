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
-- for instance, this could be generic-ly written if needed
modifyWorld :: (a -> a) -> State (a, b) ()
modifyWorld f = modify (\(w, a) -> (f w, a))

modifyWorld2 :: (a -> a) -> State a ()
modifyWorld2 = modify

doUntilJustOld :: [a] -> (a -> State b (Maybe c)) -> State b (Maybe c)
doUntilJustOld [] _ = return Nothing
doUntilJust (x:xs) f = do
    y <- f x
    case y of
        Just _ -> return y
        Nothing -> doUntilJust xs f

-- | black magic. do a list of things in a monadic instance, taking the first just
doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . getAlt . foldMap (Alt . MaybeT . f)

zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', b')) <- runStateT stabc (a, b)
    pure (c, a')