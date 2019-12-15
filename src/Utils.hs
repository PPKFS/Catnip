module Utils where

import Control.Monad.State

-- | A neater if-then-else format.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- | a simpler way to call modify for states on tuples
-- for instance, this could be generic-ly written if needed
modifyWorld :: (a -> a) -> State (a, b) ()
modifyWorld f = modify (\(w, a) -> (f w, a))