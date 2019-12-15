{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StandardRules where

import Main
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Lens.Prism
 
data A1 = A1 Int deriving Show
data A2 = A2 Int deriving Show
data A3 = A3 Int deriving Show
data UnifiedA = A1a A1 | A2a A2 | A3a A3 deriving Show
data Foo = X UnifiedA | Foo2 Char


makeClassyPrisms ''Foo
makeClassyPrisms ''UnifiedA

instance AsUnifiedA Foo where
    _UnifiedA = _X