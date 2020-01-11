{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Objects
import Rulebooks
import Actions
import Types
import System.IO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Utils
import WorldBuilder


type UserLibrary = Int
type UserObjects = ()
type UserActions = Int

instance HasLocation UserObjects where
    getLocation _ _ = nowhereRoom

--constructWorld :: [Construct UserObjects UserLibrary] -> Either (World UserObjects UserLibrary) [T.Text]
--constructWorld c = verifyWorld $ foldl applyConstruct baseWorld c

worldBuilder :: State (WorldBuilder UserObjects UserLibrary) ID
worldBuilder = do
    room <- addRoom $ makeRoom "test room" "this is a test"
    op <- addObject $ makeThing "pen" "its a pen"
    return ""

main :: IO ()
main = do
    let x = execState (runPlainRulebook whenPlayBeginsRulesImpl) (fromLeft $ makeWorld worldBuilder)
    putDoc $ fillCat $ reverse (x ^. msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. msgBuffer . stdBuffer)
    putStrLn ""