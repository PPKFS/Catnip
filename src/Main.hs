{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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

constructWorld :: World UserObjects UserLibrary
constructWorld = World
    {
        _directions = constructDirections,
        _rooms = Map.empty,
        _title = "Untitled Goose Game",
        _msgBuffer = MessageBuffer { _indentLvl = 0, _stdBuffer = [], _dbgBuffer = [], _msgStyle = Nothing },
        _std = StandardLibrary {
            _activities = constructActivities,
            _actions = constructActions,
            _rulebooks = constructRulebooks},
        _objects = Map.empty
    }

constructRulebooks :: RulebookCollection UserObjects UserLibrary
constructRulebooks = RulebookCollection
    {
        _whenPlayBeginsRules = whenPlayBeginsRulesImpl
    }

constructActivities :: ActivityCollection UserObjects UserLibrary
constructActivities = ActivityCollection
    {
        _printingDarkRoomNameActivity = printingDarkRoomNameActivityImpl,
        _printingNameActivity = printingNameActivityImpl
    }

constructActions :: ActionCollection UserObjects UserLibrary
constructActions = ActionCollection
    {
        _lookingAction = lookingActionImpl
    }

type UserLibrary = Int
type UserObjects = ()
type UserActions = Int

instance HasLocation UserObjects where
    getLocation _ _ = nowhereRoom
main :: IO ()
main = do
    let (_, x) = runRulebook whenPlayBeginsRulesImpl constructWorld
    putDoc $ fillCat $ reverse (x ^. msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. msgBuffer . stdBuffer)
    putStrLn ""