{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WorldBuilder where

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
import Lens.Family.Total

baseWorld :: HasLocation obj => World obj usr
baseWorld = World
    {
        _directions = constructDirections,
        _rooms = Map.empty,
        _title = "Untitled Goose Game",
        _msgBuffer = MessageBuffer { _indentLvl = 0, _stdBuffer = [], _dbgBuffer = [], _msgStyle = Nothing },
        _std = StandardLibrary {
            _activities = constructActivities,
            _actions = constructActions,
            _rulebooks = constructRulebooks},
        _objects = Map.fromList [("PLAYER", makeThing "yourself" "it's you." "PLAYER")],
        _player = "PLAYER",
        _nextObjID = "0",
        _firstRoom = "NA"
    }

constructRulebooks :: HasLocation obj => RulebookCollection obj usr
constructRulebooks = RulebookCollection
    {
        _whenPlayBeginsRules = whenPlayBeginsRulesImpl
    }

constructActivities :: HasLocation obj => ActivityCollection obj usr
constructActivities = ActivityCollection
    {
        _printingDarkRoomNameActivity = printingDarkRoomNameActivityImpl,
        _printingNameActivity = printingNameActivityImpl
    }

constructActions ::HasLocation obj => ActionCollection obj usr
constructActions = ActionCollection
    {
        _lookingAction = lookingActionImpl
    }

data ConstructInfo = ConstructInfo {
    _currentRoom :: ID,
    _currentObject :: ID
}

makeLenses ''ConstructInfo

fromLeft :: Either a b -> a
fromLeft (Left a) = a

type ConstructObject obj = ID -> Object obj

type WorldBuilder obj usr = (World obj usr, ConstructInfo)

generateID :: State (WorldBuilder obj usr) ID
generateID = do
    (w, _) <- get
    let newID = read (T.unpack $ w ^. nextObjID) :: Integer
    modifyWorld (\w -> w { _nextObjID = T.pack $ show (newID+1)})
    return $ w ^. nextObjID

addObject :: (ID -> Object obj) -> State (WorldBuilder obj usr) ID
addObject obj = do
    newID <- generateID
    let newObj = obj newID
    _1 . objects . at newID ?= newObj
    _2 . currentObject .= newID
    return newID

addRoom :: (ID -> RoomObj obj) -> State (WorldBuilder obj usr) ID
addRoom obj = do
    newID <- generateID
    let newObj = obj newID
    _1 . rooms . at newID ?= newObj
    (w, _) <- get
    when ((w ^. firstRoom) == "NA") $ _1 . firstRoom .= newID
    _2 . currentRoom .=  newID
    return newID

verifyWorld :: World obj usr -> Either (World obj usr) ()
verifyWorld = Left

makeWorld :: HasLocation obj => State (WorldBuilder obj usr) ID -> Either (World obj usr) ()
makeWorld s = verifyWorld $ fst $ execState s (baseWorld, ConstructInfo { _currentRoom = "NA", _currentObject = "NA"})
