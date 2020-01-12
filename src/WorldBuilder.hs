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
import Data.Set
import Data.List.Lens
import Control.Lens.Extras (is)
import SayCommon

baseWorld :: World
baseWorld = World
    {
        _directions = constructDirections,
        _rooms = empty,
        _title = "Untitled Goose Game",
        _msgBuffer = MessageBuffer { _indentLvl = 0, _stdBuffer = [], _dbgBuffer = [], _msgStyle = Nothing },
        _std = StandardLibrary {
            _activities = constructActivities,
            _actions = constructActions,
            _rulebooks = constructRulebooks},
        _objects = Map.fromList [("PLAYER", makeThing "yourself" "it's you." "PLAYER"), 
            (nowhereRoom ^. objID, nowhereRoom)],
        _player = "PLAYER",
        _nextObjID = "0",
        _firstRoom = "NA"
        --_usrLibrary = u
    }

constructRulebooks ::  RulebookCollection 
constructRulebooks = RulebookCollection
    {
        _whenPlayBeginsRules = whenPlayBeginsRulesImpl
    }

constructActivities ::  ActivityCollection 
constructActivities = ActivityCollection
    {
        _printingDarkRoomNameActivity = printingDarkRoomNameActivityImpl,
        _printingNameActivity = printingNameActivityImpl
    }

constructActions :: ActionCollection 
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

type ConstructObject obj = ID -> Object

type WorldBuilder = (World , ConstructInfo)

generateID :: State (WorldBuilder ) ID
generateID = do
    (w, _) <- get
    let newID = read (T.unpack $ w ^. nextObjID) :: Integer
    modifyR (\w -> w { _nextObjID = T.pack $ show (newID+1)})
    return $ w ^. nextObjID

addObject :: (ID -> Object) -> State (WorldBuilder ) ID
addObject obj = do
    newID <- generateID
    let newObj = obj newID
    _1 . objects . at newID ?= newObj
    _2 . currentObject .= newID
    return newID

addRoom :: (ID -> RoomObj) -> State (WorldBuilder ) ID
addRoom obj = do
    newID <- generateID
    let newObj = obj newID
    _1 . rooms %= insert newID
    _1 . objects . at newID ?= newObj
    (w, _) <- get
    when ((w ^. firstRoom) == "NA") $ _1 . firstRoom .= newID
    _2 . currentRoom .=  newID
    return newID

updateRoomData :: ID -> (RoomData -> RoomData) -> World -> World 
updateRoomData r = over (objects . ix r . info . _Room)

positionRoom :: ID -> ID -> ID -> State (WorldBuilder ) ()
positionRoom r1 dir r2 = zoom _1 (do
    w <- get
    let opp = w ^. directions . ix dir . info . _Direction
    --TODO: make sure neither are the nowhere room.
    modify $ updateRoomData r1 ((mapConnections . at opp) ?~ r2)
    modify $ updateRoomData r2 ((mapConnections . at dir) ?~ r1)
    w <- get
    sayModifyLn $ T.pack $ show $ w ^. objects . ix r1 . info . _Room . mapConnections
    return ()
    )

setRoomDescriptionSettings :: RoomDescriptionSetting -> State (WorldBuilder ) ()
setRoomDescriptionSettings r = zoom _1 $
    std . actions . lookingAction . setActionVariables .= Just (lookingSetActionVariablesRules r)

addRule :: T.Text -> Rule () -> State (WorldBuilder ) ()
addRule _ r = _1 . std . rulebooks . whenPlayBeginsRules . firstRules . _tail <>= [r]

setTitle :: T.Text -> State (WorldBuilder ) ()
setTitle t = _1 . title .= t

run :: World -> World 
run w = execState (runPlainRulebook $ w ^. std . rulebooks . whenPlayBeginsRules) w

hackyParse :: T.Text -> State (World ) (Action a)
hackyParse x = do
    w <- get
    undefined
    --if 5 == 5 then return (w ^. std . actions . lookingAction)
    --    else return $ w ^. std . activities . printingDarkRoomNameActivity

runCommand :: T.Text -> State (World ) ()
runCommand command = do
    r <- hackyParse command
    tryAction r
    return ()

type TestMeWith = [T.Text]
runActions :: Maybe TestMeWith -> State (World ) ()
runActions Nothing = return ()
runActions (Just ac) = mapM_ runCommand ac

flushMsgBuffer :: World -> IO (World )
flushMsgBuffer x = do
    putDoc $ fillCat $ reverse (x ^. msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. msgBuffer . stdBuffer)
    return x { _msgBuffer = (x ^. msgBuffer) { _stdBuffer = [], _dbgBuffer = []}}

headerLength :: Int
headerLength = 4

flushToString :: World -> IO (T.Text, World )
flushToString x = do
    putDoc $ fillCat $ reverse (x ^. msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. msgBuffer . stdBuffer)
    let outputStr = renderStrict $ layoutCompact $ fillCat $ Prelude.drop headerLength $ reverse (x ^. msgBuffer . stdBuffer)
    return (T.strip $ T.replace "\n\n" "\n" outputStr, x { _msgBuffer = (x ^. msgBuffer) { _stdBuffer = [], _dbgBuffer = []}})

makeWorld :: State (WorldBuilder ) ID -> World 
makeWorld s = fst $ execState s (baseWorld, ConstructInfo { _currentRoom = "NA", _currentObject = "NA"})
