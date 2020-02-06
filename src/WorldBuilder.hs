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
import Data.Set
import Data.List.Lens
import Control.Lens.Extras (is)
import SayCommon
import Control.Monad.Trans.Reader

emptyWorld :: World
emptyWorld = World
    {
        _directions = undefined,
        _rooms = Map.empty,
        _title = "Untitled Goose Game",
        _msgBuffer = MessageBuffer { _indentLvl = 0, _stdBuffer = [], _dbgBuffer = [], _msgStyle = Nothing },
        _objects = Map.empty,
        _player = "PLAYER",
        _nextObjID = "0",
        _firstRoom = "NA",
        _theDarknessWitnessed = False
    }
makePlayer :: Object
makePlayer = makeThing "yourself" "it's you." "PLAYER"

initWorld :: World -> World
initWorld w = w & (objects . at (w ^. player)) ?~ makePlayer

data ConstructInfo = ConstructInfo {
    _currentRoom :: ID,
    _currentObject :: ID
}

data WorldBuilder w r = WorldBuilder
    {
        _worldUnderConstruction :: w,
        _rulesUnderConstruction :: r,
        _constructInfo :: ConstructInfo
    }

type ConstructObject = ID -> Object

makeLenses ''ConstructInfo
makeLenses ''WorldBuilder

instance HasWorld w => HasWorld (WorldBuilder w r) where
    world = worldUnderConstruction . world

generateID :: HasWorld w => State w ID
generateID = do
    w <- get
    let newID = read (T.unpack $ w ^. world . nextObjID) :: Integer
    world . nextObjID .= T.pack (show (newID+1))
    return $ w ^. nextObjID

addObject :: HasWorld w => ConstructObject -> State (WorldBuilder w r) ID
addObject obj = do
    newID <- generateID
    let newObj = obj newID
    world . objects . at newID ?= newObj
    constructInfo . currentObject .= newID
    return newID

addRoom :: HasWorld w => (ID -> (Object, RoomData)) -> State (WorldBuilder w r) ID
addRoom obj = do
    newID <- generateID
    let newObj = obj newID
    world . rooms . at newID ?= snd newObj
    world . objects . at newID ?= fst newObj
    w <- get
    when ((w ^. world . firstRoom) == "NA") $ world . firstRoom .= newID
    constructInfo . currentRoom .=  newID
    return newID

setTitle :: HasWorld w => T.Text -> State (WorldBuilder w r) ()
setTitle t = world . title .= t

emptyWorldBuilder :: (HasWorld w, HasWorldRules r w) => (World -> w) -> (WorldRules World -> r) -> WorldBuilder w r
emptyWorldBuilder f r = WorldBuilder (f $ initWorld emptyWorld) (r emptyRules) (ConstructInfo { _currentRoom = "NA", _currentObject = "NA"})

makeWorld :: (HasWorld w, HasWorldRules r w) => (World -> w) -> (WorldRules World -> r) -> State (WorldBuilder w r) b -> (w, r)
makeWorld worldEmbed rulesEmbed s = (view worldUnderConstruction x, view rulesUnderConstruction x)
    where x = execState s (emptyWorldBuilder worldEmbed rulesEmbed)

run :: (HasWorld w, HasWorldRules r w) => r -> State w ()
run r = do
    runPlainRulebook (r ^. worldRules . rulebooks . whenPlayBeginsRules) r
    return ()
    --runPlainRulebook $ (u ^. universeRules . rulebooks . whenPlayBeginsRules) (u ^. universeRules . ruleSet) (u ^. universeWorld)

emptyRules :: WorldRules World
emptyRules = WorldRules
    {
        _actions = constructActions,
        _rulebooks = constructRulebooks,
        _activities = constructActivities
    }

constructRulebooks :: HasWorld w => RulebookCollection w
constructRulebooks = RulebookCollection
    {
        _whenPlayBeginsRules = whenPlayBeginsRulesImpl
    }

constructActions :: HasWorld w => ActionCollection w
constructActions = ActionCollection
    {
        _lookingAction = lookingActionImpl
    }

flushMsgBuffer :: HasWorld w => w -> IO w
flushMsgBuffer x = do
    putDoc $ fillCat $ reverse (x ^. world . msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. world . msgBuffer . stdBuffer)
    return x --{ _msgBuffer = (x ^. msgBuffer) { _stdBuffer = [], _dbgBuffer = []}}

headerLength :: Int
headerLength = 4

flushToString :: HasWorld w => w -> IO (T.Text, w)
flushToString x = do
    putDoc $ fillCat $ reverse (x ^. world . msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse (x ^. world . msgBuffer . stdBuffer)
    let outputStr = renderStrict $ layoutCompact $ fillCat $ Prelude.drop headerLength $ reverse (x ^. msgBuffer . stdBuffer)
    return (T.strip $ T.replace "\n\n" "\n" outputStr, x)
        --x . world . msgBuffer .~ ((x ^. world . msgBuffer) { _stdBuffer = [], _dbgBuffer = []}))

constructActivities :: HasWorld w => ActivityCollection w
constructActivities = ActivityCollection
    {
        _printingDarkRoomNameActivity = printingDarkRoomNameActivityImpl,
        _printingDarkRoomDescriptionActivity = printingDarkRoomDescriptionActivityImpl,
        _printingNameActivity = printingNameActivityImpl
    }

addRule :: (HasWorld w, HasWorldRules r w) => Rule w () -> State (WorldBuilder w r) ()
addRule r = rulesUnderConstruction . rulebooks . whenPlayBeginsRules . firstRules . _tail <>= [r]
{-
--updateRoomData :: ID -> (RoomData -> RoomData) -> World -> World 
--updateRoomData r = over (objects . ix r . info . _Room)
{-
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
-}

hackyParse :: T.Text -> State w (Action w a)
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

-}