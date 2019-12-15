{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import System.IO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Lens.Family.Total

-- | Everything that can be interacted with is an object.
-- it may be abstract (idk, an idea???) or concrete (a thing).
-- however, everything that is expected to be interacted with in a game setting
-- should probably be an object or something close to.
-- everything specific to the object is stored in info; e.g.
-- directions only need 1 extra property (their opposite) but a person
-- would have some kind of person construction, encompassing physicalproperties
-- as well as living properties and probably sentient properties, etc.
data Object obj = Object 
    {
        _objID :: ID,
        _name :: Name,
        _nameProperness :: NameProperness,
        _namePlurality :: NamePlurality,
        _indefiniteArticle :: T.Text,
        _info :: ObjectInfo obj
    }

-- | Surprisingly, directions are indeed objects. The only field they need is an opposite.
type Opposite = ID

data ObjectInfo obj = Direction Opposite | Room RoomData | Region | Thing ThingData | Door DoorData | ExtInfo obj

-- | And our basic field types
type ID = T.Text
type Name = T.Text
data NameProperness = ImproperNamed | ProperNamed deriving Show
data NamePlurality = SingularNamed | PluralNamed deriving Show

-- | RoomData is fairly straightforward.
data RoomData = RoomData 
    {
        _roomDescription :: Description,
        _darkness :: Darkness,
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

data Darkness = Lighted | Dark deriving Show
data IsVisited = Visited | Unvisited deriving Show
type Description = T.Text
type MapConnections = Map.Map String ID
type ContainingRegion = Maybe ID

-- THINGS --
data ThingLit = Lit | Unlit deriving Show
data Edibility = Edible | Inedible deriving Show
data Portability = FixedInPlace | Portable deriving Show
data Wearability = Wearable | Unwearable deriving Show
data Handled = Handled | Unhandled deriving Show
data Described = Described | Undescribed deriving Show
data Mentioned = Mentioned | Unmentioned deriving Show
data MarkedForListing = MarkedForListing | UnmarkedForListing deriving Show
data Pushable = PushableBetweenRooms | NotPushableBetweenRooms deriving Show

type InitialAppearance = Description
-- | a location doesn't necessarily have to be a room (e.g. a vehicle or container)
type LocationID = ID

-- | ThingData is parameterised similar to objects.
data ThingData = ThingData {
    _description :: Description,
    _location :: LocationID,
    _initialAppearance :: InitialAppearance,
    _lit :: ThingLit,
    _edible :: Edibility,
    _portable :: Portability,
    _wearable :: Wearability,
    _pushable :: Pushable,
    _handled :: Handled,
    _described :: Described,
    _mentioned :: Mentioned,
    _markedForListing :: MarkedForListing 
    }
    
-- | a door is straightforward.
type OtherSide = ID
data Openable = Openable | Unopenable
data Open = Open | Closed
data DoorData = DoorData ThingData OtherSide Open Openable

type DirectionObj obj = Object obj
type RoomObj obj = Object obj
type ThingObj obj = Object obj
type LocationObj obj = Object obj

-- | This is the big beefy state of everything
-- usr is the user defined collection of rulebooks, actions, and activities.
data World obj usr = World
    {
        _directions :: Map.Map ID (DirectionObj obj),
        _rooms :: Map.Map ID (RoomObj obj),
        _title :: T.Text,
        _msgBuffer :: MessageBuffer,
        _objects :: Map.Map ID (Object obj),
        _std :: StandardLibrary obj usr,
        _usrLibrary :: usr
    }

data MessageBuffer = MessageBuffer
    {
    _indentLvl :: Int,
    _stdBuffer :: [Doc AnsiStyle],
    _dbgBuffer :: [Doc AnsiStyle], -- this one is sent to stderr
    _msgStyle :: Maybe AnsiStyle
    }

-- | the standard library is the equivalent of inform's standard rules.
data StandardLibrary obj usr = StandardLibrary
    {
        _activities :: ActivityCollection obj usr,
        _actions :: ActionCollection obj usr,
        _rulebooks :: RulebookCollection obj usr
    }
-- | An action covers both inform actions and inform activities. I cannot work out what the difference really
-- is from the documentation, except that an action can be explicitly called upon by the player.
-- so for me, the only real difference is going to be parser-level stuff which I can wrap up
-- in a nice maybe. Like we have parameterised over rules and rulebooks andects, we parameterise actions
-- to allow for action variables.

data ActionData act = ActionData
    {
        _currentActor :: ID,
        _currentNoun :: ID,
        _actionVariables :: act
    }

data Action obj usr act = Action
    {
        _actionName :: Name,
        _checkRules :: Maybe (Rulebook obj usr (ActionData act)),
        _carryOutRules :: Rulebook obj usr (ActionData act),
        _reportRules :: Maybe (Rulebook obj usr (ActionData act)),
        -- | given a world, set the initial values of whatever the action variables is
        -- | it has some default set.
        _actionInfo :: ActionData act,
        _setActionVariables :: Maybe (World obj usr -> ActionData act -> ActionData act)
    }
-- | most actions are plain actions.
--type PlainAction usr obj = Action usr obj () 

type RuleOutcome = Maybe Bool
type WorldUpdate obj usr a b  = State (World obj usr, a) b
type WorldRuleState obj usr a  = WorldUpdate obj usr a RuleOutcome 

-- | WorldRuleState is a state processor; you feed it (World, PotentiallyParameters)
-- | and get out (Result, (PotentiallChangedWorld, PotentiallyChangedParameters))
data Rule obj usr a  = Rule 
    {
        _ruleName :: Name,
        _ruleProcessor :: WorldRuleState obj usr a 
    }

anonRule :: WorldRuleState obj usr a  -> Rule obj usr a 
anonRule rule = Rule { _ruleName = "", _ruleProcessor = rule }

data Rulebook obj usr a = Rulebook
    {
        _rulebookName :: Name,
        _firstRules :: [Rule obj usr a],
        _rules :: [Rule obj usr a],
        _lastRules :: [Rule obj usr a],
        _defaultOutcome :: Maybe Bool
    }
    
type PlainRulebook obj usr = Rulebook obj usr ()

data ActivityCollection obj usr = ActivityCollection
    {
        _printingDarkRoomNameActivity :: Action obj usr (),
        _printingNameActivity :: Action obj usr (Object obj)
    }

newtype RulebookCollection obj usr  = RulebookCollection
    {
        _whenPlayBeginsRules :: PlainRulebook obj usr 
    }

newtype ActionCollection obj usr = ActionCollection
    {
        _lookingAction :: Action obj usr LookingActionVariables 
    }

data LookingActionVariables = LookingActionVariables
    {
        _roomDescribingAction :: Name,
        _abbrevFormAllowed :: Bool,
        _visibilityLvlCnt :: Int,
        _visibilityCeiling :: LocationID
    }

makeLenses ''Rulebook
makeLenses ''World
makeLenses ''MessageBuffer
makeLenses ''Object
makeLenses ''Action
makeLenses ''Rule
makeLenses ''ActionData
makeLenses ''LookingActionVariables
makeLenses ''StandardLibrary
makeLenses ''ActivityCollection
makeLenses ''RoomData
makeLenses ''ThingData

-- | some helpful show rules for the debugging
instance Show (Rule obj usr act) where
    show r = T.unpack $ _ruleName r--r ^. ruleName

instance Show (Rulebook obj usr act) where
    show r = T.unpack $ r ^. rulebookName