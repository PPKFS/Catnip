{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Set
import Utils
import Control.Monad.Trans.Reader

-- | Everything that can be interacted with is an object.
-- it may be abstract (idk, an idea???) or concrete (a thing).
-- however, everything that is expected to be interacted with in a game setting
-- should probably be an object or something close to.
-- note that an object does not include specific info 
-- that is stored somewhere else, referenced by 'type', which...somehow needs a hierarchy
-- like "deadend" implies it's a dead end, and an indoor room, and therefore a room too...
-- this does mean we need 'smart' object construction...
data Object = Object
    {
        _objID :: ID,
        _objType :: Set Type,
        _name :: Name,
        _nameProperness :: NameProperness,
        _namePlurality :: NamePlurality,
        _indefiniteArticle :: T.Text,
        _description :: Description,
        _initialAppearance :: InitialAppearance,
        _enclosedBy :: Maybe ID,
        _location :: LocationID,
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

--hierarchy should be: map of a thing to its direct parent, which you can then look up again until you get nothing
type ObjectHierarchy = Map.Map Type Type

-- | And our basic field types
type ID = T.Text
type Type = T.Text
type Name = T.Text
data NameProperness = ImproperNamed | ProperNamed deriving Show
data NamePlurality = SingularNamed | PluralNamed deriving Show

data Capitalisation = Capitalised | Uncapitalised
data Definiteness = Indefinite | Definite
data NameStyle = NameStyle Capitalisation Definiteness

-- | RoomData is fairly straightforward.
data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

data Darkness = Lighted | Dark deriving Show
data IsVisited = Visited | Unvisited deriving Show
type Description = T.Text
type MapConnections = Map.Map ID ID
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

-- a location is always a room.
type LocationID = ID

-- | a door is straightforward.
type OtherSide = ID
data Openable = Openable | Unopenable
data Open = Open | Closed
data DoorData = DoorData OtherSide Open Openable
type Opposite = ID

--w is the overarching 'world'
--r is the overarching ruleset
--rulebooks need r to call other rulebooks
--HasRuleSet r a w means "the object r has a ruleset of type a over world w"
data World = World
    {
        _directions :: Map.Map ID Opposite,
        _rooms :: Map.Map ID RoomData,
        _title :: T.Text,
        _msgBuffer :: MessageBuffer,
        _objects :: ObjectStore,
        _player :: ID,
        _firstRoom :: ID,
        _nextObjID :: ID,
        _theDarknessWitnessed :: Bool
    }

data MessageBuffer = MessageBuffer
    {
        _indentLvl :: Int,
        _stdBuffer :: [Doc AnsiStyle],
        _dbgBuffer :: [Doc AnsiStyle], -- this one is sent to stderr
        _msgStyle :: Maybe AnsiStyle
    }

type ObjectStore = Map.Map ID Object

data WorldRules w = WorldRules
    {
        _activities :: ActivityCollection w,
        _actions :: ActionCollection w,
        _rulebooks :: RulebookCollection w
    }

newtype RulebookCollection w = RulebookCollection
    {
        _whenPlayBeginsRules :: PlainRulebook w
    }

type RuleOutcome = Maybe Bool
type ActionOutcome = Bool
type WorldUpdate w a b = State (w, a) b

class HasWorldRules t w | t -> w where
    worldRules :: Lens' t (WorldRules w)
    actions :: Lens' t (ActionCollection w)
    actions = worldRules . go where go f (WorldRules a x y) = (\x' -> WorldRules a x' y) <$> f x
    rulebooks :: Lens' t (RulebookCollection w)
    rulebooks = worldRules . go where go f (WorldRules a x y) = WorldRules a x <$> f y
    activities :: Lens' t (ActivityCollection w)
    activities = worldRules . go where go f (WorldRules a x y) = (\a' -> WorldRules a' x y) <$> f a

instance HasWorldRules (WorldRules w) w where
    worldRules = id

newtype WorldRuleState w a = WorldRuleState {
    unwrapRule :: forall r. HasWorldRules r w => r -> WorldUpdate w a RuleOutcome
}
makeRule :: Name -> (forall r. HasWorldRules r w => r -> WorldUpdate w a RuleOutcome) -> Rule w a
makeRule n r = Rule { _ruleName = n, _doRule = WorldRuleState r}

anonRule :: (forall r. HasWorldRules r w => r -> WorldUpdate w a RuleOutcome) -> Rule w a
anonRule = makeRule ""

-- | WorldRuleState is a state processor; you feed it (World, PotentiallyParameters)
-- | and get out (Result, (PotentiallyChangedWorld, PotentiallyChangedParameters))
data Rule w a = Rule
    {
        _ruleName :: Name,
        _doRule :: WorldRuleState w a
    }

data Rulebook w a = Rulebook
    {
        _rulebookName :: Name,
        _firstRules :: [Rule w a],
        _rules :: [Rule w a],
        _lastRules :: [Rule w a],
        _defaultOutcome :: Maybe Bool
    }

type PlainRulebook w = Rulebook w ()

newtype ActionCollection w = ActionCollection
    {
        _lookingAction :: Action w LookingActionVariables
    }

data ActionData act = ActionData
    {
        _currentActor :: ID,
        _currentNoun :: ID,
        _actionVariables :: act
    }

data Action w act = Action
    {
        _actionName :: Name,
        _checkRules :: Maybe (Rulebook w (ActionData act)),
        _carryOutRules :: Rulebook w (ActionData act),
        _reportRules :: Maybe (Rulebook w (ActionData act)),
        -- | given a world, set the initial values of whatever the action variables is
        -- | it has some default set.
        _actionInfo :: ActionData act,
        _setActionVariables :: Maybe (w -> ActionData act -> ActionData act)
    }

type PlainAction w = Action w ()

data ActivityCollection w = ActivityCollection
    {
        _printingDarkRoomNameActivity :: PlainAction w,
        _printingDarkRoomDescriptionActivity :: PlainAction w,
        _printingNameActivity :: Object -> NameStyle -> PlainAction w
    }

data RoomDescriptionSetting = NormalDescriptions | BriefDescriptions | VerboseDescriptions deriving (Eq, Show)

data LookingActionVariables = LookingActionVariables
    {
        _roomDescribingAction :: Name,
        _abbrevFormAllowed :: Bool,
        _visibilityLvlCnt :: Int,
        _visibilityCeiling :: LocationID,
        _roomDescriptionSetting :: RoomDescriptionSetting,
        {- We also set "abbreviated form allowed" to "true": when the ordinary looking action is running, this
        is "false".FINALLY found an explanation of this property in microsoft's repo somehow
        -}
        _abbreviatedFormAllowed :: Bool
    }

makeClassy ''Object
makeClassy ''MessageBuffer
makeClassy ''RoomData
makeClassy ''World
makeClassy ''RulebookCollection
makeClassy ''Rulebook
makeClassy ''Rule
makeClassy ''ActionCollection
makeClassy ''Action
makeClassy ''ActionData
makeClassy ''LookingActionVariables
makeClassy ''ActivityCollection

instance HasWorld w => HasWorld (w, a) where
    world = _1 . world