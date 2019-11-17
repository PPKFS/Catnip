{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Common where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.IO

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

type DirectionMap = Map.Map ID Direction
type ID = T.Text
type Name = T.Text
type ObjectType = T.Text

type Opposite = ID
type Direction = Object Opposite

data NameProperness = ImproperNamed | ProperNamed deriving Show
data NamePlurality = SingularNamed | PluralNamed deriving Show

data Object a = Object 
    {
        _objID :: ID,
        _name :: Name,
        _nameProperness :: NameProperness,
        _namePlurality :: NamePlurality,
        _indefiniteArticle :: T.Text,
        _info :: a
    }
makeLenses ''Object

data Darkness = Lighted | Dark deriving Show
data IsVisited = Visited | Unvisited deriving Show
type Description = T.Text
type MapConnections = Map.Map String String
type ContainingRegion = Maybe ID

data RoomData = RoomData 
    {
        _roomDescription :: Description,
        _darkness :: Darkness,
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: Maybe ID
    } deriving Show

type Room = Object RoomData
type RoomMap = Map.Map ID Room

type Region = Object ()

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

type InitialAppearance = T.Text
type Location = Room

data Capitalisation = Capital | NotCapital
data Definitive = Definite | Indefinite
data PrintingOptions = PrintingOptions Capitalisation Definitive

data ActionData a = ActionData
    {
        _currentActor :: World -> GenericThing,
        _actionVariables :: a
    }

data Action a = Action
    {
        _actionName :: Name,
        _checkRules :: Maybe (Rulebook (ActionData a)),
        _carryOutRules :: Rulebook (ActionData a),
        _reportRules :: Maybe (Rulebook (ActionData a)),
        -- | given a world, set the initial values of whatever the action variables is
        _actionData :: ActionData a,
        _setActionVariables :: Maybe (World -> ActionData a -> a)
    }

data World = World 
    {
        _directions :: DirectionMap,
        _rooms :: RoomMap,
        _title :: T.Text,
        _rulebooks :: Map.Map Name (World -> (RuleOutcome, World)),
        _msgBuffer :: MsgBuffer,
        _printActivity :: Action ()
    }

data ThingData a = ThingData {
    _description :: Description,
    _location :: (World -> Location),
    _initialAppearance :: InitialAppearance,
    _lit :: ThingLit,
    _edible :: Edibility,
    _portable :: Portability,
    _wearable :: Wearability,
    _pushable :: Pushable,
    _handled :: Handled,
    _described :: Described,
    _mentioned :: Mentioned,
    _markedForListing :: MarkedForListing,
    _specificData :: a }

type Thing a = Object (ThingData a)

type GenericThing = Thing ()

data MsgBuffer = MsgBuffer
    {
        _indentLvl :: Int,
        _stdBuffer :: [Doc AnsiStyle],
        _dbgBuffer :: [Doc AnsiStyle], -- this one is sent to stderr
        _msgStyle :: Maybe AnsiStyle
    }

type RuleOutcome = Maybe Bool
type WorldUpdate a b = State (World, a) b
type WorldRuleState a = WorldUpdate a RuleOutcome

-- | WorldRuleState is a state processor; you feed it (World, PotentiallyParameters)
-- | and get out (Result, (PotentiallChangedWorld, PotentiallyChangedParameters))
data Rule a = Rule 
    {
        _ruleName :: Name,
        _rule :: WorldRuleState a
    }


data Rulebook a = Rulebook
    {
        _rulebookName :: Name,
        _firstRules :: [Rule a],
        _rules :: [Rule a],
        _lastRules :: [Rule a],
        _defaultOutcome :: Maybe Bool
    }
makeLenses ''World
makeLenses ''RoomData
makeLenses ''MsgBuffer
makeLenses ''Action
makeLenses ''ActionData