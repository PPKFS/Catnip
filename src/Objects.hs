{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Objects where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Types
import Utils
import Data.Maybe

lookupID :: World obj usr -> ID -> Maybe (Object obj)
lookupID w i = case x of
    Just y -> x
    Nothing -> Map.lookup i (w ^. objects)
    where x = Map.lookup i (w ^. rooms)

lookupInManyMaps :: Ord a => [Map.Map a b] -> a -> Maybe b
lookupInManyMaps [] _ = Nothing
lookupInManyMaps (x:xs) z = case y of
    Just z -> y
    Nothing -> lookupInManyMaps xs z
    where y = Map.lookup z x

lookupLocation :: World obj usr -> ID -> Maybe (Object obj)
lookupLocation w = lookupInManyMaps [w ^. rooms, w ^. objects]

class HasLocation a where
    getLocation :: HasLocation obj => World obj usr -> a -> LocationObj obj

-- if it's a room, we get back a region.
-- otherwise, we just get the location of the object
instance HasLocation obj => HasLocation (Object obj) where
    getLocation w o = case objType of
                        Room r -> getLocation w r
                        _ -> getLocation w (o ^. location)
                        where objType = o ^. info

instance HasLocation ID where
    getLocation w i = case obj of
                        Nothing -> nowhereRoom
                        Just x -> getLocation w x
                        where obj = lookupLocation w i

instance HasLocation RoomData where
    getLocation w rd = fromMaybe globalRegion $ rd ^. containingRegion >>= lookupID w

getLocationID :: HasLocation x => HasLocation obj => World obj usr -> x -> ID
getLocationID w x = view objID $ getLocation w x

makeObject :: Name -> Description -> ID -> ObjectInfo obj -> Object obj
makeObject n d i = Object i n ImproperNamed SingularNamed 
        (getIndefiniteArticle ImproperNamed SingularNamed (T.head n)) d "" (nowhereRoom ^. objID)
        Lit Inedible Portable Unwearable NotPushableBetweenRooms Unhandled Described Mentioned UnmarkedForListing

makeThing :: Name -> Description -> ID -> Object obj
makeThing n d i = makeObject n d i Thing

-- | if we need to make an indefinite article by default
getIndefiniteArticle :: NameProperness -> NamePlurality -> Char -> T.Text
getIndefiniteArticle properness plurality beginning = 
    case properness of
    ProperNamed -> ""
    ImproperNamed -> 
        case plurality of
            PluralNamed -> "some"
            SingularNamed -> if' (beginning `elem` ['a', 'e', 'i', 'o', 'u']) "an" "a"

instance Show (Object x) where
    show c = T.unpack $ a `T.append` b
        where b = if i /= a then T.concat [" (", i, ")"] else ""
              i = c ^. objID
              a = c ^. name
              
direction :: Name -> Opposite -> DirectionObj obj
direction n opp = set objID n $ makeObject n "" n (Direction opp) 

directionPair :: Name -> Name -> (DirectionObj obj, DirectionObj obj)
directionPair d1 d2 = (direction d1 d2, direction d2 d1) -- since the id of a direction is just its name

constructDirections :: Map.Map ID (DirectionObj obj)
constructDirections = Map.fromList [(n ^. name, n) |(a, b) <-
    [directionPair "north" "south",
     directionPair "west" "east",
     directionPair "northeast" "southwest",
     directionPair "southeast" "northwest",
     directionPair "up" "down",
     directionPair "inside" "outside"],
     n <- [a, b]]

--    ROOM STUFF --
makeRoom :: Name -> Description -> ID -> RoomObj obj
makeRoom n desc i = makeObject n desc i $ Room (RoomData Unvisited Map.empty Nothing)

nowhereRoom :: RoomObj obj
nowhereRoom = makeRoom "Nowhere, The Void, You Screwed Up" 
                "If you're here, you've messed up something chronic." "0xDEADBEEF" 

globalRegion :: ThingObj obj
globalRegion = makeObject "global region" "" "0xDEADBEEF" Region

penID = "test"
blankActionData = ActionData { _currentActor = penID}

pen :: ID -> ThingObj obj
pen = makeThing "Bic pen" "just a pen"

data Enterable = Enterable | NotEnterable
data Opaqueness = Opaque | Transparent
type CarryingCapacity = Int
data ContainerData = ContainerData Opaqueness Enterable Open Openable


-- we start by reading the first 2 characters of the id, which gives us some kind of lookup table as to which record
-- field we want
--stdActions :: Functor f => (ActionCollection -> f (ActionCollection)) -> BaseWorld usr-> f (BaseWorld)
--stdActions = std . actions
--decodeID :: [Char] -> World usr -> (ID -> AnyObject obj)
--decodeID (x1:[x2]) w = undefined --case lookuptable of
    --Just x -> (\id -> Map.lookup id x)
    --Nothing -> (\id -> StdObj (Error pen))
    --where lookuptable =  Map.lookup (x1, x2) $ w ^. idTable
    
--objGet :: ID -> World usr -> AnyObject obj
--objGet id world = lookupTable id
--    where lookupTable = decodeID (T.unpack (T.take 2 id)) world