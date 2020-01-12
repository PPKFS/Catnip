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
import Control.Monad.State
import Data.Set

lookupID :: World  -> ID -> Maybe (Object)
lookupID w i = Map.lookup i (w ^. objects)

iterateObjectsWhere :: (Object -> State (World ) b) -> (Object -> Bool) 
    -> State (World ) ()
iterateObjectsWhere f p = do
    w <- get
    mapM_ f (Prelude.filter p $ Map.elems (w ^. objects))

iterateNonRooms :: (Object -> State (World ) b) -> State (World ) ()
iterateNonRooms f = do
    w <- get
    iterateObjectsWhere f (\x -> not $ member (x ^. objID) (w ^. rooms))

iterateAllObjects :: (Object -> State (World ) b) -> State (World ) ()
iterateAllObjects = flip iterateObjectsWhere (const True)

class HasLocation a where
    getLocation :: World -> a -> LocationObj

-- if it's a room, we get back a region.
-- otherwise, we just get the location of the object
instance HasLocation (Object) where
    getLocation w o = case objType of
                        Room r -> getLocation w r
                        _ -> fromJust $ lookupID w (o ^. location)
                        where objType = o ^. info
-- TODO: find better error handling.
instance HasLocation ID where
    getLocation w i = case obj of
                        Nothing -> error (show i)
                        Just x -> getLocation w x
                        where obj = lookupID w i

instance HasLocation RoomData where
    getLocation w rd = fromMaybe globalRegion $ rd ^. containingRegion >>= lookupID w

getLocationID :: HasLocation x => World -> x -> ID
getLocationID w x = view objID $ getLocation w x

makeObject :: Name -> Description -> ID -> ObjectInfo -> Object
makeObject n d i = Object i n ImproperNamed SingularNamed 
        (getIndefiniteArticle ImproperNamed SingularNamed (T.head n)) d "" (nowhereRoom ^. objID)
        Lit Inedible Portable Unwearable NotPushableBetweenRooms Unhandled Described Mentioned UnmarkedForListing

makeThing :: Name -> Description -> ID -> Object
makeThing n d i = makeObject n d i Thing

makePlainThing :: ID -> Object
makePlainThing = makeThing "" ""

makeThingWithName :: Name -> ID -> Object
makeThingWithName n = makeThing n ""

-- | if we need to make an indefinite article by default
getIndefiniteArticle :: NameProperness -> NamePlurality -> Char -> T.Text
getIndefiniteArticle properness plurality beginning = 
    case properness of
    ProperNamed -> ""
    ImproperNamed -> 
        case plurality of
            PluralNamed -> "some"
            SingularNamed -> if' (beginning `elem` ['a', 'e', 'i', 'o', 'u']) "an" "a"

instance Show (Object) where
    show c = T.unpack $ a `T.append` b
        where b = if i /= a then T.concat [" (", i, ")"] else ""
              i = c ^. objID
              a = c ^. name
              
direction :: Name -> Opposite -> DirectionObj
direction n opp = set objID n $ makeObject n "" n (Direction opp) 

directionPair :: Name -> Name -> (DirectionObj, DirectionObj)
directionPair d1 d2 = (direction d1 d2, direction d2 d1) -- since the id of a direction is just its name

constructDirections :: Map.Map ID (DirectionObj)
constructDirections = Map.fromList [(n ^. name, n) |(a, b) <-
    [directionPair "north" "south",
     directionPair "west" "east",
     directionPair "northeast" "southwest",
     directionPair "southeast" "northwest",
     directionPair "up" "down",
     directionPair "inside" "outside"],
     n <- [a, b]]

--    ROOM STUFF --
makeRoom :: Name -> Description -> ID -> RoomObj
makeRoom n desc i = makeObject n desc i $ Room (RoomData Unvisited Map.empty Nothing)

nowhereRoom :: RoomObj
nowhereRoom = makeRoom "Nowhere, The Void, You Screwed Up" 
                "If you're here, you've messed up something chronic." "0xDEADBEEF" 
                
globalRegion :: ThingObj
globalRegion = makeObject "global region" "" "0xDEADBEEF" Region

penID = "test"
blankActionData = ActionData { _currentActor = penID}

pen :: ID -> ThingObj
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