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
lookupID w i = Map.lookup i (w ^. objects)
    
class HasLocation a where
    getLocation :: HasLocation obj => World obj usr -> a -> LocationObj obj

instance HasLocation obj => HasLocation (Object obj) where
    getLocation w o = getLocation w (o ^. info)

instance HasLocation obj => HasLocation (ObjectInfo obj) where
    getLocation w (Room x) = getLocation w x
    getLocation w (Thing x) = getLocation w x
    getLocation w (ExtInfo y) = getLocation w y

instance HasLocation ID where
    getLocation w i = case obj of
                        Nothing -> nowhereRoom
                        Just x -> getLocation w x
                        where obj = lookupID w i

instance HasLocation RoomData where
    getLocation w rd = fromMaybe globalRegion $ rd ^. containingRegion >>= lookupID w
    
instance HasLocation ThingData where
    getLocation w t = fromMaybe nowhereRoom (do
        let x = t ^. location
        lookupID w x)

-- TODO: add a properly random way to generate IDs
randomList :: T.Text
randomList = "f6h3"

lengthNameID :: Int
lengthNameID = 4

mkID :: Name -> ID
mkID n = T.take lengthNameID n `T.append` randomList

makeObject :: Name -> ObjectInfo obj -> Object obj
makeObject n = Object (mkID n) n ImproperNamed SingularNamed 
        (getIndefiniteArticle ImproperNamed SingularNamed (T.head n))

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
direction n opp = set objID n $ makeObject n (Direction opp)

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
makeRoom :: Name -> Description -> RoomObj obj
makeRoom n desc = makeObject n $ Room (RoomData desc Lighted Unvisited Map.empty Nothing)

nowhereRoom :: (RoomObj obj)
nowhereRoom = (makeRoom "Nowhere, The Void, You Screwed Up" "If you're here, you've messed up something chronic.") 
                { _objID = "0xDEADBEEF"} 

globalRegion :: ThingObj obj
globalRegion = set objID "GLOBAL_REGION" $ makeObject "global region" Region

penID = "test"
blankActionData = ActionData { _currentActor = penID}

makeThing :: Name -> Description -> ThingObj obj
makeThing n d = makeObject n $ Thing (ThingData d (nowhereRoom ^. objID) "" 
    Lit Inedible Portable Unwearable NotPushableBetweenRooms Unhandled Described Mentioned UnmarkedForListing)

pen :: ThingObj obj
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