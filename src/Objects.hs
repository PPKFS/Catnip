{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Objects where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set
import qualified Data.Text as T
import           Types
import           Utils

iterateObjects :: HasWorld w => (Object -> State w b) -> (Object -> Bool) -> State w ()
iterateObjects f p = do
    w <- get
    mapM_ f (Prelude.filter p $ Map.elems (w ^. objects))

iterateAllObjects :: HasWorld w => (Object -> State w b) -> State w ()
iterateAllObjects = flip iterateObjects (const True)

iterateObjectsOfType :: HasWorld w => (Object -> State w b) -> Type -> State w ()
iterateObjectsOfType f t = iterateObjects f (member t . view objType)

iterateThings :: HasWorld w => (Object -> State w b) -> State w ()
iterateThings = flip iterateObjectsOfType "thing"

getTypes :: ObjectHierarchy -> Type -> Set Type
getTypes m t = case x of
    Just y -> insert y (getTypes m y)
    Nothing -> empty
    where x = Map.lookup t m

makeObject :: Set Type -> Name -> Description -> ID -> Object
makeObject t n d i = Object i t n ImproperNamed SingularNamed 
        (getIndefiniteArticle ImproperNamed SingularNamed (T.head n)) d "" Nothing (nowhereRoom ^. objID)
        Lit Inedible Portable Unwearable NotPushableBetweenRooms Unhandled Described Mentioned UnmarkedForListing

-- | if we need to make an indefinite article by default
getIndefiniteArticle :: NameProperness -> NamePlurality -> Char -> T.Text
getIndefiniteArticle properness plurality beginning = 
    case properness of
    ProperNamed -> ""
    ImproperNamed -> 
        case plurality of
            PluralNamed -> "some"
            SingularNamed -> if' (beginning `elem` ['a', 'e', 'i', 'o', 'u']) "an" "a"

makeRoom :: Name -> Description -> ID -> (Object, RoomData)
makeRoom n desc i = (makeObject (fromList ["room"]) n desc i, RoomData Unvisited Map.empty Nothing) 

nowhereRoom :: Object
nowhereRoom = fst $ makeRoom "Nowhere, The Void, You Screwed Up" 
                "If you're here, you've messed up something chronic." "0xDEADBEEF" 

instance Show Object where
    show c = T.unpack $ a `T.append` b
        where b = if i /= a then T.concat [" (", i, ")"] else ""
              i = c ^. objID
              a = c ^. name

class HasLocation x where
    getLocationID :: HasWorld w => w -> x -> ID
    getLocation :: HasWorld w => w -> x -> Object

instance Eq Object where
    (==) x y = (x ^. objID) == (y ^. objID)

instance HasLocation Object where
    getLocationID _ = _location
    getLocation w i = w ^. objects . at (i ^. location) . non nowhereRoom

instance HasLocation ID where
    getLocationID w i = getLocationID w (w ^. objects . at i . non nowhereRoom)
    getLocation w i = getLocation w (w ^. objects . at i . non nowhereRoom)

makeThing :: Name -> Description -> ID -> Object
makeThing = makeObject (fromList ["thing"])

makePlainThing :: ID -> Object
makePlainThing = makeThing "" ""

makeThingWithName :: Name -> ID -> Object
makeThingWithName n = makeThing n ""

--direction :: Name -> ID -> DirectionObj
--direction n opp = set objID n $ makeObject n "" n (Direction opp) 

--    ROOM STUFF --


{-
instance HasLocation2 RoomData where
    getLocation w rd = fromMaybe globalRegion $ rd ^. containingRegion >>= lookupID w

getLocationID :: HasLocation2 x => World -> x -> ID
getLocationID w x = view objID $ getLocation w x

type ObjectInfo = Int


              


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
-}