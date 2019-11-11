{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Objects where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Common



-- TODO: add a properly random way to generate IDs
randomList :: T.Text
randomList = "f6h3"

class HasType a where
    objectType :: a -> ObjectType

instance Show (Object x) where
    show c = T.unpack $ a `T.append` b
        where b = if i /= a then T.concat [" (", i, ")"] else ""
              i = c ^. objID
              a = c ^. name
              
lengthNameID :: Int
lengthNameID = 4

mkID :: Name -> ID
mkID n = (T.take lengthNameID n) `T.append` randomList

defaultObject :: Name -> (a -> Object a)
defaultObject n = Object (mkID n) n ImproperNamed SingularNamed

direction :: Name -> Opposite -> Direction
direction n opp = set objID n $ defaultObject n opp

instance HasType Direction where
    objectType _ = "direction"

opposite :: Direction -> Name -- this really should include the reader/state/thingy?
opposite = view info
directionPair :: Name -> Name -> (Direction, Direction)
directionPair d1 d2 = (direction d1 d2, direction d2 d1) -- since the id of a direction is just its name

constructDirections :: DirectionMap
constructDirections = Map.fromList [(n ^. name, n) |(a, b) <-
    [directionPair "north" "south",
     directionPair "west" "east",
     directionPair "northeast" "southwest",
     directionPair "southeast" "northwest",
     directionPair "up" "down",
     directionPair "inside" "outside"],
     n <- [a, b]]

--    ROOM STUFF --
room :: Name -> Description -> Room
room n desc = defaultObject n $ RoomData desc Lighted Unvisited Map.empty Nothing

nowhereRoom :: Room
nowhereRoom = (room "Nowhere, The Void, You Screwed Up" "If you're here, you've messed up something chronic.") 
                { _objID = "0xDEADBEEF"} 

instance HasType Room where
    objectType _ = "room"

instance HasType GenericThing where
    objectType _ = "thing"

defaultThing :: Name -> Description -> (a -> Thing a)
defaultThing n d a = defaultObject n $ ThingData d (\w -> nowhereRoom) "" 
    Lit Inedible Portable Unwearable NotPushableBetweenRooms Unhandled Described Mentioned UnmarkedForListing a
genericThing :: Name -> Description -> GenericThing
genericThing n d = defaultThing n d ()

pen :: GenericThing
pen = genericThing "Bic pen" "just a pen"

type OtherSide = ID
data Openable = Openable | Unopenable
data Open = Open | Closed
data DoorData = DoorData OtherSide Open Openable
type Door = Thing DoorData

instance HasType (Thing DoorData) where
    objectType _ = "door"

data Enterable = Enterable | NotEnterable
data Opaqueness = Opaque | Transparent
type CarryingCapacity = Int
data ContainerData = ContainerData Opaqueness Enterable Open Openable
type Container = Thing ContainerData

instance HasType Container where
    objectType _ = "container"

makeLenses ''ThingData
makeLenses ''Rulebook
makeLenses ''Rule
makeLenses ''ActionData