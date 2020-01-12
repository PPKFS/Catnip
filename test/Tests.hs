{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Test.Framework
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Objects
import Rulebooks
import Actions
import System.IO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Utils
import WorldBuilder
import SayCommon
import Types


type UserLibrary = Int
type UserObjects = ()
type UserActions = Int

instance HasLocation UserObjects where
    getLocation _ _ = nowhereRoom

testHarness :: World -> T.Text -> Maybe TestMeWith -> IO ()
testHarness w e ac = do
    let w' = run w
    let w'' = w'--runActions ac w'
    (output, w) <- flushToString w''
    assertEqual e output 

--TEST 1 - Bic
checkDesc :: Object-> State (World) ()
checkDesc o = when (o ^. description == "") (sayModifyLn $ o ^. name <> " has no description.")

example1World :: World
example1World = makeWorld $ do
    setTitle "Bic"
    room <- addRoom $ makeRoom "The Staff Break Room" ""
    addObject $ makeThingWithName "Bic pen"
    -- addObject makePlainThing
    addObject $ makeThing "orange" 
        "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    addObject $ makeThing "napkin" "Slightly crumpled."
    addRule "when play begins rulebook" (Rule "run property checks at the start of play rule" (do
        zoom _1 (do
            w <- get
            iterateNonRooms checkDesc)
        return Nothing))
    return ""

test_example1 :: IO ()
test_example1 = testHarness example1World "The Staff Break Room\nBic pen has no description." Nothing

-- TEST 2 - Verbosity

example2World :: World
example2World = makeWorld $ do
    setTitle "Verbosity"
    room1 <- addRoom $ makeRoom "Wilkie Memorial Research Wing" "The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment. \n \n The Men's Restroom is immediately west of this point."
    room2 <- addRoom $ makeRoom "The Men's Restroom" "Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building, and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things scrawled over the urinals which might have been intended in confidence."
    positionRoom room2 "west" room1
    setRoomDescriptionSettings BriefDescriptions
    return ""

test_example2 :: IO ()
test_example2 = testHarness example2World "Wilkie Memorial Research Wing" Nothing

main :: IO ()
main = htfMain htf_thisModulesTests