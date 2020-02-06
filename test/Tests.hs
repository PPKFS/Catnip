{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Test.Tasty
import Test.Tasty.HUnit
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
import Control.Monad.Trans.Reader

type VanillaRules = WorldRules World
type VanillaWorldBuilder = WorldBuilder World VanillaRules

testHarness :: (HasWorld w, HasWorldRules r w) => State (WorldBuilder w r) b 
    -> (World -> w) -> (WorldRules World -> r) -> T.Text  -> Assertion
testHarness w wEmb rEmb ex = do
    let (w', r) = makeWorld wEmb rEmb w
    let w'' = execState (run r) w'
    (output, w) <- flushToString w''
    assertEqual "test" ex output 
    
testHarnessVanilla :: State VanillaWorldBuilder b -> T.Text -> Assertion
testHarnessVanilla w = testHarness w id id
    
--TEST 1 - Bic
checkDesc :: Object-> State World ()
checkDesc o = when (o ^. description == "") (sayModifyLn $ o ^. name <> " has no description.")

example1World :: State VanillaWorldBuilder ()
example1World = do
    setTitle "Bic"
    room <- addRoom $ makeRoom "The Staff Break Room" ""
    addObject $ makeThingWithName "Bic pen"
    -- addObject makePlainThing
    addObject $ makeThing "orange" 
        "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    addObject $ makeThing "napkin" "Slightly crumpled."
    addRule $ makeRule "run property checks at the start of play rule" (\_ -> do
        zoom _1 (do
            w <- get
            iterateThings checkDesc)
        return Nothing)
    --return ""

test_example1 :: Assertion
test_example1 = testHarnessVanilla example1World "The Staff Break Room\nBic pen has no description."
{-
-- TEST 2 - Verbosity

-- example2World :: World
--example2World = makeWorld $ do
--    setTitle "Verbosity"
--    room1 <- addRoom $ makeRoom "Wilkie Memorial Research Wing" "The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment. \n \n The Men's Restroom is immediately west of this point."
 --   room2 <- addRoom $ makeRoom "The Men's Restroom" "Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building, and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things scrawled over the urinals which might have been intended in confidence."
    --positionRoom room2 "west" room1
    --setRoomDescriptionSettings BriefDescriptions
--    return ""

--test_example2 :: IO ()
--test_example2 = testHarness example2World "Wilkie Memorial Research Wing" Nothing
-}

tests :: TestTree
tests = testGroup "Tests" [ testCase "Test 1" test_example1]

main :: IO ()
main = defaultMain tests