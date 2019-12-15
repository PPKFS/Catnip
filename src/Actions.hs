{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
module Actions where

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State
import Objects
import Rulebooks
import Types
import Utils
import SayCommon
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Maybe

-- | trying an action gives an outcome of whether it succeeded and the newly updated
-- world (if it affected the change)
tryAction :: Action obj usr a -> World obj usr -> (Bool, World obj usr)
tryAction action world = case result of
    -- should never result in nothing BUT.
    Nothing -> (True, world3) 
    Just r ->  (r, world3)
    -- run the action processing rulebook.
    -- right now I have to hardcode it because I'm feeding it an 'a' thing.
    -- run the action processing rulebook with initial setting action
    -- and then parse out the results. I guess it's kinda buggy that I can't
    -- work out how to thread it so I don't need to use undefineds or 
    -- to duplicate code but oh well.
    where world2 = sayDbgLn ("Trying to do the " <> (action ^. actionName) <> " action") world
          (result, (world3, _)) = runRulebookInternally actionProcessingRules action world

printName :: Object obj -> World obj usr -> State (World obj usr, b) RuleOutcome
printName op w = runActivity (w ^. std . activities . printingNameActivity) op

-- printNameEx :: World obj usr -> Object obj -> World obj usr
-- LOOKING --
-- consists of looking and all the activities naturally associated with looking
-- mostly that's printing out room

printingNameActivityImpl :: Action obj usr (Object obj)
printingNameActivityImpl = (makeActivity "printing the name of something activity" []){
    _carryOutRules = (blankRulebook "") {
        _lastRules = [ Rule "standard name printing rule" (do
            (w, a) <- get
            sayModifyLn $ a ^. actionVariables . name
            sayModifyLn "aaaaa"
            return Nothing
            )
        ]
    }
}

printingDarkRoomNameActivityImpl :: Action obj usr ()
printingDarkRoomNameActivityImpl = makeActivity "printing the name of a dark room activity" [
        anonRule (do 
            sayModifyLn "Darkness"
            return Nothing)]

lookingSetActionVariablesRules :: World obj usr -> LookingActionVariables
lookingSetActionVariablesRules = undefined

lookingCarryOutRules :: HasLocation obj => Rulebook obj usr (ActionData LookingActionVariables)
lookingCarryOutRules = (blankRulebook "looking carry out rulebook") {
    _rules = [
        Rule "room description heading rule" (do 
            -- | bold type. If in darkness, we print the dark room.
            -- else if we are at some level of visibility ceiling, we say The first.
            -- why does it have equals 3???
            -- otherwise, we just print the name of the room.
            -- cut off bold type, then iterate all levels of visibility holders (supporters, etc)
            -- and append them.
            modifyWorld $ setStyle (Just bold)
            (w, a) <- get
            let loc = getLocation w (a ^. currentActor)
            --sayModifyLn $ fromMaybe pen (lookupID w (a ^. currentActor)) ^. name
            if' ((a ^. actionVariables . visibilityLvlCnt) == 1) 
                (runActivity (w ^. std . activities . printingDarkRoomNameActivity) ())
                $ if' ((a ^. actionVariables . visibilityCeiling) == (loc ^. objID))
                    (printName loc w)
                    (printName loc w)
                    --(printNameEx (loc ^. name) Capitalised Definite w)
            modifyWorld $ setStyle Nothing
            return Nothing
            ),

        Rule "room description body rule" (do 
            (w, a) <- get
            return Nothing
            )
        --Rule "room description paragraphs aboutects rule" desc_obj_rule,
        --Rule "check new arrival rule" check_arrival_rule
    ]
}

lookingActionImpl :: HasLocation obj => Action obj usr LookingActionVariables
lookingActionImpl = Action
    {
        _actionName = "looking",
        _checkRules = Just lookingCarryOutRules,
        _carryOutRules = lookingCarryOutRules,
        _reportRules = Just lookingCarryOutRules,
        _actionInfo = blankActionData {  _actionVariables = LookingActionVariables 
            {
                _roomDescribingAction = "",
                _abbrevFormAllowed = False,
                _visibilityLvlCnt = 0,
                _visibilityCeiling = ""
            }},
        _setActionVariables = Nothing
    }

introText :: World obj usr -> World obj usr
introText = execState (do
    w <- get
    let shortBorder = "------" :: T.Text
        totalLength = 2 * T.length shortBorder + T.length (w ^. title) + 2 :: Int
        longBorder = T.replicate totalLength "-" :: T.Text
    modify $ setStyle (Just (color Green <> bold))
    modify $ sayLn longBorder
    w2 <- get
    modify $ sayLn (shortBorder <> " " <> (w2 ^. title) <> " " <> shortBorder)
    modify $ sayLn longBorder
    modify $ sayLn "\n"
    modify $ setStyle Nothing
    return w)

-- | the when play begins rulebook is mostly identical to the beginning rulebook
-- in inform, plus minus a few random implementation specific bits.
whenPlayBeginsRulesImpl :: HasLocation obj => PlainRulebook obj usr
whenPlayBeginsRulesImpl = (blankRulebook "when play begins rulebook") {
    _firstRules = 
        [
            Rule "display banner rule" (do
                modifyWorld introText
                return Nothing),
            Rule "position player in model world rule" 
                (return Nothing),
            -- | do looking.
            Rule "initial room description rule" (do 
                modifyWorld $ \x -> snd (tryAction lookingActionImpl x)
                return Nothing)
        ]
}
