{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Actions where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Objects
import Rulebooks
import Common
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- | trying an action gives an outcome of whether it succeeded and the newly updated
-- world (if it affected the change)
try_action :: Action a -> World -> (Bool, World)
try_action action world = case result of
    -- should never result in nothing BUT.
    Nothing -> (True, world3) 
    Just r ->  (r, world3)
    -- run the (hardcoded) action processing rulebook.
    -- right now I have to hardcode it because I'm feeding it an 'a' thing.
    -- run the action processing rulebook with initial setting action
    -- and then parse out the results. I guess it's kinda buggy that I can't
    -- work out how to thread it so I don't need to use undefineds or 
    -- to duplicate code but oh well.
    where world2 = sayDbgLn ("Trying to do the " <> (action ^. actionName) <> " action") world
          (result, (world3, _)) = (runRulebookInternally actionProcessingRules action world)

printNameEx :: Object a -> PrintingOptions -> World -> State (World, b) RuleOutcome
printNameEx obj op w = runActivity (w ^. printActivity) 
-- LOOKING --
-- consists of looking and all the activities naturally associated with looking
-- mostly that's printing out room

data LookingActionVariables = LookingActionVariables
    {
        _roomDescribingAction :: Name,
        _abbrevFormAllowed :: Bool,
        _visibilityLvlCnt :: Int,
        _visibilityCeiling :: ID
    }
makeLenses ''LookingActionVariables

printingDarkRoomNameActivity :: PlainAction
printingDarkRoomNameActivity = makeActivity "printing the name of a dark room activity" [
        Rule "" (do 
            sayModifyLn "Darkness"
            return Nothing)]

lookingSetActionVariablesRules :: World -> LookingActionVariables
lookingSetActionVariablesRules = undefined

lookingCarryOutRules :: Rulebook (ActionData LookingActionVariables)
lookingCarryOutRules = (blankRulebook "looking carry out rulebook") {
    _rules = [
        Rule "room description heading rule" (do 
            -- | bold type. If in darkness, we print the dark room.
            -- else if we are at some level of visibility ceiling, we say The first.
            -- otherwise, we just print the name of the room.
            -- cut off bold type, then iterate all levels of visibility holders (supporters, etc)
            -- and append them.
            modifyWorld $ setStyle (Just bold)
            (w, a) <- get
            let actor = (a ^. currentActor $ w) 
                loc = (actor ^. info) ^. location $ w
            let cond = ((a ^. actionVariables . visibilityLvlCnt) == 0) 
            if' cond
                (do { (runActivity printingDarkRoomNameActivity) ; return () })
                $ if' ((a ^. actionVariables . visibilityCeiling) == (loc ^. objID))
                    (sayModifyLn (loc ^. name))
                    (sayModifyLn (loc ^. name))
                    --(printName (loc ^. name) w)id
                    --(printNameEx (loc ^. name) Capitalised Definite w)
            modifyWorld $ setStyle Nothing
            return Nothing
            ),

        Rule "room description body rule" (do 
            (w, a) <- get
            let actor = (a ^. currentActor $ w) 
                loc = (actor ^. info) ^. location $ w
            sayModifyLn $ (loc ^. info ^. roomDescription)
            return Nothing
            )
        --Rule "room description paragraphs about objects rule" desc_obj_rule,
        --Rule "check new arrival rule" check_arrival_rule
    ]
}

lookingAction :: Action LookingActionVariables
lookingAction = Action
    {
        _actionName = "looking",
        _checkRules = Just lookingCarryOutRules,
        _carryOutRules = lookingCarryOutRules,
        _reportRules = Just lookingCarryOutRules,
        _actionData = blankActionData {  _actionVariables = LookingActionVariables 
            {
                _roomDescribingAction = "",
                _abbrevFormAllowed = False,
                _visibilityLvlCnt = 0,
                _visibilityCeiling = ""
            }}
    }


-- | the when play begins rulebook is mostly identical to the beginning rulebook
-- in inform, plus minus a few random implementation specific bits.
whenPlayBeginsRules :: PlainRulebook
whenPlayBeginsRules = (plainRulebook "when play begins rulebook") {
    _firstRules = 
        [
            Rule "display banner rule" (do
                modifyWorld introText
                return Nothing),
            Rule "position player in model world rule" (do
                -- TODO
                return Nothing),
            -- | do looking.
            Rule "initial room description rule" (do 
                modifyWorld $ \x -> (snd (try_action (lookingAction) x))
                return Nothing)
        ]
}
