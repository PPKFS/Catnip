{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
module Actions where

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State
import Rulebooks
import Types
import Utils
import SayCommon
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Maybe
import Objects

introText :: HasWorld w => State w ()
introText = do
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
    return ()

-- | the when play begins rulebook is mostly identical to the beginning rulebook
-- in inform, plus minus a few random implementation specific bits.
whenPlayBeginsRulesImpl :: (HasWorld w) => PlainRulebook w
whenPlayBeginsRulesImpl = (blankRulebook "when play begins rulebook") {
    _firstRules = 
        [
            makeRule "display banner rule" (\_ -> do
                zoom _1 introText
                return Nothing),
            makeRule "position player in model world rule" (\_ -> do
                zoom _1 (do
                    w <- get
                    objects . ix (w ^. player) . location .= (w ^. firstRoom))
                    --move <$> use player <*> use firstRoom)
                return Nothing),
            -- | do looking.
            makeRule "initial room description rule" (\r -> do
                _ <- zoom _1 $ tryAction (r ^. actions . lookingAction) r
                return Nothing)
        ]
}

blankActionData :: a -> ActionData a
blankActionData a = ActionData { _currentActor = "NA", _currentNoun = "NA", _actionVariables = a}

tryAction :: (HasWorld w, HasWorldRules r w) => Action w act -> r -> State w ActionOutcome
tryAction ac r = do
    sayDbgModifyLn ("Trying to do the " <> (ac ^. actionName) <> " action")
    w <- get
    x <- zoomOut (runRulebook actionProcessingRules r) (ac & (actionInfo . currentActor) .~ (w ^. player))
    return $ fromMaybe True x

lookingSetActionVariablesRules :: RoomDescriptionSetting -> w -> ActionData LookingActionVariables -> 
    ActionData LookingActionVariables
lookingSetActionVariablesRules r _ = set (actionVariables . roomDescriptionSetting) r

lookingActionImpl :: HasWorld w => Action w LookingActionVariables
lookingActionImpl = Action
    {
        _actionName = "looking",
        _checkRules = Nothing,--Just lookingCarryOutRules,
        _carryOutRules = lookingCarryOutRules,
        _reportRules = Nothing,--Just lookingCarryOutRules,
        _actionInfo = blankActionData $ LookingActionVariables 
            {
                _roomDescribingAction = "",
                _abbrevFormAllowed = False,
                _visibilityLvlCnt = 1,
                _visibilityCeiling = "",
                _abbreviatedFormAllowed = False,
                _roomDescriptionSetting = NormalDescriptions
            },
        _setActionVariables = Just $ lookingSetActionVariablesRules NormalDescriptions
    }

getVisibilityLevelCount :: ActionData LookingActionVariables -> Int
getVisibilityLevelCount = view $ actionVariables . visibilityLvlCnt

lookingCarryOutRules :: HasWorld w => Rulebook w (ActionData LookingActionVariables)
lookingCarryOutRules = (blankRulebook "looking carry out rulebook") {
    _rules = [
        makeRule "room description heading rule" (\r -> do 
            -- | bold type. If in darkness, we print the dark room.
            -- else if we are at some level of visibility ceiling, we say The first.
            -- why does it have equals 3???
            -- otherwise, we just print the name of the room.
            -- cut off bold type, then iterate all levels of visibility holders (supporters, etc)
            -- and append them.
            modifyR $ setStyle (Just bold)
            (w, a) <- get
            let visCeiling = a ^. actionVariables . visibilityCeiling
            let visCnt = getVisibilityLevelCount a
            let loc = getLocation w (a ^. currentActor)
            _ <- if' (visCnt == 0)
                (zoom _1 $ runActivity (r ^. activities . printingDarkRoomNameActivity) r)
                $ if' (visCeiling == (loc ^. objID))
                    (zoom _1 $ printName r loc)
                    (zoom _1 $ printNameExtended r loc (NameStyle Capitalised Definite))
            modifyR $ setStyle Nothing
            return Nothing
            ),

        makeRule "room description body text rule" (\r -> do
            (w, a) <- get
            let visCount = getVisibilityLevelCount a
            let loc = getLocationID w $ a ^. currentActor
            let descSetting = a ^. actionVariables . roomDescriptionSetting
            -- if we are in darkness, and in brief mode, or normal mode and we are allowed abbreviated form(?)
            -- and we've seen the darkness before, then do nothing
            if visCount == 0 then (
                if descSetting == BriefDescriptions || 
                    (descSetting == NormalDescriptions && a ^. actionVariables . abbreviatedFormAllowed 
                        && w ^. theDarknessWitnessed) then return Nothing 
                else zoom _1 $ runActivity (r ^. activities . printingDarkRoomDescriptionActivity) r)
            else ( -- the vis ceiling is the room, and the various tests pass
                if (a ^. actionVariables . visibilityCeiling) == loc 
                    then 
                        if descSetting == BriefDescriptions || (descSetting == NormalDescriptions 
                            && a ^. actionVariables . abbreviatedFormAllowed)
                            then return Nothing else return $ Just False
                    else return Nothing)
        )
        --Rule "room description paragraphs aboutects rule" desc_obj_rule,
        --Rule "check new arrival rule" check_arrival_rule
    ]
}
printName :: (HasWorld w, HasWorldRules r w) => r -> Object -> State w RuleOutcome
printName r o = printNameExtended r o defaultStyle

printNameExtended :: (HasWorld w, HasWorldRules r w) => r -> Object -> NameStyle -> State w RuleOutcome
printNameExtended r obj s = runActivity ((r ^. activities . printingNameActivity) obj s) r

printingNameActivityImpl :: HasWorld w => Object -> NameStyle -> Action w ()
printingNameActivityImpl obj _ = (makeActivity "printing the name of something activity" []){
    _carryOutRules = (blankRulebook "") {
        _lastRules = [ makeRule "standard name printing rule" (\_ -> do
            sayModifyLn $ obj ^. name
            return Nothing
            )
        ]
    }
}

printingDarkRoomNameActivityImpl :: HasWorld w => Action w ()
printingDarkRoomNameActivityImpl = makeActivity "printing the name of a dark room activity" [
        anonRule (\_ -> do 
            sayModifyLn "Darkness"
            return Nothing)]

printingDarkRoomDescriptionActivityImpl :: HasWorld w => Action w ()
printingDarkRoomDescriptionActivityImpl = makeActivity "printing the description of a dark room activity" [
        anonRule (\_ -> do 
            sayModifyLn "It is pitch dark, and you can't see a thing."
            return Nothing)]