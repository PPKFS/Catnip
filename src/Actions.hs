{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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

tryAction :: Action obj usr act -> State (World obj usr) ActionOutcome
tryAction ac = do
    sayDbgModifyLn ("Trying to do the " <> (ac ^. actionName) <> " action")
    w <- get
    x <- zoomOut (runRulebook actionProcessingRules) (ac & (actionInfo . currentActor) .~ (w ^. player))
    return $ fromMaybe True x

printName :: Object obj -> State (World obj usr) RuleOutcome
printName o = printNameExtended o defaultStyle

printNameR :: Object obj -> State (World obj usr, b) RuleOutcome
printNameR o = zoom _1 (printName o)

printNameExtended :: Object obj -> NameStyle -> State (World obj usr) RuleOutcome
printNameExtended obj s = do
    w <- get
    runActivity ((w ^. std . activities . printingNameActivity) obj s)

printNameExtendedR :: Object obj -> NameStyle -> State (World obj usr, b) RuleOutcome
printNameExtendedR obj s = zoom _1 (printNameExtended obj s)
-- LOOKING --
-- consists of looking and all the activities naturally associated with looking
-- mostly that's printing out room

--move :: HasLocation x => x -> ID -> State (World obj usr) ()
--move thingToMove newLoc = do sayModifyLn $ "hello" <> newLoc

    --do
   -- world <- getWorld
   -- let world2 = world :: World obj usr
    --let currLoc = getLocation thingToMove
    --let currLocID = currLoc ^. objID
    --let newLoc = currLoc & contains %~ (delete currLocID)
   -- return ""
    --_2 . objects . at currLocID ?= (currLoc . contains . at  )
    --remove things from the current location
    --add it to the new location
    --set it to the new location too

printingNameActivityImpl :: Object obj -> NameStyle -> Action obj usr ()
printingNameActivityImpl obj _ = (makeActivity "printing the name of something activity" []){
    _carryOutRules = (blankRulebook "") {
        _lastRules = [ Rule "standard name printing rule" (do
            sayModifyLnR $ obj ^. name
            return Nothing
            )
        ]
    }
}

printingDarkRoomNameActivityImpl :: Action obj usr ()
printingDarkRoomNameActivityImpl = makeActivity "printing the name of a dark room activity" [
        anonRule (do 
            sayModifyLnR "Darkness"
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
            modifyR $ setStyle (Just bold)
            (w, a) <- get
            let loc = getLocation w (a ^. currentActor)
            _ <- if' ((a ^. actionVariables . visibilityLvlCnt) == 1)
                (runActivityR (w ^. std . activities . printingDarkRoomNameActivity))
                $ if' ((a ^. actionVariables . visibilityCeiling) == (loc ^. objID))
                    (printNameR loc)
                    (printNameExtendedR loc (NameStyle Capitalised Definite))
            modifyR $ setStyle Nothing
            return Nothing
            ),

        Rule "room description body rule" (return Nothing)
        --Rule "room description paragraphs aboutects rule" desc_obj_rule,
        --Rule "check new arrival rule" check_arrival_rule
    ]
}

lookingActionImpl :: HasLocation obj => Action obj usr LookingActionVariables
lookingActionImpl = Action
    {
        _actionName = "looking",
        _checkRules = Nothing,--Just lookingCarryOutRules,
        _carryOutRules = lookingCarryOutRules,
        _reportRules = Nothing,--Just lookingCarryOutRules,
        _actionInfo = blankActionData {  _actionVariables = LookingActionVariables 
            {
                _roomDescribingAction = "",
                _abbrevFormAllowed = False,
                _visibilityLvlCnt = 0,
                _visibilityCeiling = ""
            }},
        _setActionVariables = Nothing
    }

introText :: State (World obj usr) ()
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
whenPlayBeginsRulesImpl :: HasLocation obj => PlainRulebook obj usr
whenPlayBeginsRulesImpl = (blankRulebook "when play begins rulebook") {
    _firstRules = 
        [
            Rule "display banner rule" (do
                zoom _1 introText
                return Nothing),
            Rule "position player in model world rule" (do
                zoom _1 (do
                    w <- get
                    objects . ix (w ^. player) . location .= (w ^. firstRoom))
                    --move <$> use player <*> use firstRoom)
                return Nothing),
            -- | do looking.
            Rule "initial room description rule" (do 
                _ <- zoom _1 $ do { w <- get ; tryAction (w ^. std . actions . lookingAction)}
                return Nothing)
        ]
}
