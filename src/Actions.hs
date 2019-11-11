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

introText :: World -> World
introText = execState (do
    w <- get
    let shortBorder = "------" :: T.Text
        totalLength = (2 * (T.length shortBorder)) + T.length (w ^. title) + 2 :: Int
        longBorder = T.replicate totalLength "-" :: T.Text
    modify $ sayLn longBorder
    w <- get
    modify $ sayLn (shortBorder <> " " <> (w ^. title) <> " " <> shortBorder)
    modify $ sayLn longBorder
    modify $ sayLn (T.replicate 3 "\n")
    return w)

blankActionData = ActionData { _actionName = "aaaaaaa", _currentActor = \w -> pen }

--also need to set variables
try_action :: ActionData -> World -> (Bool, World)
try_action a w = case x of
    (Nothing, w') -> (True, w') -- should never occur
    (Just r, w') ->  (r, w')
    where x = (getRulebook w3 "action processing rulebook") w3
          w2 = sayLn ("Trying to do the " <> (a ^. actionName) <> " action") w
          w3 = w2 { _actionVariables = a }

whenPlayBeginsRules :: PlainRulebook
whenPlayBeginsRules = (blankRulebook "when play begins rulebook") {
    _firstRules = 
        [
            Rule "display banner rule" (do
                modifyWorld introText
                return Nothing),
            Rule "position player in model world rule" (do
                -- TODO
                return Nothing),
            Rule "initial room description rule" (do 
                modifyWorld $ \x -> (snd (try_action (blankActionData { _actionName = "looking"}) x)) 
                return Nothing)
        ]
}

actionProcessingRules :: Rulebook ActionData
actionProcessingRules = (blankRulebook "action processing rulebook") {
    _firstRules = 
        [
            Rule "announce items from multiple object lists rule" $ blankRule,
            Rule "set pronouns from items from multiple object lists rule" $ blankRule,
            --skipping annouce items, set pronouns
            Rule "before stage rule" $ abideByRulebook "before stage rulebook"
        ],
    _rules = 
        [
            Rule "basic visibility rule" $ blankRule,
            Rule "basic accessibility rule" $ blankRule,
            Rule "carrying requirements rule" $ blankRule
        ],
    _lastRules = 
        [
            Rule "instead stage rule" $ abideByRulebook "instead stage rulebook",
            Rule "requested actions require persuasion rule" $ blankRule,
            Rule "carry out requested actions rule" $ blankRule,
            Rule "descend to specific action processing rule" $ 
                abideByRulebook "descend to specific action processing rulebook",
            Rule "end action processing in success rule" (do { return $ Just True})
        ]}

specificActionProcessingRules :: Rulebook ActionData
specificActionProcessingRules = (blankRulebook "descend to specific action processing rulebook") {
    _rules = [
        Rule "check stage rule" $ (do 
            (_, a) <- get
            abideByRulebook ((a ^. actionName) <> " check rulebook")
            ),
        Rule "carry out stage rule" $ (do 
            (_, a) <- get
            sayModifyLn (a ^. actionName)
            abideByRulebook ((a ^. actionName) <> " carry out rulebook")
            )
            ]}
-- ew.
-- type ActionConstructor = (Name, ActionData, [(Name, (World -> (RuleOutcome, World)))])
lookingCarryOutRules :: Rulebook ActionData
lookingCarryOutRules = (blankRulebook "looking carry out rulebook") {
    _rules = [
        Rule "room description heading rule" (do 
            (w, a) <- get
            let actor = (a ^. currentActor $ w) 
                loc = (actor ^. info) ^. location $ w
            sayModifyLn $ (loc ^. name) <> " (current room)"
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