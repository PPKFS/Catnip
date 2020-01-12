{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Rulebooks where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Objects
import Control.Monad.State
import Control.Monad
import Types
import Utils
import SayCommon
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Maybe
import Data.Foldable

-- | smart constructor for rulebooks. Sets everything to just blank.
blankRulebook :: Name -> Rulebook obj usr act 
blankRulebook n = Rulebook { _rulebookName = n, _firstRules = [], _rules = [], _lastRules = [], _defaultOutcome = Nothing}

-- | a rule that does nothing
blankRule :: WorldRuleState obj usr act
blankRule = return Nothing


runRuleset :: [Rule obj usr a] -> State (World obj usr, a) RuleOutcome
runRuleset = doUntilJustM (\r -> do
    when ( r ^. ruleName /= "") $ sayDbgModifyLnR ("Following the " `mappend` (r ^. ruleName));
    r ^. ruleProcessor)


-- run some rulebook, that is expecting no additional information,
runPlainRulebook :: PlainRulebook obj usr -> State (World obj usr) RuleOutcome
runPlainRulebook r = zoomOut (runRulebook r) ()

runRulebook :: Rulebook obj usr a -> State (World obj usr, a) RuleOutcome
runRulebook r = do
    --print out some debug text if relevant
    zoom _1 (indentDbg True)
    when (r ^. rulebookName /= "") $ sayDbgModifyLnR ("Following the " <> (r ^. rulebookName))
    --run our ruleset
    result <- doUntilJustM runRuleset [r ^. firstRules, r ^. rules, r ^. lastRules]
    return $ if' (isNothing result) (r ^. defaultOutcome) result

runRulebookMaybe :: Maybe (Rulebook obj usr a) -> State (World obj usr, a) RuleOutcome
runRulebookMaybe Nothing = return Nothing
runRulebookMaybe (Just r) = runRulebook r

-- | more constructors of not much
makeActivity :: Name -> [Rule obj usr (ActionData a)] -> Action obj usr a
makeActivity name rules = Action { _actionName = name, _checkRules = Nothing, _reportRules = Nothing,
    _actionInfo = blankActionData, _setActionVariables = Nothing, _carryOutRules = (blankRulebook name) 
    { _rules = rules }}

setActionVars :: (World obj usr, Action obj usr act) -> (World obj usr, Action obj usr act)
setActionVars (w, a) = case setVar of
        Nothing -> (w, a)
        Just b -> (w, a { _actionInfo = b w actionI })
        where setVar = a ^. setActionVariables
              actionI = a ^. actionInfo

-- | the main action processing rulebook. though I guess with haskell's laziness and everything,
-- it's more of a function that doesn't take any parameters?
actionProcessingRules :: Rulebook obj usr (Action obj usr act)
actionProcessingRules = (blankRulebook "action processing rulebook") {
    _firstRules = 
        [
            Rule "set action variables rule" (do
                modify setActionVars
                return Nothing
                ),
            Rule "announce items from multiple object lists rule" (return Nothing),
            Rule "set pronouns from items from multiple object lists rule" blankRule
            --Rule "before stage rule" $ abideByRulebookWithArgs "before stage rulebook" a
        ],
    _rules = 
        [
            Rule "basic visibility rule" blankRule,
            Rule "basic accessibility rule" blankRule,
            Rule "carrying requirements rule" blankRule
        ],
    _lastRules = 
        [
            --Rule "instead stage rule" $ abideByRulebook "instead stage rulebook",
            Rule "requested actions require persuasion rule" blankRule,
            Rule "carry out requested actions rule" blankRule,
            Rule "descend to specific action processing rule" (runRulebook specificActionProcessingRules),
            Rule "end action processing in success rule" (return $ Just True)
        ]}

specificActionProcessingRules :: Rulebook obj usr (Action obj usr act)
specificActionProcessingRules = (blankRulebook "descend to specific action processing rulebook") {
    _rules = [
        Rule "investigate player's awareness before action rule" blankRule,
        Rule "check stage rule" (do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebookMaybe (a ^. checkRules))
            ),
        Rule "carry out stage rule" (do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebook (a ^. carryOutRules))
            ),
        Rule "after stage rule" blankRule,
        Rule "investigate player's awareness after action rule" blankRule,
        Rule "report stage rule" (do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebookMaybe (a ^. checkRules))),
        Rule "end specific action processing rule" (return $ Just True)
            ]}

-- R functions operate within a rulebook; so they have (World, something) sigs
runActivityR :: Action obj usr a -> State (World obj usr, b) RuleOutcome
runActivityR a = zoom _1 (runActivity a)

runActivity :: Action obj usr a -> State (World obj usr) RuleOutcome
runActivity a = do
    zoomOut (do
        modify setActionVars
        (_, a') <- get
        zoom (alongside id actionInfo) (doUntilJustM 
            runRulebookMaybe [a' ^. checkRules, Just $ a' ^. carryOutRules, a' ^. reportRules])) a
    return Nothing

-- | if we begin an activity, we do not do statewise stuff
-- because we instead want to return the activity which complains with the context
-- it's being run in.
beginActivity :: Action obj usr act -> World obj usr -> (RuleOutcome, (World obj usr, Action obj usr act))
beginActivity a w = runState x (w, a) where
    x = undefined --processActionIfExists (a ^. checkRules)

beginActivityIf :: Bool -> Action obj usr act -> World obj usr -> (World obj usr, Action obj usr act)
beginActivityIf cond act w = if cond
    then (let (_, x) = beginActivity act w in x)
    else (w, act)

-- | ending an activity just gives us the World usr obj act , we don't care about the outcome
endActivity :: Action obj usr act -> World obj usr -> World obj usr
endActivity a w = fst $ execState x (w, a) where
    x = undefined -- processActionIfExists (a ^. checkRules)