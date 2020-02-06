{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Rulebooks where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Objects
import           SayCommon
import           Types
import           Utils

-- | smart constructor for rulebooks. Sets everything to just blank.
blankRulebook :: Name -> Rulebook w act 
blankRulebook n = Rulebook { 
    _rulebookName = n, 
    _firstRules = [], 
    _rules = [], 
    _lastRules = [], 
    _defaultOutcome = Nothing }

-- | a rule that does nothing
blankRule :: r -> WorldUpdate w a RuleOutcome
blankRule _ = return Nothing

-- | run rules until you get some kind of output
runRuleset :: (HasWorld w, HasWorldRules r w) => [Rule w a] -> r -> WorldUpdate w a RuleOutcome
runRuleset rules ruleset = doUntilJustM (\r -> do
    when (r ^. ruleName /= "") $ sayDbgModifyLn ("Following the " `mappend` (r ^. ruleName))
    processRule r) rules
    where processRule r = unwrapRule (r ^. doRule) ruleset

-- run some rulebook, that is expecting no additional information,
runPlainRulebook :: (HasWorld w, HasWorldRules r w) => PlainRulebook w -> r -> State w RuleOutcome
runPlainRulebook r ruleset = zoomOut (runRulebook r ruleset) ()

runRulebook :: (HasWorld w, HasWorldRules r w) => Rulebook w a -> r -> WorldUpdate w a RuleOutcome
runRulebook r ruleset = do
    --print out some debug text if relevant
    zoom _1 (indentDbg True)
    when (r ^. rulebookName /= "") $ sayDbgModifyLn ("Following the " <> (r ^. rulebookName))
    --run our ruleset
    result <- doUntilJustM (`runRuleset` ruleset) [r ^. firstRules, r ^. rules, r ^. lastRules]
    return $ if' (isNothing result) (r ^. defaultOutcome) result

runRulebookMaybe :: (HasWorld w, HasWorldRules r w) => Maybe (Rulebook w a) -> r -> WorldUpdate w a RuleOutcome
runRulebookMaybe Nothing _ = return Nothing
runRulebookMaybe (Just r) ruleset = runRulebook r ruleset


setActionVars :: HasWorld w => (w , Action w act) -> (w, Action w act)
setActionVars (w, a) = case setVar of
        Nothing -> (w, a)
        Just b -> (w, a { _actionInfo = b w actionI })
        where setVar = a ^. setActionVariables
              actionI = a ^. actionInfo

-- | the main action processing rulebook. though I guess with haskell's laziness and everything,
-- it's more of a function that doesn't take any parameters?
actionProcessingRules :: HasWorld w => Rulebook w (Action w act)
actionProcessingRules = (blankRulebook "action processing rulebook") {
    _firstRules = 
        [
            makeRule "set action variables rule" (\_ -> do
                modify setActionVars
                return Nothing
                ),
            makeRule "announce items from multiple object lists rule" blankRule,
            makeRule "set pronouns from items from multiple object lists rule" blankRule
            --Rule "before stage rule" $ abideByRulebookWithArgs "before stage rulebook" a
        ],
    _rules = 
        [
            makeRule "basic visibility rule" blankRule,
            makeRule "basic accessibility rule" blankRule,
            makeRule "carrying requirements rule" blankRule
        ],
    _lastRules = 
        [
            --Rule "instead stage rule" $ abideByRulebook "instead stage rulebook",
            makeRule "requested actions require persuasion rule" blankRule,
            makeRule "carry out requested actions rule" blankRule,
            makeRule "descend to specific action processing rule" (runRulebook specificActionProcessingRules),
            makeRule "end action processing in success rule" (\_ -> return $ Just True)
        ]}

specificActionProcessingRules :: HasWorld w => Rulebook w (Action w act)
specificActionProcessingRules = (blankRulebook "descend to specific action processing rulebook") {
    _rules = [
        makeRule "investigate player's awareness before action rule" blankRule,
        makeRule "check stage rule" (\r -> do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebookMaybe (a ^. checkRules) r)
            ),
        makeRule "carry out stage rule" (\r -> do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebook (a ^. carryOutRules) r)
            ),
        makeRule "after stage rule" blankRule,
        makeRule "investigate player's awareness after action rule" blankRule,
        makeRule "report stage rule" (\r -> do 
            (_, a) <- get
            zoom (alongside id actionInfo) (runRulebookMaybe (a ^. checkRules) r)),
        makeRule "end specific action processing rule" blankRule
            ]}

runActivity :: (HasWorld w, HasWorldRules r w) => Action w a -> r -> State w RuleOutcome
runActivity a r = do
    zoomOut (do 
        modify setActionVars
        (_, a') <- get
        zoom (alongside id actionInfo) (doUntilJustM (`runRulebookMaybe` r) 
            [a' ^. checkRules, Just $ a' ^. carryOutRules, a' ^. reportRules])) a
    return Nothing

makeActivity :: Name -> [Rule w (ActionData a)] -> Action w a
makeActivity name rules = Action { _actionName = name, _checkRules = Nothing, _reportRules = Nothing,
    _actionInfo = undefined, _setActionVariables = Nothing, _carryOutRules = (blankRulebook name) 
    { _rules = rules }}