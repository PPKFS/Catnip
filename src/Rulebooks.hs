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

-- | smart constructor for rulebooks. Sets everything to just blank.
blankRulebook :: Name -> Rulebook obj usr act 
blankRulebook n = Rulebook { _rulebookName = n, _firstRules = [], _rules = [], _lastRules = [], _defaultOutcome = Nothing}

-- | a rule that does nothing
blankRule :: WorldRuleState obj usr act
blankRule = return Nothing

-- | internal use function ; run a list of rules
runRuleset :: [Rule obj usr a] -> a -> World obj usr -> (RuleOutcome, (World obj usr, a))
runRuleset [] a w = (Nothing, (w, a))
runRuleset (r:rs) a w = case x of
    (Just True, _) -> x
    (Nothing, (w', a')) -> runRuleset rs a' w'
    where x = runState y (w, a)
          y = if' ( r ^. ruleName /= "")
            printFollowing
            (r ^. ruleProcessor) where
                printFollowing = do 
                            modifyWorld $ sayDbgLn ("Following the " `mappend` (r ^. ruleName))
                            r ^. ruleProcessor

-- run some rulebook, that is expecting no additional information,
runRulebook :: PlainRulebook obj usr -> World obj usr -> (Maybe Bool, World obj usr)
runRulebook r w = (\(f, (w', _)) -> (f, w')) $ runRulebookInternally r () w

-- | this is the meat of the rulebook code
-- we take a rulebook, which we initialise with some data a
-- we then iterate all 3 of its rulebooks, doing stuff if needbe
-- if we hit a value other than nothing, we return and stop
runRulebookInternally :: Rulebook obj usr a -> a -> World obj usr -> (Maybe Bool, (World obj usr, a))
runRulebookInternally r x w = (res, (w2', a')) where
        (res, (w', a')) = iterateRulebooks [r ^. firstRules, r ^. rules, r ^. lastRules] a w2
        w1 = indentDbg w True
        a = x
        w2 = if' ( r ^. rulebookName /= "") 
            (sayDbgLn ("Following the " <> (r ^. rulebookName)) w1)
            w1
        w2' = indentDbg w' False
        iterateRulebooks :: [[Rule obj usr act]] -> act -> World obj usr -> (RuleOutcome, (World obj usr, act))
        iterateRulebooks [] a w = (r ^. defaultOutcome, (w, a))
        iterateRulebooks (x:xs) a w = case ro of
            (Just _, _) -> ro
            (Nothing, (w', a')) -> iterateRulebooks xs a' w'
            where ro = runRuleset x a w



-- | more constructors of not much
makeActivity :: Name -> [Rule obj usr (ActionData a)] -> Action obj usr a
makeActivity name rules = Action { _actionName = name, _checkRules = Nothing, _reportRules = Nothing,
    _actionInfo = blankActionData, _setActionVariables = Nothing, _carryOutRules = (blankRulebook name) 
    { _rules = rules }}

setActionVars :: World obj usr -> Action obj usr act-> Action obj usr act
setActionVars w a = case setVar of
                    Nothing -> a
                    Just b -> a { _actionInfo = b w actionI } 
                    where actionI = a ^. actionInfo
                          setVar = a ^. setActionVariables

-- | the main action processing rulebook. though I guess with haskell's laziness and everything,
-- it's more of a function that doesn't take any parameters?
actionProcessingRules :: Rulebook obj usr (Action obj usr act)
actionProcessingRules = (blankRulebook "action processing rulebook") {
    _firstRules = 
        [
            Rule "set action variables rule" (do
                (w, a) <- get
                put (w, setActionVars w a)
                return Nothing
                ),

            Rule "announce items from multiple object lists rule" (do
                (w, a) <- get
                return Nothing),
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
            Rule "descend to specific action processing rule"
            (state $ abideByRulebookWithArgs specificActionProcessingRules id const),
            Rule "end action processing in success rule" (return $ Just True)
        ]}

specificActionProcessingRules :: Rulebook obj usr (Action obj usr act)
specificActionProcessingRules = (blankRulebook "descend to specific action processing rulebook") {
    _rules = [
        Rule "investigate player's awareness before action rule" blankRule,
        Rule "check stage rule" (do 
            (_, a) <- get 
            let x = a ^. checkRules
            case x of 
                Nothing -> return Nothing
                Just action -> processAction action)
            ,
        Rule "carry out stage rule" (do 
            (_, a) <- get
            processAction (a ^. carryOutRules) 
            ),
        Rule "after stage rule" blankRule,
        Rule "investigate player's awareness after action rule" blankRule,
        Rule "report stage rule" (do 
            (_, a) <- get
            let x = a ^. reportRules
            case x of
                Nothing -> return Nothing
                Just action -> processAction action),
        Rule "end specific action processing rule" (return $ Just True)
            ]}

-- | this is a sort of internal run rulebook
-- take some rulebook that takes inner as its processing handmedown
-- take a way to extract an internal part of an external context
-- take a way to reapply the internals to the external
-- take a world and that outer context
-- run it all
-- normally, this is extracting actionvars from an action.
abideByRulebookWithArgs :: Rulebook obj usr inner -> (outer -> inner) -> (inner -> outer -> outer)
    -> (World obj usr, outer) -> (RuleOutcome, (World obj usr, outer))
abideByRulebookWithArgs rulebook convertIn convertOut (world, actionVars) = (result2, 
    (world2, convertOut action2 actionVars)) where
        (result2, (world2, action2)) = runRulebookInternally rulebook (convertIn actionVars) world

-- | rulebook from actiondata (e.g. a check rulebook) that can be run in an Action context.
-- so all we do to make it work is take the action info from the inner context
-- and move it back to the outer one
-- i.e. we have an Action a with actiondata AD. a contains rulebooks and things. these run on AD.
-- so what we do is run the rulebook on AD and get out AD', which we then set AD = AD'.
processAction :: Rulebook obj usr (ActionData a) -> 
    State (World obj usr, Action obj usr a) RuleOutcome
processAction r = state $ abideByRulebookWithArgs r (view actionInfo) (\a b -> b { _actionInfo = a})

processActionIfExists :: Maybe (Rulebook obj usr (ActionData a)) 
        -> State (World obj usr, Action obj usr a) RuleOutcome
processActionIfExists = maybe (return Nothing) processAction

-- | this looks identical to processAction but the difference is that...this unwraps the state processing?
runActionIfExists2 :: Maybe (Rulebook obj usr (ActionData a)) 
        -> (World obj usr, ActionData a) -> (RuleOutcome, (World obj usr, ActionData a))
runActionIfExists2 Nothing a = (Nothing, a)
runActionIfExists2 (Just r) s = abideByRulebookWithArgs r id (\a b -> b) s

runActivity :: Action obj usr a -> a -> State (World obj usr, b) RuleOutcome
runActivity act noun = do
    (w, b) <- get
    let a = setActionVars w (act & actionInfo . actionVariables .~ noun)
    let (out1, (w', a')) = runState (processActionIfExists (a ^. checkRules)) (w, a)
    case out1 of
        Just x -> do { put (w', b); return $ Just x }
        Nothing -> case out2 of
                Just x -> do { put (w'', b); return $ Just x }
                Nothing -> do {  put (w''', b); return out3 }
                        where (out3, (w''', a''')) = runState (processActionIfExists (a ^. reportRules)) (w'', a'')
            where (out2, (w'', a'')) = runState (processAction (a ^. carryOutRules)) (w', a')

runActivityIf :: Bool -> Action obj usr a -> a -> State (World obj usr, b) RuleOutcome
runActivityIf c a b = if c then runActivity a b else return Nothing

-- | run an activity and ignore whatever the output is, only caring about the world state
-- | e.g. printing stuff
runActivityPlainly :: Action obj usr a -> a -> State (World obj usr, b) RuleOutcome
runActivityPlainly a args = do 
                        runActivity a args
                        return Nothing

-- | if we begin an activity, we do not do statewise stuff
-- because we instead want to return the activity which complains with the context
-- it's being run in.
beginActivity :: Action obj usr act -> World obj usr -> (RuleOutcome, (World obj usr, Action obj usr act))
beginActivity a w = runState x (w, a) where
    x = processActionIfExists (a ^. checkRules)

beginActivityIf :: Bool -> Action obj usr act -> World obj usr -> (World obj usr, Action obj usr act)
beginActivityIf cond act w = if cond
    then (let (_, x) = beginActivity act w in x)
    else (w, act)

-- | ending an activity just gives us the World usr obj act , we don't care about the outcome
endActivity :: Action obj usr act -> World obj usr -> World obj usr
endActivity a w = fst $ execState x (w, a) where
    x = processActionIfExists (a ^. checkRules)