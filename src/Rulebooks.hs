{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Rulebooks where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Objects
import Control.Monad.State
import Control.Monad
import Common
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- | a version of modify that just ignores any rulebook variables
modifyWorld :: (a -> a) -> State (a, b) ()
modifyWorld f = modify (\(w, a) -> (f w, a))

instance Show (Rule a) where
    show r = T.unpack $ r ^. ruleName

instance Show (Rulebook a) where
    show r = T.unpack $ r ^. rulebookName

sayInt :: Doc AnsiStyle -> World -> World
sayInt a w = w & msgBuffer . stdBuffer %~ ((:) $ apl a)
                where apl = case (w ^. msgBuffer . msgStyle) of
                        Nothing -> id
                        Just x -> annotate x

say :: T.Text -> World -> World
say a w = sayInt (pretty a) w

sayLn :: T.Text -> World -> World
sayLn a = sayInt (pretty a <> line)

sayDbgInt :: Doc AnsiStyle -> World -> World
sayDbgInt a w = w & msgBuffer . dbgBuffer %~ ((:) $ (pretty (T.replicate (w ^. msgBuffer . indentLvl) " ") <> a))
                where apl = case (w ^. msgBuffer . msgStyle) of
                                Nothing -> id
                                Just x -> annotate x

sayDbg :: T.Text -> World -> World
sayDbg a w = sayDbgInt (pretty a) w

sayDbgLn :: T.Text -> World -> World
sayDbgLn a = sayDbgInt (pretty a <> line)

indentDbg :: World -> Bool -> World
indentDbg w b = w & msgBuffer . indentLvl %~ ((+) $ (if b then 1 else (-1)) * 4)-- w2 & msgBuffer . stdBuffer %~ ((:) $ (w ^. msgBuffer . indentLvl, line)) where

setStyle :: (Maybe AnsiStyle) -> World -> World
setStyle s w= w & msgBuffer . msgStyle .~ s
-- | same as say, but prebaked to save having to modify sayLn 
sayModify :: T.Text -> WorldUpdate b ()
sayModify a = modifyWorld (say a)

sayModifyLn :: T.Text -> WorldUpdate b ()
sayModifyLn a = modifyWorld (sayLn a)

sayModifyFormatted :: Doc AnsiStyle -> WorldUpdate b ()
sayModifyFormatted a = modifyWorld (sayInt a)

sayModifyLnFormatted :: Doc AnsiStyle -> WorldUpdate b ()
sayModifyLnFormatted a = modifyWorld (sayInt $ a <> line)

blankRulebook :: Name -> Rulebook a
blankRulebook n = Rulebook { _rulebookName = n, _firstRules = [], _rules = [], _lastRules = [], _defaultOutcome = Nothing}

type PlainRulebook = Rulebook ()
plainRulebook :: Name -> PlainRulebook
plainRulebook n = blankRulebook n

blankRule :: WorldRuleState a
blankRule = do { return Nothing }

runRuleset :: [Rule a] -> a -> World -> (RuleOutcome, (World, a))
runRuleset [] a w = (Nothing, (w, a))
runRuleset (r:rs) a w = case x of
    (Just True, _) -> x
    (Nothing, (w', a')) -> runRuleset rs a' w'
    where x = runState y (w, a)
          y = (do { modifyWorld $ sayDbgLn ("Following the " `mappend` (r ^. ruleName)) ; (r ^. rule) })

runRulebook :: PlainRulebook -> World -> (Maybe Bool, World)
runRulebook r w = (\(f, (w', _)) -> (f, w')) $ runRulebookInternally r () w

-- | so this is a pain to have to keep coming back to
-- we take a rulebook, some initial conditions, and a world
-- we then try each ruleset in turn until we get some answer
runRulebookInternally :: Rulebook a -> a -> World -> (Maybe Bool, (World, a))
runRulebookInternally r x w = (res, (w2', a')) where
        (res, (w', a')) = iterateRulebooks [r ^. firstRules, r ^. rules, r ^. lastRules] a w2
        w1 = indentDbg w True
        a = x
        w2 = sayDbgLn ("Following the " <> (r ^. rulebookName)) w1
        w2' = indentDbg (w') False
        iterateRulebooks :: [[Rule a]] -> a -> World -> (RuleOutcome, (World, a))
        iterateRulebooks [] a w = (r ^. defaultOutcome, (w, a))
        iterateRulebooks (x:xs) a w = case ro of
            (Just _, _) -> ro
            (Nothing, (w', a')) -> iterateRulebooks xs a' w'
            where ro = runRuleset x a w
-- | call a rulebook as a rule


-- | to abide by a rulebook and act as glue for its various action parameters 
-- this covers obvious things like current actors but also action variables to be passed
-- between rulebooks.
-- Rulebook -> (some way to convert the input argument) -> (some way to append 
-- our new mutated argument to the input argument) -> state)
abideByRulebookWithArgs :: Rulebook a -> (b -> a) -> (a -> b -> b) -> (World, b) -> (RuleOutcome, (World, b))
abideByRulebookWithArgs  rulebook convertIn convertOut (world, actionVars) = (result2, 
    (world2, convertOut action2 actionVars)) where
        (result2, (world2, action2)) = (runRulebookInternally rulebook (convertIn actionVars) world)

abideByRulebook :: T.Text -> WorldRuleState a
abideByRulebook r = do { state $ abideWrap r }

abideWrap :: Name -> (World, a) -> (RuleOutcome, (World, a))
abideWrap r (w, a) = (x, (w', a)) where
    (x, w') = ((getRulebook w r) w)

getRulebook :: World -> Name -> (World -> (RuleOutcome, World))
getRulebook w n = case x of
    Nothing -> runRulebook (plainRulebook "dummy rulebook")
    Just r -> r
    where x = Map.lookup n (w ^. rulebooks)

introText :: World -> World
introText = execState (do
    w <- get
    let shortBorder = "------" :: T.Text
        totalLength = (2 * (T.length shortBorder)) + T.length (w ^. title) + 2 :: Int
        longBorder = T.replicate totalLength "-" :: T.Text
    modify $ (setStyle (Just (color Green <> bold)))
    modify $ sayLn longBorder
    w <- get
    modify $ sayLn (shortBorder <> " " <> (w ^. title) <> " " <> shortBorder)
    modify $ sayLn longBorder
    modify $ sayLn "\n"
    modify $ setStyle Nothing
    return w)

blankActionData = ActionData { _currentActor = \w -> pen }

-- | An action covers both inform actions and inform activities. I cannot work out what the difference really
-- is from the documentation, except that an action can be explicitly called upon by the player.
-- so for me, the only real difference is going to be parser-level stuff which I can wrap up
-- in a nice maybe. Like we have parameterised over rules and rulebooks and objects, we parameterise actions
-- to allow for action variables.

type PlainAction = Action ()
makeActivity :: Name -> [Rule (ActionData ())] -> PlainAction
makeActivity name rules = Action { _actionName = name, _checkRules = Nothing, _reportRules = Nothing,
_actionData = blankActionData, _setActionVariables = Nothing, _carryOutRules = (blankRulebook "") { _rules = rules }}

-- | the main action processing rulebook. It requires some kind of input
-- to know what form of action is being processed right now.
actionProcessingRules :: Rulebook (Action a)
actionProcessingRules = (blankRulebook "action processing rulebook") {
    _firstRules = 
        [
            --Rule "set action variables rule" $  (do {modify setActionVariables }),
            Rule "announce items from multiple object lists rule" $ (do
                (w, a) <- get
                return Nothing),
            Rule "set pronouns from items from multiple object lists rule" $ blankRule
            --Rule "before stage rule" $ abideByRulebookWithArgs "before stage rulebook" a
        ],
    _rules = 
        [
            Rule "basic visibility rule" blankRule {-$ (do
                (w', a') <- get
                -- LEAVING THIS HERE JUST FOR CLARITY
                let (result2, (world2, action2)) = runRulebookInternally (_chRules a') (Just (_actionVariables a')) w'
                    a'' = a' { _actionVariables = action2 }
                put (world2, a'')
                return result2
            )-},
            Rule "basic accessibility rule" $ blankRule,
            Rule "carrying requirements rule" $ blankRule
        ],
    _lastRules = 
        [
            Rule "instead stage rule" $ abideByRulebook "instead stage rulebook",
            Rule "requested actions require persuasion rule" $ blankRule,
            Rule "carry out requested actions rule" $ blankRule,
            Rule "descend to specific action processing rule" $ (do
                -- todo: find if this can be made neater
                state $ abideByRulebookWithArgs specificActionProcessingRules id (\a b -> a)
                ),
            Rule "end action processing in success rule" (do { return $ Just True})
        ]}

-- | rulebook from actiondata (e.g. a check rulebook) that can be run in an Action context.
processActionAbide :: Rulebook (ActionData a) -> State (World, Action a) RuleOutcome
processActionAbide r = state $ abideByRulebookWithArgs r (\b -> b ^. actionData) (\a b -> b { _actionData = a})

processActionAbideIfExists :: Maybe (Rulebook (ActionData a)) -> State (World, Action a) RuleOutcome
processActionAbideIfExists Nothing = return Nothing
processActionAbideIfExists (Just r) = processActionAbide r

-- | rulebook from actiondata (e.g. a check rulebook) that can be run in an actiondata context.
-- the difference is that runaction works entirely in a actiondata context.
runActionIfExists :: Maybe (Rulebook (ActionData a)) -> (World, ActionData a) -> (RuleOutcome, (World, ActionData a))
runActionIfExists Nothing a = (Nothing, a)
runActionIfExists (Just r) s = abideByRulebookWithArgs r id (\a b -> b) s

runActivity :: Action a -> State (World, b) RuleOutcome
runActivity a = do
    (w, b) <- get
    let (out1, (w', a')) = runState (processActionAbideIfExists (a ^. checkRules)) (w, a)
    case out1 of
        Just x -> do { put (w', b); return $ Just x }
        Nothing -> case out2 of
                Just x -> do { put (w'', b); return $ Just x }
                Nothing -> do {  put (w''', b); return out3 }
                        where (out3, (w''', a''')) = runState (processActionAbideIfExists (a ^. reportRules)) (w'', a'')
            where (out2, (w'', a'')) = runState (processActionAbide (a ^. carryOutRules)) (w', a')

runActivityIf :: Bool -> Action a -> State (World, b) RuleOutcome
runActivityIf c a = do if c then runActivity a else return Nothing

-- | if we begin an activity, we do not do statewise stuff
-- because we instead want to return the activity which complains with the context
-- it's being run in.
beginActivity :: Action a -> World -> (RuleOutcome, (World, Action a))
beginActivity a w = runState x (w, a) where
    x = (processActionAbideIfExists (a ^. checkRules))

beginActivityIf :: Bool -> Action a -> World  -> (World, Action a)
beginActivityIf cond act w = if cond
    then (let (_, x) = beginActivity act w in x)
    else (w, act)

specificActionProcessingRules :: Rulebook (Action a)
specificActionProcessingRules = (blankRulebook "descend to specific action processing rulebook") {
    _rules = [
        Rule "investigate player's awareness before action rule" blankRule,
        Rule "check stage rule" $ (do 
            (_, a) <- get
            let x = (a ^. checkRules) 
            case x of
                Nothing -> return Nothing
                Just action -> processActionAbide action
            ),
        Rule "carry out stage rule" $ (do 
            (_, a) <- get
            processActionAbide (a ^. carryOutRules) 
            ),
        Rule "after stage rule" blankRule,
        Rule "investigate player's awareness after action rule" blankRule,
        Rule "report stage rule" $ (do 
            (_, a) <- get
            let x = (a ^. reportRules) 
            case x of
                Nothing -> return Nothing
                Just action -> processActionAbide action
            ),
        Rule "end specific action processing rule" (do { return $ Just True })
            ]}