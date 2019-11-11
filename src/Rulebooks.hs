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
import Common

-- | a version of modify that just ignores any rulebook variables
modifyWorld :: (a -> a) -> State (a, b) ()
modifyWorld f = modify (\(w, a) -> (f w, a))

instance Show (Rule a) where
    show r = T.unpack $ r ^. ruleName

instance Show (Rulebook a) where
    show r = T.unpack $ r ^. rulebookName

say :: T.Text -> World -> World
say a w = w & msgBuffer %~ (flip T.append) a

sayLn :: T.Text -> World -> World
sayLn a = say (a <> "\n")

-- | same as say, but prebaked to save having to modify sayLn 
sayModify :: T.Text -> WorldUpdate b ()
sayModify a = modifyWorld (say a)

sayModifyLn :: T.Text -> WorldUpdate b ()
sayModifyLn a = modifyWorld (sayLn a)

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
          y = (do { modifyWorld $ sayLn ("Following the " `mappend` (r ^. ruleName)) ; (r ^. rule) })

-- | This should never need to be explicitly called, but instead partially applied when updating rulebooks
runRulebook :: Rulebook a -> a -> World -> (Maybe Bool, World)
runRulebook r a w = (res, w') where
        (res, (w', _)) = iterateRulebooks [r ^. firstRules, r ^. rules, r ^. lastRules] a w2
        w2 = sayLn ("Following the " <> (r ^. rulebookName)) w
        iterateRulebooks :: [[Rule a]] -> a -> World -> (RuleOutcome, (World, a))
        iterateRulebooks [] a w = (r ^. defaultOutcome, (w, a))
        iterateRulebooks (x:xs) a w = case ro of
            (Just _, _) -> ro
            (Nothing, (w', a')) -> iterateRulebooks xs a' w'
            where ro = runRuleset x a w
-- | call a rulebook as a rule
abideByRulebook :: T.Text -> WorldRuleState a
abideByRulebook r = do { state $ abideWrap r }

abideWrap :: Name -> (World, a) -> (RuleOutcome, (World, a))
abideWrap r (w, a) = (x, (w', a)) where
    (x, w') = ((getRulebook w r) w)

getRulebook :: World -> Name -> (World -> (RuleOutcome, World))
getRulebook w n = case x of
    Nothing -> runRulebook (blankRulebook "dummy rulebook") ()
    Just r -> r
    where x = Map.lookup n (w ^. rulebooks)