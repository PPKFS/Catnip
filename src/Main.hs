{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
import Objects
import Rulebooks
import Actions
import Common

constructRulebooks :: Map.Map Name (World -> (RuleOutcome, World))
constructRulebooks = Map.fromList [ addRulebook whenPlayBeginsRules, addVRulebook actionProcessingRules,
    addVRulebook specificActionProcessingRules, addVRulebook lookingCarryOutRules]

    where addVRulebook r = (r ^. rulebookName, \w -> runRulebook r (w ^. actionVariables) w)
          addRulebook r = (r ^. rulebookName, runRulebook r ())
--constructActions :: [ActionConstructor]
--constructActions = [makeLookingAction]

constructWorld :: World
constructWorld = World
    {
        _directions = constructDirections,
        _rooms = Map.empty,
        _title = "Untitled Game",
        _rulebooks = constructRulebooks,
        _msgBuffer = "",
        _actionVariables = blankActionData,
        _actions = Map.empty --Map.fromList $ map (\(y, z , _) -> (y, z)) x
    }

main :: IO ()
main = do
    let x = runRulebook whenPlayBeginsRules () constructWorld
    putStr $ T.unpack ((snd x) ^. msgBuffer)