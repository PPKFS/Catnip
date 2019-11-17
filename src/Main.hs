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
import System.IO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

constructRulebooks :: Map.Map Name (World -> (RuleOutcome, World))
constructRulebooks = Map.fromList [ addRulebook whenPlayBeginsRules]
        --addRulebook actionProcessingRules,
        --addRulebook specificActionProcessingRules, addRulebook lookingCarryOutRules]
    where 
        addRulebook r = (r ^. rulebookName, runRulebook r)
--constructActions :: [ActionConstructor]
--constructActions = [makeLookingAction]

constructWorld :: World
constructWorld = World
    {
        _directions = constructDirections,
        _rooms = Map.empty,
        _title = "Untitled Goose Game",
        _rulebooks = constructRulebooks,
        _msgBuffer = MsgBuffer { _indentLvl = 0, _stdBuffer = [], _dbgBuffer = [], _msgStyle = Nothing }
        --_actions = Map.empty --Map.fromList $ map (\(y, z , _) -> (y, z)) x
    }

main :: IO ()
main = do
    let (_, x) = runRulebook whenPlayBeginsRules constructWorld
    putDoc $ fillCat $ reverse $ (x ^. msgBuffer . dbgBuffer)
    putDoc $ fillCat $ reverse $ (x ^. msgBuffer . stdBuffer)
    putStrLn $ ""