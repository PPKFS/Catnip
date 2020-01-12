{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module SayCommon where

import Types
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Control.Lens
import Control.Monad.State
import Utils
import System.IO


defaultStyle :: NameStyle
defaultStyle = NameStyle Capitalised Definite

-- | so this is the internals of the say method; applies whatever is said to the end
-- of the buffer of standard input and annotates if needed.
sayInt :: Doc AnsiStyle -> World ->  World
sayInt a w = w & msgBuffer . stdBuffer %~ ((:) $ apl a)
                where apl = case w ^. msgBuffer . msgStyle of
                        Nothing -> id
                        Just x -> annotate x

say :: T.Text ->  World ->  World 
say a = sayInt (pretty a)

sayLn :: T.Text ->  World ->  World 
sayLn a = sayInt (pretty a <> line)

sayDbgInt :: Doc AnsiStyle ->  World ->  World 
sayDbgInt a w = w & msgBuffer . dbgBuffer %~ (:) (pretty (T.replicate (w ^. msgBuffer . indentLvl) " ") <> a)
                where apl = case w ^. msgBuffer . msgStyle of
                                Nothing -> id
                                Just x -> annotate x

                                --data Capitalisation = Capital | NotCapital
                                --data Definitive = Definite | Indefinite
                                --data PrintingOptions = PrintingOptions Capitalisation Definitive
sayDbg :: T.Text ->  World ->  World 
sayDbg a = sayDbgInt (pretty a)

sayDbgLn :: T.Text ->  World ->  World 
sayDbgLn a = sayDbgInt (pretty a <> line)

sayDbgModifyLn :: T.Text -> State (World) ()
sayDbgModifyLn a = modify (sayDbgLn a)

sayDbgModifyLnR :: T.Text -> WorldUpdate b ()
sayDbgModifyLnR a = modifyR (sayDbgLn a)

indentDbg :: Bool -> State (World ) ()
indentDbg b = msgBuffer . indentLvl %= (+) ((if b then 1 else (-1)) * 4)

indentDbgR :: Bool -> WorldUpdate b ()
indentDbgR b = zoom _1 (indentDbg b)


setStyle :: Maybe AnsiStyle ->  World ->  World 
setStyle s w = w & msgBuffer . msgStyle .~ s
-- | same as say, but prebaked to save having to modify sayLn 
sayModify :: T.Text -> WorldUpdate b ()
sayModify a = modifyR(say a)

sayModifyLnR :: T.Text -> WorldUpdate b ()
sayModifyLnR a = modifyR (sayLn a)

sayModifyLn :: T.Text -> State (World ) ()
sayModifyLn a = modify (sayLn a)

sayModifyFormatted :: Doc AnsiStyle -> WorldUpdate b ()
sayModifyFormatted a = modifyR (sayInt a)

sayModifyLnFormatted :: Doc AnsiStyle -> WorldUpdate b ()
sayModifyLnFormatted a = modifyR (sayInt $ a <> line)