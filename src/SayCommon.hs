{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}

module SayCommon where

import           Control.Lens
import           Control.Monad.State
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.IO
import           Types
import           Utils


defaultStyle :: NameStyle
defaultStyle = NameStyle Capitalised Definite

-- | so this is the internals of the say method; applies whatever is said to the end
-- of the buffer of standard input and annotates if needed.
sayInternal :: (HasWorld x) => Doc AnsiStyle -> x -> x
sayInternal a w = w & world . msgBuffer . stdBuffer %~ (:) (apl a)
                where apl = maybe id annotate (w ^. msgBuffer . msgStyle)

say :: (HasWorld x) => T.Text -> x -> x
say a = sayInternal (pretty a)

sayLn :: (HasWorld x) => T.Text -> x -> x
sayLn a = sayInternal (pretty a <> line)

sayDbgInt :: (HasWorld x) => Doc AnsiStyle -> x -> x
sayDbgInt a w = w & world . msgBuffer . dbgBuffer %~ (:) (pretty (T.replicate (w ^. msgBuffer . indentLvl) " ") <> a)
                where apl = maybe id annotate (w ^. msgBuffer . msgStyle)
sayDbg :: (HasWorld x) => T.Text -> x -> x
sayDbg a = sayDbgInt (pretty a)

sayDbgLn ::  (HasWorld x) => T.Text -> x -> x
sayDbgLn a = sayDbgInt (pretty a <> line)

sayDbgModifyLn :: (HasWorld x) => T.Text -> State x ()
sayDbgModifyLn a = modify (sayDbgLn a)

indentDbg :: (HasWorld x) => Bool -> State x ()
indentDbg b = msgBuffer . indentLvl %= (+) ((if b then 1 else (-1)) * 4)

setStyle :: (HasWorld x) => Maybe AnsiStyle -> x -> x
setStyle s w = w & world . msgBuffer . msgStyle .~ s
-- | same as say, but prebaked to save having to modify sayLn 
sayModify :: (HasWorld x) => T.Text -> State x ()
sayModify a = modify (say a)

sayModifyLn :: (HasWorld x) => T.Text -> State x ()
sayModifyLn a = modify (sayLn a)

sayModifyFormatted :: (HasWorld x) => Doc AnsiStyle -> State x ()
sayModifyFormatted a = modify (sayInternal a)

sayModifyLnFormatted :: (HasWorld x) =>  Doc AnsiStyle -> State x ()
sayModifyLnFormatted a = modify (sayInternal $ a <> line)