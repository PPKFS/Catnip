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

-- | so this is the internals of the say method; applies whatever is said to the end
-- of the buffer of standard input and annotates if needed.
sayInt :: Doc AnsiStyle ->  World obj usr ->  World obj usr
sayInt a w = w & msgBuffer . stdBuffer %~ ((:) $ apl a)
                where apl = case (w ^. msgBuffer . msgStyle) of
                        Nothing -> id
                        Just x -> annotate x

say :: T.Text ->  World obj usr ->  World obj usr 
say a w = sayInt (pretty a) w

sayLn :: T.Text ->  World obj usr ->  World obj usr 
sayLn a = sayInt (pretty a <> line)

sayDbgInt :: Doc AnsiStyle ->  World obj usr ->  World obj usr 
sayDbgInt a w = w & msgBuffer . dbgBuffer %~ ((:) $ (pretty (T.replicate (w ^. msgBuffer . indentLvl) " ") <> a))
                where apl = case (w ^. msgBuffer . msgStyle) of
                                Nothing -> id
                                Just x -> annotate x

                                --data Capitalisation = Capital | NotCapital
                                --data Definitive = Definite | Indefinite
                                --data PrintingOptions = PrintingOptions Capitalisation Definitive
sayDbg :: T.Text ->  World obj usr ->  World obj usr
sayDbg a w = sayDbgInt (pretty a) w

sayDbgLn :: T.Text ->  World obj usr ->  World obj usr
sayDbgLn a = sayDbgInt (pretty a <> line)

indentDbg ::  World obj usr -> Bool ->  World obj usr
indentDbg w b = w & msgBuffer . indentLvl %~ ((+) $ (if b then 1 else (-1)) * 4)-- w2 & msgBuffer . stdBuffer %~ ((:) $ (w ^. msgBuffer . indentLvl, line)) where

setStyle :: (Maybe AnsiStyle) ->  World obj usr ->  World obj usr
setStyle s w= w & msgBuffer . msgStyle .~ s
-- | same as say, but prebaked to save having to modify sayLn 
sayModify :: T.Text -> WorldUpdate obj usr  b ()
sayModify a = modifyWorld (say a)

sayModifyLn :: T.Text -> WorldUpdate obj usr b ()
sayModifyLn a = modifyWorld (sayLn a)

sayModifyFormatted :: Doc AnsiStyle -> WorldUpdate obj usr b ()
sayModifyFormatted a = modifyWorld (sayInt a)

sayModifyLnFormatted :: Doc AnsiStyle -> WorldUpdate obj usr b ()
sayModifyLnFormatted a = modifyWorld (sayInt $ a <> line)