module QCommon.CBufShared 
    ( addText
    , insertText
    ) where

import           Control.Lens     (use, (^.))
import           Control.Monad    (when)
import qualified Data.ByteString  as B

import           QCommon.SizeBufT
import qualified QCommon.SZ       as SZ
import           QuakeState
import           Types

import {-# SOURCE #-} qualified QCommon.Com as Com

insertText :: B.ByteString -> Quake ()
insertText text = do
    cmdText <- use (globals.gCmdText)
    SZ.clear (globals.gCmdText)
    addText text
    when ((cmdText^.sbCurSize) /= 0) (appendLeftoverCommands cmdText)
  where
    appendLeftoverCommands cmdText =
        SZ.write (globals.gCmdText)
                 (B.take (cmdText^.sbCurSize) (cmdText^.sbData))
                 (cmdText^.sbCurSize)

addText :: B.ByteString -> Quake ()
addText text = do
    cmdText <- use (globals.gCmdText)
    if (cmdText^.sbCurSize) + len >= (cmdText^.sbMaxSize)
        then Com.printf "Cbuf.addText: overflow\n"
        else SZ.write (globals.gCmdText) text len
  where
    len = B.length text
