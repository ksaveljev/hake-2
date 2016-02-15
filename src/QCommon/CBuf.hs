module QCommon.CBuf
  (addEarlyCommands
  ,addLateCommands
  ,addText
  ,execute
  ,initialize)
  where

import qualified QCommon.Com as Com
import qualified QCommon.SZ as SZ
import           QuakeState
import           Types

import           Control.Lens (use, (^.))
import qualified Data.ByteString as B

addEarlyCommands :: Bool -> Quake ()
addEarlyCommands = error "CBuf.addEarlyCommands" -- TODO

addLateCommands :: Quake Bool
addLateCommands = error "CBuf.addLateCommands" -- TODO

addText :: B.ByteString -> Quake ()
addText text =
  do cmdText <- use (globals.gCmdText)
     if (cmdText^.sbCurSize) + len >= (cmdText^.sbMaxSize)
       then Com.printf "Cbuf.addText: overflow\n"
       else SZ.write (globals.gCmdText) text len
  where len = B.length text

execute :: Quake ()
execute = error "CBuf.execute" -- TODO

initialize :: Quake ()
initialize = SZ.initialize (globals.gCmdText) "" 8192