module Client.CL where

import Control.Lens (use)

import Quake
import QuakeState
import qualified Client.Console as Console
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified QCommon.CBuf as CBuf
import qualified QCommon.FS as FS

init :: Quake ()
init = do
    dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

    if dedicatedValue /= 0
      then return ()
      else do
        Console.init >> S.init >> VID.init >> V.init

        {- TODO:
        Globals.net_message.data = Globals.net_message_buffer;
        Globals.net_message.maxsize = Globals.net_message_buffer.length;
        -}

        Menu.init >> SCR.init >> initLocal >> IN.init

        FS.execAutoexec
        CBuf.execute

initLocal :: Quake ()
initLocal = undefined -- TODO

writeConfiguration :: Quake ()
writeConfiguration = undefined -- TODO
