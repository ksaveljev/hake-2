module Client.CL
  ( initialize
  , writeConfiguration
  ) where

import qualified Client.Console as Console
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import qualified Constants
import qualified QCommon.CBuf as CBuf
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import qualified QCommon.SZ as SZ
import           QuakeState
import qualified Sound.S as S
import qualified Sys.IN as IN
import           Types

import           Control.Lens ((^.))
import           Control.Monad (unless)

initialize :: Quake ()
initialize =
  do inDedicatedMode <- fmap ((/= 0) . (^.cvValue)) dedicatedCVar
     unless inDedicatedMode $
       do Console.initialize
          S.initialize
          VID.initialize
          V.initialize
          SZ.initialize (globals.gNetMessage) "" Constants.maxMsgLen
          Menu.initialize
          SCR.initialize
          initLocal
          IN.initialize
          FS.execAutoexec
          CBuf.execute

writeConfiguration :: Quake ()
writeConfiguration = error "CL.writeConfiguration" -- TODO

initLocal :: Quake ()
initLocal = error "CL.initLocal" -- TODO
