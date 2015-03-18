{-# LANGUAGE OverloadedStrings #-}
module Client.CL where

import Control.Lens (use, (.=), (^.))
import Control.Monad (unless, liftM)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
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
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar

    unless (dedicatedValue /= 0) $ do
      Console.init >> S.init >> VID.init >> V.init

      bufData <- use $ globals.netMessageBuffer
      globals.netMessage.sbData .= bufData
      globals.netMessage.sbMaxSize .= B.length bufData

      Menu.init >> SCR.init >> initLocal >> IN.init

      FS.execAutoexec
      CBuf.execute

initLocal :: Quake ()
initLocal = undefined -- TODO

writeConfiguration :: Quake ()
writeConfiguration = undefined -- TODO
