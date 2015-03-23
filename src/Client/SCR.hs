module Client.SCR where

import Control.Lens ((.=), use, (^.))
import Control.Monad (liftM, when)

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Sound.S as S
import qualified Sys.Timer as Timer

init :: Quake ()
init = undefined -- TODO

beginLoadingPlaque :: Quake ()
beginLoadingPlaque = do
    S.stopAllSounds
    globals.cl.csSoundPrepped .= False

    clientStatic <- use $ globals.cls
    developerValue <- liftM (^.cvValue) developerCVar

    when ((clientStatic^.csDisableScreen) == 0 &&
          developerValue == 0 &&
          (clientStatic^.csState) /= Constants.caDisconnected &&
          (clientStatic^.csKeyDest) /= Constants.keyConsole) $ do
      
      cinematicTime <- use $ globals.cl.csCinematicTime
      if cinematicTime > 0
        then scrGlobals.scrDrawLoading .= 2
        else scrGlobals.scrDrawLoading .= 1

      updateScreen

      msec <- Timer.milliseconds
      serverCount <- use $ globals.cl.csServerCount

      globals.cls.csDisableScreen .= fromIntegral msec
      globals.cls.csDisableServerCount .= serverCount

endLoadingPlaque :: Quake ()
endLoadingPlaque = io (putStrLn "SCR.endLoadingPlaque") >> undefined -- TODO

updateScreenF :: XCommandT
updateScreenF = updateScreen2

updateScreen :: Quake ()
updateScreen = do
    renderer <- use $ globals.re
    (renderer^.reUpdateScreen) updateScreenF

updateScreen2 :: Quake ()
updateScreen2 = io (putStrLn "SCR.updateScreen2") >> undefined -- TODO
