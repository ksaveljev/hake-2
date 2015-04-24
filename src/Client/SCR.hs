{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.SCR where

import Control.Lens ((.=), use, (^.))
import Control.Monad (liftM, when, void)

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sound.S as S
import qualified Sys.Timer as Timer

init :: Quake ()
init = do
    void $ CVar.get "viewsize" "100" Constants.cvarArchive
    void $ CVar.get "scr_conspeed" "3" 0
    void $ CVar.get "scr_showturtle" "0" 0
    void $ CVar.get "scr_showpause" "1" 0
    void $ CVar.get "scr_centertime" "2.5" 0
    void $ CVar.get "scr_printspeed" "8" 0
    void $ CVar.get "netgraph" "1" 0
    void $ CVar.get "timegraph" "1" 0
    void $ CVar.get "debuggraph" "1" 0
    void $ CVar.get "graphheight" "32" 0
    void $ CVar.get "graphscale" "1" 0
    void $ CVar.get "graphshift" "0" 0
    void $ CVar.get "scr_drawall" "1" 0
    void $ CVar.get "fps" "0" 0

    -- register our commands
    Cmd.addCommand "timerefresh" (Just timeRefreshF)
    Cmd.addCommand "loading" (Just loadingF)
    Cmd.addCommand "sizeup" (Just sizeUpF)
    Cmd.addCommand "sizedown" (Just sizeDownF)
    Cmd.addCommand "sky" (Just skyF)

    scrGlobals.scrInitialized .= True

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
    Just renderer <- use $ globals.re
    (renderer^.rRefExport.reUpdateScreen) updateScreenF

{-
- ================== SCR_UpdateScreen
- 
- This is called every frame, and can also be called explicitly to flush
- text to the screen. ==================
-}
updateScreen2 :: Quake ()
updateScreen2 = do
    needToUpdate <- shouldUpdate

    when needToUpdate $ do
      io (putStrLn "SCR.updateScreen2") >> undefined -- TODO

  where shouldUpdate :: Quake Bool
        shouldUpdate = do
          disableScreen <- use $ globals.cls.csDisableScreen
          initialized <- use $ scrGlobals.scrInitialized
          conInitialized <- use $ globals.con.cInitialized

          -- if the screen is disabled (loading plaque is up, or vid mode
          -- changing) do nothing at all
          if | disableScreen /= 0 -> do
                 msec <- Timer.milliseconds
                 when (msec - truncate disableScreen > 120000) $ do
                   globals.cls.csDisableScreen .= 0
                   Com.printf "Loading plaque timed out.\n"
                 return False
               -- not initialized yet
             | (not initialized) || (not conInitialized) -> return False
             | otherwise -> return True

timeRefreshF :: XCommandT
timeRefreshF = io (putStrLn "SCR.timeRefreshF") >> undefined -- TODO

loadingF :: XCommandT
loadingF = io (putStrLn "SCR.loadingF") >> undefined -- TODO

sizeUpF :: XCommandT
sizeUpF = io (putStrLn "SCR.sizeUpF") >> undefined -- TODO

sizeDownF :: XCommandT
sizeDownF = io (putStrLn "SCR.sizeDownF") >> undefined -- TODO

skyF :: XCommandT
skyF = io (putStrLn "SCR.skyF") >> undefined -- TODO
