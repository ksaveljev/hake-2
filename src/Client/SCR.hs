{-# LANGUAGE FlexibleContexts #-}
module Client.SCR
  ( beginLoadingPlaque
  , endLoadingPlaque
  , initialize
  , runCinematic
  , runConsole
  , updateScreen
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.Console as Console
import           Client.RefExportT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import           Types

import           Control.Lens (use, (^.), (.=))
import qualified Data.ByteString as B

beginLoadingPlaque :: Quake ()
beginLoadingPlaque =
  do S.stopAllSounds
     globals.gCl.csSoundPrepped .= False
     clientStatic <- use (globals.gCls)
     developer <- developerCVar
     beginWhenReady clientStatic developer

beginWhenReady :: ClientStaticT -> CVarT -> Quake ()
beginWhenReady clientStatic developer
  | ready =
      do checkCinematicTime =<< use (globals.gCl.csCinematicTime)
         updateScreen
         setDisableScreen =<< Timer.milliseconds
         setDisableServerCount =<< use (globals.gCl.csServerCount)
  | otherwise = return ()
  where ready = (clientStatic^.csDisableScreen) == 0 &&
                (developer^.cvValue) == 0 &&
                (clientStatic^.csState) /= Constants.caDisconnected &&
                (clientStatic^.csKeyDest) /= Constants.keyConsole
        checkCinematicTime cinematicTime
          | cinematicTime > 0 = scrGlobals.scrDrawLoading .= 2
          | otherwise = scrGlobals.scrDrawLoading .= 1
        setDisableScreen msec = globals.gCls.csDisableScreen .= fromIntegral msec
        setDisableServerCount serverCount = globals.gCls.csDisableServerCount .= serverCount

endLoadingPlaque :: Quake ()
endLoadingPlaque =
  do globals.gCls.csDisableScreen .= 0
     Console.clearNotify

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("viewsize", "100", Constants.cvarArchive), ("scr_conspeed", "3", 0)
  , ("scr_showturtle", "0", 0), ("scr_showpause", "1", 0)
  , ("scr_centertime", "2.5", 0), ("scr_printspeed", "8", 0)
  , ("netgraph", "1", 0), ("timegraph", "1", 0), ("debuggraph", "1", 0)
  , ("graphheight", "32", 0), ("graphscale", "1", 0), ("graphshift", "0", 0)
  , ("scr_drawall", "1", 0), ("fps", "0", 0)
  ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("timerefresh", Just timeRefreshF), ("loading", Just loadingF)
  , ("sizeup", Just sizeUpF), ("sizedown", Just sizeDownF), ("sky", Just skyF)
  ]

initialize :: Quake ()
initialize =
  do CVar.initializeCVars initialCVars
     Cmd.addInitialCommands initialCommands
     scrGlobals.scrInitialized .= True

timeRefreshF :: XCommandT
timeRefreshF = error "SCR.timeRefreshF" -- TODO

loadingF :: XCommandT
loadingF = XCommandT "SCR.loadingF" beginLoadingPlaque

sizeUpF :: XCommandT
sizeUpF = XCommandT "SCR.sizeUpF" sizeUp

sizeUp :: Quake ()
sizeUp =
  do viewSize <- viewSizeCVar
     CVar.setValueF "viewsize" ((viewSize^.cvValue) + 10)

sizeDownF :: XCommandT
sizeDownF = XCommandT "SCR.sizeDownF" sizeDown

sizeDown :: Quake ()
sizeDown =
  do viewSize <- viewSizeCVar
     CVar.setValueF "viewsize" ((viewSize^.cvValue) - 10)

skyF :: XCommandT
skyF = error "SCR.skyF" -- TODO

updateScreen :: Quake ()
updateScreen =
  do renderer <- use (globals.gRenderer)
     maybe rendererError doUpdateScreen renderer
  where rendererError = error "SCR.updateScreen renderer is Nothing"
        doUpdateScreen renderer = (renderer^.rRefExport.reUpdateScreen) updateScreenF

updateScreenF :: XCommandT
updateScreenF = XCommandT "SCR.updateScreenF" updateScreen2

updateScreen2 :: Quake ()
updateScreen2 = error "SCR.updateScreen2" -- TODO

runCinematic :: Quake ()
runCinematic = error "SCR.runCinematic" -- TODO

runConsole :: Quake ()
runConsole = error "SCR.runConsole" -- TODO