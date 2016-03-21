{-# LANGUAGE FlexibleContexts #-}
module Client.SCR
  ( beginLoadingPlaque
  , endLoadingPlaque
  , finishCinematic
  , initialize
  , runCinematic
  , runConsole
  , updateScreen
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInv as CLInv
import qualified Client.Console as Console
import           Client.ConsoleT
import           Client.FrameT
import qualified Client.Menu as Menu
import           Client.RefExportT
import qualified Client.V as V
import           Client.VidDefT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import           Game.PlayerStateT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.MSG as MSG
import           QCommon.NetChanT
import qualified QCommon.SZ as SZ
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)

import           Control.Lens (preuse, use, ix, (^.), (.=), _1, _2)
import           Control.Monad (when, void)
import           Data.Bits ((.&.))
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
updateScreen2 =
  do needToUpdate <- shouldUpdate
     when needToUpdate $
       do checkStereoSeparation =<< clStereoSeparationCVar
          (numFrames, separation) <- checkStereo =<< clStereoCVar
          renderer <- use (globals.gRenderer)
          maybe rendererError (doUpdateScreen2 separation numFrames) renderer
  where rendererError = error "SCR.updateScreen2 renderer is Nothing"
        doUpdateScreen2 separation numFrames renderer =
          do mapM_ (runFrames renderer separation) [0..numFrames-1]
             (renderer^.rRefExport.reEndFrame)

shouldUpdate :: Quake Bool
shouldUpdate =
  do disableScreen <- use (globals.gCls.csDisableScreen)
     initialized <- use (scrGlobals.scrInitialized)
     conInitialized <- use (globals.gCon.cInitialized)
     checkIfUpdateIsNeeded disableScreen initialized conInitialized
  where checkIfUpdateIsNeeded disableScreen initialized conInitialized
          | disableScreen /= 0 =
              do msec <- Timer.milliseconds
                 when (msec - truncate disableScreen > 120000) $
                   do globals.gCls.csDisableScreen .= 0
                      Com.printf "Loading plaque timed out.\n"
                 return False
          | (not initialized) || (not conInitialized) = return False
          | otherwise = return True

checkStereoSeparation :: CVarT -> Quake ()
checkStereoSeparation stereoSeparation
  | (stereoSeparation^.cvValue) > 1 =
      void (CVar.setValueF "cl_stereo_separation" 1.0)
  | (stereoSeparation^.cvValue) < 0 =
      void (CVar.setValueF "cl_stereo_separation" 0.0)
  | otherwise = return ()

checkStereo :: CVarT -> Quake (Int, (Float, Float))
checkStereo stereo
  | (stereo^.cvValue) /= 0 =
      do stereoSeparation <- clStereoSeparationCVar
         return (2, ((stereoSeparation^.cvValue) / (-2), (stereoSeparation^.cvValue) / 2))
  | otherwise = return (1, (0, 0))

runFrames :: Renderer -> (Float, Float) -> Int -> Quake ()
runFrames renderer separation idx =
  do (renderer^.rRefExport.reBeginFrame) separationValue
     scrDrawLoading' <- use (scrGlobals.scrDrawLoading)
     cinematicTime <- use (globals.gCl.csCinematicTime)
     cinematicPaletteActive <- use (globals.gCl.csCinematicPaletteActive)
     doRunFrames scrDrawLoading' cinematicTime cinematicPaletteActive
  where separationValue | idx == 0 = separation^._1
                        | otherwise = separation^._2
        doRunFrames scrDrawLoading' cinematicTime cinematicPaletteActive
          | scrDrawLoading' == 2 =
              do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                 scrGlobals.scrDrawLoading .= 0
                 vidDef <- use (globals.gVidDef)
                 dim <- (renderer^.rRefExport.reDrawGetPicSize) "loading"
                 maybe dimError (drawPic vidDef) dim
          | cinematicTime > 0 =
              do keyDest <- use (globals.gCls.csKeyDest)
                 checkKeyDest keyDest cinematicPaletteActive
          | otherwise =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 calcVrect
                 tileClear
                 V.renderView separationValue
                 drawStats
                 checkStatLayout =<< preuse (globals.gCl.csFrame.fPlayerState.psStats.ix Constants.statLayouts)
                 drawNet
                 checkDrawCenterString
                 drawFPS
                 drawPause
                 drawConsole
                 Menu.draw
                 drawLoading
        checkStatLayout Nothing = Com.fatalError "SCR.runFrames checkStatLayout has Nothing"
        checkStatLayout (Just statLayout) =
          do when (statLayout .&. 1 /= 0) drawLayout
             when (statLayout .&. 2 /= 0) CLInv.drawInventory
        dimError = Com.fatalError "SCR.runFrames reDrawGetPicSize returned Nothing"
        drawPic vidDef (width, height) =
          (renderer^.rRefExport.reDrawPic) (((vidDef^.vdWidth) - width) `div` 2) (((vidDef^.vdHeight) - height) `div` 2) "loading"
        checkKeyDest keyDest cinematicPaletteActive
          | keyDest == Constants.keyMenu =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 Menu.draw
          | keyDest == Constants.keyConsole =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 drawConsole
          | otherwise = void drawCinematic

runCinematic :: Quake ()
runCinematic = error "SCR.runCinematic" -- TODO

runConsole :: Quake ()
runConsole = error "SCR.runConsole" -- TODO

finishCinematic :: Quake ()
finishCinematic =
  do MSG.writeByteI (globals.gCls.csNetChan.ncMessage) Constants.clcStringCmd
     serverCount <- use (globals.gCl.csServerCount)
     SZ.printSB (globals.gCls.csNetChan.ncMessage) (B.concat ["nextserver ", encode serverCount, "\n"])

calcVrect :: Quake ()
calcVrect = error "SCR.calcVrect" -- TODO

tileClear :: Quake ()
tileClear = error "SCR.tileClear" -- TODO

drawStats :: Quake ()
drawStats = error "SCR.drawStats" -- TODO

drawLayout :: Quake ()
drawLayout = error "SCR.drawLayout" -- TODO

drawNet :: Quake ()
drawNet = error "SCR.drawNet" -- TODO

checkDrawCenterString :: Quake ()
checkDrawCenterString = error "SCR.checkDrawCenterString" -- TODO

drawFPS :: Quake ()
drawFPS = error "SCR.drawFPS" -- TODO

drawPause :: Quake ()
drawPause = error "SCR.drawPause" -- TODO

drawConsole :: Quake ()
drawConsole = error "SCR.drawConsole" -- TODO

drawCinematic :: Quake Bool
drawCinematic = error "SCR.drawCinematic" -- TODO

drawLoading :: Quake ()
drawLoading = error "SCR.drawLoading" -- TODO