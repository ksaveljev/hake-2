{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.SCR where

import Control.Lens ((.=), use, (^.), _1, _2, ix, preuse)
import Control.Monad (liftM, when, void)
import Data.Bits ((.&.))

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLInv as CLInv
import qualified Client.Menu as Menu
import qualified Client.V as V
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
      -- range check cl_camera_separation so we don't inadvertently fry
      -- someone's brain
      checkStereoSeparation
      (numFrames, separation) <- checkStereo

      runFrames separation 0 numFrames

      Just renderer <- use $ globals.re
      renderer^.rRefExport.reEndFrame

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

        checkStereoSeparation :: Quake ()
        checkStereoSeparation = do
          stereoSeparationValue <- liftM (^.cvValue) clStereoSeparationCVar
          if | stereoSeparationValue > 1 ->
                 void $ CVar.setValueF "cl_stereo_separation" 1.0
             | stereoSeparationValue < 0 ->
                 void $ CVar.setValueF "cl_stereo_separation" 0.0
             | otherwise -> return ()

        checkStereo :: Quake (Int, (Float, Float))
        checkStereo = do
          stereoValue <- liftM (^.cvValue) clStereoCVar
          if stereoValue /= 0
            then do
              stereoSeparationValue <- liftM (^.cvValue) clStereoSeparationCVar
              return (2, (stereoSeparationValue / (-2), (stereoSeparationValue / 2)))
            else
              return (1, (0, 0))

        runFrames :: (Float, Float) -> Int -> Int -> Quake ()
        runFrames separation idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let access = if idx == 0 then _1 else _2

              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reBeginFrame) (separation^.access)

              scrDrawLoading' <- use $ scrGlobals.scrDrawLoading
              cinematicTime <- use $ globals.cl.csCinematicTime
              cinematicPaletteActive <- use $ globals.cl.csCinematicPaletteActive

                   -- loading plaque over black screen
              if | scrDrawLoading' == 2 -> do
                     (renderer^.rRefExport.reCinematicSetPalette) Nothing
                     scrGlobals.scrDrawLoading .= 0 -- false
                     Just (width, height) <- (renderer^.rRefExport.reDrawGetPicSize) "loading"

                     vidDef' <- use $ globals.vidDef
                     (renderer^.rRefExport.reDrawPic) (((vidDef'^.vdWidth) - width) `div` 2) (((vidDef'^.vdHeight) - height) `div` 2) "loading"

                   -- if a cinematic is supposed to be running, handle
                   -- menus and console specially
                 | cinematicTime > 0 -> do
                     keyDest <- use $ globals.cls.csKeyDest

                     if | keyDest == Constants.keyMenu -> do
                            when cinematicPaletteActive $ do
                              (renderer^.rRefExport.reCinematicSetPalette) Nothing
                              globals.cl.csCinematicPaletteActive .= False

                            Menu.draw

                        | keyDest == Constants.keyConsole -> do
                            when cinematicPaletteActive $ do
                              (renderer^.rRefExport.reCinematicSetPalette) Nothing
                              globals.cl.csCinematicPaletteActive .= False

                            drawConsole

                        | otherwise -> do
                            -- TODO: implement cinematics completely
                            drawCinematic

                 | otherwise -> do
                     -- make sure the game palette is active
                     when cinematicPaletteActive $ do
                       (renderer^.rRefExport.reCinematicSetPalette) Nothing
                       globals.cl.csCinematicPaletteActive .= False

                     -- do 3D refresh drawing, and then update the screen
                     calcVrect

                     -- clear any dirty part of the background
                     tileClear

                     V.renderView (if idx == 0 then separation^._1 else separation^._2)

                     drawStats

                     Just statLayout <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statLayouts
                     when (statLayout .&. 1 /= 0) $
                       drawLayout
                     when (statLayout .&. 2 /= 0) $
                       CLInv.drawInventory

                     drawNet
                     checkDrawCenterString
                     drawFPS

                     drawPause
                     drawConsole
                     Menu.draw
                     drawLoading

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

runCinematic :: Quake ()
runCinematic = io (putStrLn "SCR.runCinematic") >> undefined -- TODO

finishCinematic :: Quake ()
finishCinematic = io (putStrLn "SCR.finishCinematic") >> undefined -- TODO

runConsole :: Quake ()
runConsole = io (putStrLn "SCR.runConsole") >> undefined -- TODO

calcVrect :: Quake ()
calcVrect = do
    io (putStrLn "SCR.calcVrect") >> undefined -- TODO

tileClear :: Quake ()
tileClear = do
    io (putStrLn "SCR.tileClear") >> undefined -- TODO

drawStats :: Quake ()
drawStats = do
    io (putStrLn "SCR.drawStats") >> undefined -- TODO

drawLayout :: Quake ()
drawLayout = do
    io (putStrLn "SCR.drawLayout") >> undefined -- TODO

drawNet :: Quake ()
drawNet = do
    io (putStrLn "SCR.drawNet") >> undefined -- TODO

checkDrawCenterString :: Quake ()
checkDrawCenterString = do
    io (putStrLn "SCR.checkDrawCenterString") >> undefined -- TODO

drawFPS :: Quake ()
drawFPS = do
    io (putStrLn "SCR.drawFPS") >> undefined -- TODO

drawPause :: Quake ()
drawPause = do
    io (putStrLn "SCR.drawPause") >> undefined -- TODO

drawConsole :: Quake ()
drawConsole = do
    io (putStrLn "SCR.drawConsole") >> undefined -- TODO

drawCinematic :: Quake ()
drawCinematic = do
    io (putStrLn "SCR.drawCinematic") >> undefined -- TODO

drawLoading :: Quake ()
drawLoading = do
    io (putStrLn "SCR.drawLoading") >> undefined -- TODO
