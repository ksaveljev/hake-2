{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.SCR where

import Control.Lens ((.=), use, (^.), _1, _2, ix, preuse, zoom, (-=), (+=))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.&.), complement, shiftR)
import Data.Char (ord)
import Data.Maybe (isNothing, isJust)
import Data.Monoid (mempty, (<>))
import Linear (V3(..), _y)
import System.IO (Handle, hGetPosn, hSeek, hTell, SeekMode(AbsoluteSeek))
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import Util.Binary
import qualified Constants
import qualified Client.CLInv as CLInv
import {-# SOURCE #-} qualified Client.Console as Console
import {-# SOURCE #-} qualified Client.Menu as Menu
import {-# SOURCE #-} qualified Client.V as V
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

statLayouts :: Int
statLayouts = 13

statMinus :: Int
statMinus = 10 -- num frame for '-' stats digit

iconWidth :: Int
iconWidth = 24

iconHeight :: Int
iconHeight = 24

charWidth :: Int
charWidth = 16

iconSpace :: Int
iconSpace = 8

sbNums1 :: V.Vector B.ByteString
sbNums1 =
    V.fromList [ "num_0" , "num_1" , "num_2"
               , "num_3" , "num_4" , "num_5"
               , "num_6" , "num_7" , "num_8"
               , "num_9" , "num_minus"
               ]

sbNums2 :: V.Vector B.ByteString
sbNums2 =
    V.fromList [ "anum_0" , "anum_1" , "anum_2"
               , "anum_3" , "anum_4" , "anum_5"
               , "anum_6" , "anum_7" , "anum_8"
               , "anum_9" , "anum_minus"
               ]

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
endLoadingPlaque = do
    globals.cls.csDisableScreen .= 0
    Console.clearNotify

updateScreenF :: XCommandT
updateScreenF =
  XCommandT "SCR.updateScreenF" updateScreen2

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
                            void $ drawCinematic

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
timeRefreshF =
  XCommandT "SCR.timeRefreshF" (do
    state <- use $ globals.cls.csState

    when (state == Constants.caActive) $ do
      start <- Timer.milliseconds

      c <- Cmd.argc
      Just renderer <- use $ globals.re

      if c == 2
        then do -- run without page flipping
          (renderer^.rRefExport.reBeginFrame) 0

          mapM_ (\i -> do
                        globals.cl.csRefDef.rdViewAngles._y .= i / 128.0 * 360.0
                        use (globals.cl.csRefDef) >>= (renderer^.rRefExport.reRenderFrame) 
                ) [0..127]

          renderer^.rRefExport.reEndFrame

        else do
          mapM_ (\i -> do
                        globals.cl.csRefDef.rdViewAngles._y .= i / 128.0 * 360.0
                        (renderer^.rRefExport.reBeginFrame) 0
                        use (globals.cl.csRefDef) >>= (renderer^.rRefExport.reRenderFrame) 
                        renderer^.rRefExport.reEndFrame
                ) [0..127]

      stop <- Timer.milliseconds
      let time = fromIntegral (stop - start) / 1000

      Com.printf (BC.pack (show time) `B.append` " seconds (" `B.append` BC.pack (show (128.0 / time)) `B.append` " fps)\n") -- IMPROVE?
  )

loadingF :: XCommandT
loadingF = XCommandT "SCR.loadingF" beginLoadingPlaque

sizeUpF :: XCommandT
sizeUpF =
  XCommandT "SCR.sizeUpF" (do
    v <- liftM (^.cvValue) viewSizeCVar
    CVar.setValueF "viewsize" (v + 10)
  )

sizeDownF :: XCommandT
sizeDownF =
  XCommandT "SCR.sizeDownF" (do
    v <- liftM (^.cvValue) viewSizeCVar
    CVar.setValueF "viewsize" (v - 10)
  )

{-
- ================= SCR_Sky_f
- 
- Set a specific sky and rotation speed =================
-}
skyF :: XCommandT
skyF =
  XCommandT "SCR.skyF" (do
    c <- Cmd.argc

    if c < 2
      then
        Com.printf "Usage: sky <basename> <rotate> <axis x y z>\n"
      else do
        rotate <- if c > 2
                    then do
                      v <- Cmd.argv 2
                      return (Lib.atof v)
                    else
                      return 0

        axis <- if c == 6
                  then do
                    a <- Cmd.argv 3
                    b <- Cmd.argv 4
                    c <- Cmd.argv 5
                    return (V3 (Lib.atof a) (Lib.atof b) (Lib.atof c))
                  else
                    return (V3 0 0 1)

        v <- Cmd.argv 1
        Just renderer <- use $ globals.re
        (renderer^.rRefExport.reSetSky) v rotate axis
  )

runCinematic :: Quake ()
runCinematic = do
    cl' <- use $ globals.cl
    cls' <- use $ globals.cls

    if | cl'^.csCinematicTime <= 0 -> stopCinematic
       | cl'^.csCinematicFrame == -1 -> return () -- static image
       | cls'^.csKeyDest /= Constants.keyGame -> globals.cl.csCinematicTime .= (cls'^.csRealTime) - (cl'^.csCinematicFrame) * 1000 `div` 14 -- pause if menu or console is up
       | otherwise -> do
           let frameF :: Float = fromIntegral (cls'^.csRealTime) - fromIntegral (cl'^.csCinematicTime) * 14 / 1000
               frame :: Int = truncate frameF

           unless (frame <= (cl'^.csCinematicFrame)) $ do
             when (frame > (cl'^.csCinematicFrame) + 1) $ do
               Com.println $ "Dropped frame: " `B.append` BC.pack (show frame) `B.append` -- IMPROVE ?
                             " > " `B.append` BC.pack (show $ (cl'^.csCinematicTime) + 1)  -- IMPROVE ?
               globals.cl.csCinematicTime .= (cls'^.csRealTime) - (cl'^.csCinematicFrame) * 1000 `div` 14

             use (scrGlobals.scrCin.cPicPending) >>= \v ->
               scrGlobals.scrCin.cPic .= v

             nextFrame <- readNextFrame
             scrGlobals.scrCin.cPicPending .= nextFrame

             when (isNothing nextFrame) $ do
               stopCinematic
               finishCinematic
               -- hack to get the black screen behind loading
               globals.cl.csCinematicTime .= 1
               beginLoadingPlaque
               globals.cl.csCinematicTime .= 0

stopCinematic :: Quake ()
stopCinematic = do
    restartSound <- use $ scrGlobals.scrCin.cRestartSound

    when restartSound $ do
      -- done
      globals.cl.csCinematicTime .= 0
      scrGlobals.scrCin.cPic .= Nothing
      scrGlobals.scrCin.cPicPending .= Nothing

      use (globals.cl.csCinematicPaletteActive) >>= \active ->
        when active $ do
          Just renderer <- use $ globals.re
          (renderer^.rRefExport.reCinematicSetPalette) Nothing
          globals.cl.csCinematicPaletteActive .= False

      use (globals.cl.csCinematicFile) >>= \f ->
        when (isJust f) $
          globals.cl.csCinematicFile .= Nothing

      use (scrGlobals.scrCin.cHNodes1) >>= \v ->
        when (isJust v) $
          scrGlobals.scrCin.cHNodes1 .= Nothing

      S.disableStreaming
      scrGlobals.scrCin.cRestartSound .= False

{-
- FinishCinematic
- 
- Called when either the cinematic completes, or it is aborted
-}
finishCinematic :: Quake ()
finishCinematic = do
    -- tell the server to advance to the next map / cinematic
    MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
    serverCount <- use $ globals.cl.csServerCount
    SZ.print (globals.cls.csNetChan.ncMessage) ("nextserver " `B.append` BC.pack (show serverCount) `B.append` "\n") -- IMPROVE?

runConsole :: Quake ()
runConsole = do
    -- decide one the height of the console
    keyDest <- use $ globals.cls.csKeyDest

    let conLines = if keyDest == Constants.keyConsole
                     then 0.5 -- half screen
                     else 0 -- none visible

    scrGlobals.scrConLines .= conLines
    conCurrent <- use $ scrGlobals.scrConCurrent
    frameTime <- use $ globals.cls.csFrameTime
    conSpeedValue <- liftM (^.cvValue) scrConSpeedCVar

    if | conLines < conCurrent -> do
           let v = conCurrent - conSpeedValue * frameTime
           scrGlobals.scrConCurrent .= v
           when (conLines > v) $
             scrGlobals.scrConCurrent .= conLines
       | conLines > conCurrent -> do
           let v = conCurrent + conSpeedValue * frameTime
           scrGlobals.scrConCurrent .= v
           when (conLines < v) $
             scrGlobals.scrConCurrent .= conLines
       | otherwise -> return ()

{-
- ================= SCR_CalcVrect =================
- 
- Sets scr_vrect, the coordinates of the rendered window
-}
calcVrect :: Quake ()
calcVrect = do
    -- bound viewsize
    viewSizeCVar >>= \viewSize -> do
      when ((viewSize^.cvValue) < 40) $
        void $ CVar.set "viewsize" "40"
      when ((viewSize^.cvValue) > 100) $
        void $ CVar.set "viewsize" "100"

    size :: Int <- liftM (truncate . (^.cvValue)) viewSizeCVar
    vidDef' <- use $ globals.vidDef

    let w = ((vidDef'^.vdWidth) * size `div` 100) .&. (complement 7)
        h = ((vidDef'^.vdHeight) * size `div` 100) .&. (complement 1)

    zoom (globals.scrVRect) $ do
      vrWidth .= w
      vrHeight .= h
      vrX .= ((vidDef'^.vdWidth) - w) `div` 2
      vrY .= ((vidDef'^.vdHeight) - h) `div` 2

{-
- ============== SCR_TileClear
- 
- Clear any parts of the tiled background that were drawn on last frame
- ==============
-}
tileClear :: Quake ()
tileClear = do
    drawAllValue <- liftM (^.cvValue) scrDrawAllCVar

    when (drawAllValue /= 0) $
      dirtyScreen -- for power vr or broken page filppers...

    scrConCurrent' <- use $ scrGlobals.scrConCurrent
    viewSizeValue <- liftM (^.cvValue) viewSizeCVar
    cinematicTime <- use $ globals.cl.csCinematicTime
        -- full screen console | full screen rendering | full screen cinematic
    unless (scrConCurrent' == 1 || viewSizeValue == 100 || cinematicTime > 0) $ do
      -- erase rect will be the union of the past three frames
      -- so tripple buffering works properly
      scrDirty' <- use $ scrGlobals.scrDirty
      scrOldDirty' <- use $ scrGlobals.scrOldDirty
      let tmpClear = calcClear scrDirty' scrOldDirty' 0 2

      scrGlobals.scrOldDirty .= (scrDirty', scrOldDirty'^._1)

      zoom (scrGlobals.scrDirty) $ do
        x1 .= 9999
        x2 .= (-9999)
        y1 .= 9999
        y2 .= (-9999)

      -- don't bother with anything covered by the console
      vidDef' <- use $ globals.vidDef
      let tmp :: Int = truncate $ scrConCurrent' * fromIntegral (vidDef'^.vdHeight)
          clear = if tmp >= (tmpClear^.y1)
                    then tmpClear { _y1 = tmp }
                    else tmpClear

      -- nothing disturbed
      unless ((clear^.y2) <= (clear^.y1)) $ do
        vrect <- use $ globals.scrVRect
        let top = vrect^.vrY
            bottom = top + (vrect^.vrHeight) - 1
            left = vrect^.vrX
            right = left + (vrect^.vrWidth) - 1

        void $ clearAbove top clear
                 >>= clearBelow bottom
                 >>= clearLeft left
                 >>= clearRight right

  where calcClear :: DirtyT -> (DirtyT, DirtyT) -> Int -> Int -> DirtyT
        calcClear clear oldDirty idx maxIdx
          | idx >= maxIdx = clear
          | otherwise =
              let access = if idx == 0 then _1 else _2
                  xx1 = if (oldDirty^.access.x1) < (clear^.x1) then oldDirty^.access.x1 else clear^.x1
                  xx2 = if (oldDirty^.access.x2) > (clear^.x2) then oldDirty^.access.x2 else clear^.x2
                  yy1 = if (oldDirty^.access.y1) < (clear^.y1) then oldDirty^.access.y1 else clear^.y1
                  yy2 = if (oldDirty^.access.y2) > (clear^.y2) then oldDirty^.access.y2 else clear^.y2
                  newClear = DirtyT { _x1 = xx1, _x2 = xx2, _y1 = yy1, _y2 = yy2 }
              in calcClear newClear oldDirty (idx + 1) maxIdx

        clearAbove :: Int -> DirtyT -> Quake DirtyT
        clearAbove top clear = do
          if (clear^.y1) < top -- clear above view screen
            then do
              let i = if (clear^.y2) < (top - 1) then clear^.y2 else top - 1
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawTileClear) (clear^.x1) (clear^.y1) ((clear^.x2) - (clear^.x1) + 1) (i - (clear^.y1) + 1) "backtile"
              return $ clear { _y1 = top }
            else
              return clear

        clearBelow :: Int -> DirtyT -> Quake DirtyT
        clearBelow bottom clear = do
          if (clear^.y2) > bottom -- clear below view screen
            then do
              let i = if (clear^.y1) > bottom + 1 then clear^.y1 else bottom + 1
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawTileClear) (clear^.x1) i ((clear^.x2) - (clear^.x1) + 1) ((clear^.y2) - i + 1) "backtile"
              return $ clear { _y2 = bottom }
            else
              return clear

        clearLeft :: Int -> DirtyT -> Quake DirtyT
        clearLeft left clear = do
          if (clear^.x1) < left -- clear left of view screen
            then do
              let i = if (clear^.x2) < left - 1 then clear^.x2 else left - 1
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawTileClear) (clear^.x1) (clear^.y1) (i - (clear^.x1) + 1) ((clear^.y2) - (clear^.y1) + 1) "backtile"
              return $ clear { _x1 = left }
            else
              return clear

        clearRight :: Int -> DirtyT -> Quake DirtyT
        clearRight right clear = do
          if (clear^.x2) > right -- clear right of view screen
            then do
              let i = if (clear^.x1) > right + 1 then clear^.x1 else right + 1
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawTileClear) i (clear^.y1) ((clear^.x2) - i + 1) ((clear^.y2) - (clear^.y1) + 1) "backtile"
              return $ clear { _x2 = right }
            else
              return clear

{-
- ================ SCR_DrawStats
- 
- The status bar is a small layout program that is based on the stats array
- ================
-}
drawStats :: Quake ()
drawStats = do
    Just str <- preuse $ globals.cl.csConfigStrings.ix Constants.csStatusBar
    executeLayoutString str

drawLayout :: Quake ()
drawLayout = do
    Just v <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix statLayouts
    when (v /= 0) $ do
      layout <- use $ globals.cl.csLayout
      executeLayoutString layout

drawNet :: Quake ()
drawNet = do
    nc <- use $ globals.cls.csNetChan
    unless ((nc^.ncOutgoingSequence) - (nc^.ncIncomingAcknowledged) < Constants.cmdBackup - 1) $ do
      vrect <- use $ globals.scrVRect
      Just renderer <- use $ globals.re
      (renderer^.rRefExport.reDrawPic) ((vrect^.vrX) + 64) (vrect^.vrY) "net"

checkDrawCenterString :: Quake ()
checkDrawCenterString = do
    frameTime <- use $ globals.cls.csFrameTime
    scrGlobals.scrCenterTimeOff -= frameTime

    use (scrGlobals.scrCenterTimeOff) >>= \v ->
      unless (v <= 0)
        drawCenterString

drawFPS :: Quake ()
drawFPS = do
    fps <- fpsCVar

    if | (fps^.cvValue) > 0 -> do
           when (fps^.cvModified) $ do
             CVar.update fps { _cvModified = False }
             CVar.setValueI "cl_maxfps" 1000

           realTime <- use $ globals.cls.csRealTime
           lastTime <- use $ scrGlobals.scrLastTime
           let diff = realTime - lastTime

           when (diff > truncate ((fps^.cvValue) * 1000)) $ do
             frameCount <- use $ globals.cls.csFrameCount
             lastFrames <- use $ scrGlobals.scrLastFrames
             let fpsvalue :: Float = fromIntegral (frameCount - lastFrames) * 100000 / fromIntegral diff / 100.0
                 fpsStr = BC.pack (show fpsvalue) `B.append` " fps"

             scrGlobals.scrLastFrames .= frameCount
             scrGlobals.scrLastTime .= realTime
             scrGlobals.scrFPSValue .= fpsStr

           vidDef' <- use $ globals.vidDef
           fpsValue <- use $ scrGlobals.scrFPSValue
           let x = (vidDef'^.vdWidth) - 8 * (B.length fpsValue) - 2
           drawFPSByChar x 0 fpsValue

       | fps^.cvModified -> do
           CVar.update fps { _cvModified = False }
           CVar.setValueI "cl_maxfps" 90

       | otherwise -> return ()

  where drawFPSByChar :: Int -> Int -> B.ByteString -> Quake ()
        drawFPSByChar x idx str
          | idx >= B.length str = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) x 2 (ord $ BC.index str idx)
              drawFPSByChar (x + 8) (idx + 1) str

drawPause :: Quake ()
drawPause = do
    showPauseValue <- liftM (^.cvValue) scrShowPauseCVar
    pausedValue <- liftM (^.cvValue) clPausedCVar

    -- turn off for screenshots
    unless (showPauseValue == 0 || pausedValue == 0) $ do
      vidDef' <- use $ globals.vidDef
      Just renderer <- use $ globals.re
      Just (width, _) <- (renderer^.rRefExport.reDrawGetPicSize) "pause"
      (renderer^.rRefExport.reDrawPic) (((vidDef'^.vdWidth) - width) `div` 2) ((vidDef'^.vdHeight) `div` 2 + 8) "pause"

drawConsole :: Quake ()
drawConsole = do
    Console.checkResize

    state <- use $ globals.cls.csState
    refreshPrepped <- use $ globals.cl.csRefreshPrepped

         -- forced full screen console
    if | state == Constants.caDisconnected || state == Constants.caConnecting ->
           Console.drawConsole 1

         -- connected, but can't render
       | state /= Constants.caActive || not refreshPrepped -> do
           Console.drawConsole 0.5

           vidDef' <- use $ globals.vidDef
           Just renderer <- use $ globals.re
           (renderer^.rRefExport.reDrawFill) 0 ((vidDef'^.vdHeight) `div` 2) (vidDef'^.vdWidth) ((vidDef'^.vdHeight) `div` 2) 0

       | otherwise -> do
           conCurrent <- use $ scrGlobals.scrConCurrent
           if conCurrent /= 0
             then Console.drawConsole conCurrent
             else do
               keyDest <- use $ globals.cls.csKeyDest
               when (keyDest == Constants.keyGame || keyDest == Constants.keyMessage) $
                 Console.drawNotify -- only draw notify in game
           
{-
- DrawCinematic
- 
- Returns true if a cinematic is active, meaning the view rendering should
- be skipped.
-}
drawCinematic :: Quake Bool
drawCinematic = do
    cinematicTime <- use $ globals.cl.csCinematicTime
    keyDest <- use $ globals.cls.csKeyDest
    Just renderer <- use $ globals.re

    if | cinematicTime <= 0 ->
           return False

       | keyDest == Constants.keyMenu -> do
           -- blank screen and pause if menu is up
           (renderer^.rRefExport.reCinematicSetPalette) Nothing
           globals.cl.csCinematicPaletteActive .= False
           return True

       | otherwise -> do
           cinematicPaletteActive <- use $ globals.cl.csCinematicPaletteActive

           unless cinematicPaletteActive $ do
             cinematicPalette <- use $ globals.cl.csCinematicPalette
             (renderer^.rRefExport.reCinematicSetPalette) (Just cinematicPalette)
             globals.cl.csCinematicPaletteActive .= True

           cin <- use $ scrGlobals.scrCin

           case cin^.cPic of
             Nothing -> return True
             Just picture -> do
               vidDef' <- use $ globals.vidDef
               (renderer^.rRefExport.reDrawStretchRaw) 0 0 (vidDef'^.vdWidth) (vidDef'^.vdHeight) (cin^.cWidth) (cin^.cHeight) picture
               return True

drawLoading :: Quake ()
drawLoading = do
    loading <- use $ scrGlobals.scrDrawLoading

    unless (loading == 0) $ do
      scrGlobals.scrDrawLoading .= 0
      vidDef' <- use $ globals.vidDef
      Just renderer <- use $ globals.re

      Just (width, height) <- (renderer^.rRefExport.reDrawGetPicSize) "loading"
      (renderer^.rRefExport.reDrawPic) (((vidDef'^.vdWidth) - width) `div` 2) (((vidDef'^.vdHeight) - height) `div` 2) "loading"

dirtyScreen :: Quake ()
dirtyScreen = do
    vidDef' <- use $ globals.vidDef

    addDirtyPoint 0 0
    addDirtyPoint ((vidDef'^.vdWidth) - 1) ((vidDef'^.vdHeight) - 1)

addDirtyPoint :: Int -> Int -> Quake ()
addDirtyPoint x y = do
    dirty <- use $ scrGlobals.scrDirty

    when (x < (dirty^.x1)) $
      scrGlobals.scrDirty.x1 .= x

    when (x > (dirty^.x2)) $
      scrGlobals.scrDirty.x2 .= x

    when (y < (dirty^.y1)) $
      scrGlobals.scrDirty.y1 .= y

    when (y > (dirty^.y2)) $
      scrGlobals.scrDirty.y2 .= y

drawCrosshair :: Quake ()
drawCrosshair = do
    crosshair <- crosshairCVar

    unless ((crosshair^.cvValue) == 0) $ do
      when (crosshair^.cvModified) $ do
        CVar.update crosshair { _cvModified = False }
        touchPics

      crosshairPic <- use $ scrGlobals.scrCrosshairPic

      unless (B.null crosshairPic) $ do
        vrect <- use $ globals.scrVRect
        Just renderer <- use $ globals.re
        crosshairWidth <- use $ scrGlobals.scrCrosshairWidth
        crosshairHeight <- use $ scrGlobals.scrCrosshairHeight

        (renderer^.rRefExport.reDrawPic) ((vrect^.vrX) + ((vrect^.vrWidth) - crosshairWidth) `shiftR` 1)
                                         ((vrect^.vrY) + ((vrect^.vrHeight) - crosshairHeight) `shiftR` 1)
                                         crosshairPic

executeLayoutString :: B.ByteString -> Quake ()
executeLayoutString str = do
    shouldSkip <- checkIfShouldSkip

    unless shouldSkip $ do
      parseLayoutString 0 0 3 0

  where checkIfShouldSkip :: Quake Bool
        checkIfShouldSkip = do
          state <- use $ globals.cls.csState
          refreshPrepped <- use $ globals.cl.csRefreshPrepped

          return $ if state /= Constants.caActive || not refreshPrepped || B.length str == 0
                     then True
                     else False

        parseLayoutString :: Int -> Int -> Int -> Int -> Quake ()
        parseLayoutString x y width idx
          | idx >= B.length str = return ()
          | otherwise = do
              (maybeToken, newIdx) <- Com.parse str (B.length str) idx

              case maybeToken of
                Nothing -> parseLayoutString x y width newIdx
                Just token -> do
                  (x', y', width', finalIdx) <- processToken x y width newIdx token
                  parseLayoutString x' y' width' finalIdx

        processToken :: Int -> Int -> Int -> Int -> B.ByteString -> Quake (Int, Int, Int, Int)
        processToken x y width idx token =
          case token of
            "xl" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let x' = Lib.atoi tkn
              return (x', y, width, newIdx)

            "xr" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              w <- use $ globals.vidDef.vdWidth
              let x' = w + Lib.atoi tkn
              return (x', y, width, newIdx)

            "xv" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              w <- use $ globals.vidDef.vdWidth
              let x' = w `div` 2 - 160 + Lib.atoi tkn
              return (x', y, width, newIdx)

            "yt" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let y' = Lib.atoi tkn
              return (x, y', width, newIdx)

            "yb" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              h <- use $ globals.vidDef.vdHeight
              let y' = h + Lib.atoi tkn
              return (x, y', width, newIdx)

            "yv" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              h <- use $ globals.vidDef.vdHeight
              let y' = h `div` 2 - 120 + Lib.atoi tkn
              return (x, y', width, newIdx)

            "pic" -> do
              -- draw a pick from a stat number
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix (Lib.atoi tkn)
              let value' = fromIntegral value
              when (value' >= Constants.maxImages) $
                Com.comError Constants.errDrop "Pic >= MAX_IMAGES"

              Just cs <- preuse $ globals.cl.csConfigStrings.ix (Constants.csImages + value')
              when (B.length cs > 0) $ do -- IMPROVE: do we need to introduce Maybe ByteString in csConfigStrings ?
                addDirtyPoint x y
                addDirtyPoint (x + 23) (y + 23)
                Just renderer <- use $ globals.re
                (renderer^.rRefExport.reDrawPic) x y cs

              return (x, y, width, newIdx)

            "client" -> do
              -- draw a deathmatch client block
              vidDef' <- use $ globals.vidDef

              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let x' = (vidDef'^.vdWidth) `div` 2 - 160 + Lib.atoi tkn

              (Just tkn2, newIdx2) <- Com.parse str (B.length str) newIdx
              let y' = (vidDef'^.vdHeight) `div` 2 - 120 + Lib.atoi tkn2

              addDirtyPoint x' y'
              addDirtyPoint (x' + 159) (y' + 31)

              (Just tkn3, newIdx3) <- Com.parse str (B.length str) newIdx2
              let clientInfoIdx = Lib.atoi tkn3
              when (clientInfoIdx >= Constants.maxClients || clientInfoIdx < 0) $
                Com.comError Constants.errDrop "client >= MAX_CLIENTS"

              Just clientInfo <- preuse $ globals.cl.csClientInfo.ix clientInfoIdx
              
              (Just tkn4, newIdx4) <- Com.parse str (B.length str) newIdx3
              let score = Lib.atoi tkn4

              (Just tkn5, newIdx5) <- Com.parse str (B.length str) newIdx4
              let ping = Lib.atoi tkn5

              (Just tkn6, newIdx6) <- Com.parse str (B.length str) newIdx5
              let time = Lib.atoi tkn6

              Console.drawAltString (x' + 32) y' (clientInfo^.ciName)
              Console.drawString (x' + 32) (y' + 8) "Score: "
              Console.drawAltString (x' + 32 + 7 * 8) (y' + 8) (BC.pack $ show score)
              Console.drawString (x' + 32) (y' + 16) ("Ping:  " `B.append` BC.pack (show ping)) -- IMPROVE?
              Console.drawString (x' + 32) (y' + 24) ("Time:  " `B.append` BC.pack (show time)) -- IMPROVE?

              Just renderer <- use $ globals.re

              if isNothing (clientInfo^.ciIcon)
                then do
                  iconName <- use $ globals.cl.csBaseClientInfo.ciIconName
                  (renderer^.rRefExport.reDrawPic) x' y' iconName
                else
                  (renderer^.rRefExport.reDrawPic) x' y' (clientInfo^.ciIconName)

              return (x', y', width, newIdx6)
              
            "ctf" -> do
              -- draw a ctf client block
              vidDef' <- use $ globals.vidDef

              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let x' = (vidDef'^.vdWidth) `div` 2 - 160 + Lib.atoi tkn

              (Just tkn2, newIdx2) <- Com.parse str (B.length str) newIdx
              let y' = (vidDef'^.vdHeight) `div` 2 - 120 + Lib.atoi tkn2

              addDirtyPoint x' y'
              addDirtyPoint (x' + 159) (y' + 31)

              (Just tkn3, newIdx3) <- Com.parse str (B.length str) newIdx2
              let clientInfoIdx = Lib.atoi tkn3
              when (clientInfoIdx >= Constants.maxClients || clientInfoIdx < 0) $
                Com.comError Constants.errDrop "client >= MAX_CLIENTS"

              Just clientInfo <- preuse $ globals.cl.csClientInfo.ix clientInfoIdx

              (Just tkn4, newIdx4) <- Com.parse str (B.length str) newIdx3
              let score = Lib.atoi tkn4

              (Just tkn5, newIdx5) <- Com.parse str (B.length str) newIdx4
              let tmpPing = Lib.atoi tkn5
                  ping = if tmpPing > 999 then 999 else tmpPing

              let block = BC.pack $ printf "%3d %3d %-12.12s" score ping (BC.unpack $ clientInfo^.ciName)

              playerNum <- use $ globals.cl.csPlayerNum
              if clientInfoIdx == playerNum
                then Console.drawAltString x' y' block
                else Console.drawString x' y' block

              return (x', y', width, newIdx5)

            "picn" -> do
              -- draw a pic from a name
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              addDirtyPoint x y
              addDirtyPoint (x + 23) (y + 23)

              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawPic) x y tkn

              return (x, y, width, newIdx)

            "num" -> do
              -- draw a number
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let width' = Lib.atoi tkn

              (Just tkn2, newIdx2) <- Com.parse str (B.length str) newIdx
              let statsIdx = Lib.atoi tkn2

              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix statsIdx
              drawField x y 0 width' (fromIntegral value)

              return (x, y, width', newIdx2)

            "hnum" -> do
              -- health number
              let width' = 3
              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statHealth

              color <- if | value > 25 -> return 0 -- green
                          | value > 0 -> do
                              serverFrame <- use $ globals.cl.csFrame.fServerFrame
                              return $ (serverFrame `shiftR` 2) .&. 1
                          | otherwise -> return 1

              Just sf <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statFlashes
              when (sf .&. 1 /= 0) $ do
                Just renderer <- use $ globals.re
                (renderer^.rRefExport.reDrawPic) x y "field_3"

              drawField x y color width' (fromIntegral value)

              return (x, y, width', idx)
              
            "anum" -> do
              -- ammo number
              let width' = 3
              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statAmmo

              color <- if | value > 5 -> return 0 -- green
                          | value >= 0 -> do
                              serverFrame <- use $ globals.cl.csFrame.fServerFrame
                              return $ (serverFrame `shiftR` 2) .&. 1
                          | otherwise -> return (-1)
              
              -- negative number = don't show
              unless (color == -1) $ do
                Just sf <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statFlashes
                when (sf .&. 4 /= 0) $ do
                  Just renderer <- use $ globals.re
                  (renderer^.rRefExport.reDrawPic) x y "field_3"

                drawField x y color width' (fromIntegral value)

              return (x, y, width', idx)

            "rnum" -> do
              -- armor number
              let width' = 3
              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statArmor

              unless (value < 1) $ do
                Just sf <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix Constants.statFlashes
                when (sf .&. 2 /= 0) $ do
                  Just renderer <- use $ globals.re
                  (renderer^.rRefExport.reDrawPic) x y "field_3"

                drawField x y 0 width' (fromIntegral value)

              return (x, y, width', idx)

            "stat_string" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              let index = Lib.atoi tkn
              
              when (index < 0 || index >= Constants.maxConfigStrings) $
                Com.comError Constants.errDrop "Bad stat_string index"

              Just csIndexTmp <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix index
              let csIndex = fromIntegral csIndexTmp

              when (csIndex < 0 || csIndex >= Constants.maxConfigStrings) $
                Com.comError Constants.errDrop "Bad stat_string index"

              Just configStr <- preuse $ globals.cl.csConfigStrings.ix csIndex

              Console.drawString x y configStr

              return (x, y, width, newIdx)

            "cstring" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              drawHUDString tkn x y 320 0
              return (x, y, width, newIdx)

            "string" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              Console.drawString x y tkn
              return (x, y, width, newIdx)

            "cstring2" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              drawHUDString tkn x y 320 0x80
              return (x, y, width, newIdx)

            "string2" -> do
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              Console.drawAltString x y tkn
              return (x, y, width, newIdx)

            "if" -> do
              -- draw a number
              (Just tkn, newIdx) <- Com.parse str (B.length str) idx
              Just value <- preuse $ globals.cl.csFrame.fPlayerState.psStats.ix (Lib.atoi tkn)

              if value == 0
                then do
                  -- skip to endif
                  finalIdx <- skipToEndIf newIdx
                  return (x, y, width, finalIdx)
                else
                  return (x, y, width, newIdx)

            _ -> return (x, y, width, idx)

        skipToEndIf :: Int -> Quake Int
        skipToEndIf idx
          | idx >= B.length str = return idx
          | otherwise = do
              (tkn, newIdx) <- Com.parse str (B.length str) idx
              case tkn of
                Nothing -> skipToEndIf newIdx
                Just token -> if token == "endif"
                                then return newIdx
                                else skipToEndIf newIdx

drawField :: Int -> Int -> Int -> Int -> Int -> Quake ()
drawField x y color width value = do
    unless (width < 1) $ do
      let width' = if width > 5 then 5 else width

      addDirtyPoint x y
      addDirtyPoint (x + width' * charWidth + 2) (y + 23)

      let num = BC.pack (show value)
          len = B.length num
          len' = if len > width' then width' else len
          x' = x + 2 + charWidth * (width' - len')

      draw num x' 0 len'

  where draw :: B.ByteString -> Int -> Int -> Int -> Quake ()
        draw num x' idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let ptr = num `BC.index` idx
                  frame = if ptr == '-'
                            then statMinus
                            else ord ptr - ord '0'
                  pic = if color == 0
                          then sbNums1 V.! frame
                          else sbNums2 V.! frame
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawPic) x' y pic
              draw num (x' + charWidth) (idx + 1) maxIdx

drawHUDString :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawHUDString _ _ _ _ _ = do
    io (putStrLn "SCR.drawHUDString") >> undefined -- TODO

drawCenterString :: Quake ()
drawCenterString = do
    str <- use $ scrGlobals.scrCenterString

    unless (B.null str) $ do
      centerLines <- use $ scrGlobals.scrCenterLines
      vidDef' <- use $ globals.vidDef

      -- the finale prints the characters one at a time
      let y = if centerLines <= 4
                then truncate (fromIntegral (vidDef'^.vdHeight) * 0.35)
                else 48

      drawLines str vidDef' 9999 y 0

  where drawLines :: B.ByteString -> VidDefT -> Int -> Int -> Int -> Quake ()
        drawLines str vidDef' remaining y start = do
          -- scan the width of the line
          let len = scanLineWidth str start 0 40
              x = ((vidDef'^.vdWidth) - len * 8) `div` 2

          addDirtyPoint x y

          (remaining', x') <- drawLine str remaining start x y 0 len
          let y' = y + 8

          addDirtyPoint x' y'

          let start' = findEndOfLine str start

          when (start' < B.length str) $
            drawLines str vidDef' remaining' y' (start' + 1) -- skip the \n

        scanLineWidth :: B.ByteString -> Int -> Int -> Int -> Int
        scanLineWidth str s idx maxIdx
          | idx >= maxIdx || (s + idx) >= B.length str = idx
          | otherwise =
              if str `BC.index` (s + idx) == '\n'
                then idx
                else scanLineWidth str s (idx + 1) maxIdx

        drawLine :: B.ByteString -> Int -> Int -> Int -> Int -> Int -> Int -> Quake (Int, Int)
        drawLine str remaining start x y idx maxIdx
          | idx >= maxIdx = return (remaining, x)
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) x y (ord (str `BC.index` (start + idx)))
              if remaining == 0
                then return (0, x)
                else drawLine str (remaining - 1) start (x + 8) y (idx + 1) maxIdx

        findEndOfLine :: B.ByteString -> Int -> Int
        findEndOfLine str idx
          | idx >= B.length str || str `BC.index` idx == '\n' = idx
          | otherwise = findEndOfLine str (idx + 1)

readNextFrame :: Quake (Maybe B.ByteString)
readNextFrame = do
    Just cinematicFileHandle <- use $ globals.cl.csCinematicFile

    -- read the next frame
    command <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)

    if command == 2
      then
        -- last frame marker
        return Nothing

      else do
        when (command == 1) $ do
          -- read palette
          io (B.hGet cinematicFileHandle 768) >>= (globals.cl.csCinematicPalette .=)
          -- dubious... exposes an edge case
          globals.cl.csCinematicPaletteActive .= False

        -- decompress the next frame
        size <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)

        when (size > 0x20000 || size < 1) $
          Com.comError Constants.errDrop ("Bad compressed frame size:" `B.append` BC.pack (show size)) -- IMPROVE

        compressed <- io $ B.hGet cinematicFileHandle size

        -- read sound
        cinematicFrame <- use $ globals.cl.csCinematicFrame
        cin <- use $ scrGlobals.scrCin
        let start = cinematicFrame * (cin^.cSRate) `div` 14
            end = (cinematicFrame + 1) * (cin^.cSRate) `div` 14
            count = end - start

        position <- io $ hTell cinematicFileHandle
        S.rawSamples count (cin^.cSRate) (cin^.cSWidth) (cin^.cSChannels) cinematicFileHandle
        -- skip the sound samples
        io $ hSeek cinematicFileHandle AbsoluteSeek (fromIntegral (fromIntegral position + count * (cin^.cSWidth) * (cin^.cSChannels)))
        
        pic <- huff1Decompress compressed size
        globals.cl.csCinematicFrame += 1

        return (Just pic)

huff1Decompress :: B.ByteString -> Int -> Quake B.ByteString
huff1Decompress _ _ = do
    io (putStrLn "SCR.huff1Decompress") >> undefined -- TODO

debugGraph :: Float -> Int -> Quake ()
debugGraph _ _ = return () -- IMPLEMENT ME!

playCinematic :: B.ByteString -> Quake ()
playCinematic arg = do
    globals.cl.csCinematicFrame .= 0

    if ".pcx" `BC.isSuffixOf` arg
      then do
        -- static pcx image
        let name = "pics/" `B.append` arg
        cinematics <- use $ scrGlobals.scrCin
        (picSize, Just palette, Just cinematics') <- loadPCX name True (Just cinematics)
        scrGlobals.scrCin .= cinematics'

        zoom (globals.cl) $ do
          csCinematicFrame .= -1
          csCinematicTime .= 1

        endLoadingPlaque

        globals.cls.csState .= Constants.caActive

        when (picSize == 0 || isNothing (cinematics'^.cPic)) $ do
          Com.println (name `B.append` " not found.")
          globals.cl.csCinematicTime .= 0
        
      else do
        let name = "video/" `B.append` arg
        -- cinematicFile <- FS.loadMappedFile name
        cinematicFile <- FS.fOpenFile name
        globals.cl.csCinematicFile .= cinematicFile
        
        case cinematicFile of
          Nothing -> do
            -- Com.comError Constants.errDrop ("Cinematic " `B.append` name `B.append` " not found.\n")
            finishCinematic
            globals.cl.csCinematicTime .= 0

          Just cinematicFileHandle -> do
            endLoadingPlaque

            globals.cls.csState .= Constants.caActive

            width     <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)
            height    <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)
            sRate     <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)
            sWidth    <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)
            sChannels <- io $ BL.hGet cinematicFileHandle 4 >>= return . (runGet getInt)

            zoom (scrGlobals.scrCin) $ do
              cWidth     .= width
              cHeight    .= height
              cSRate     .= sRate
              cSWidth    .= sWidth
              cSChannels .= sChannels

            huff1TableInit

            scrGlobals.scrCin.cRestartSound .= True
            globals.cl.csCinematicFrame .= 0
            readNextFrame >>= (scrGlobals.scrCin.cPic .=)
            Timer.milliseconds >>= \ms -> globals.cl.csCinematicTime .= ms

loadPCX :: B.ByteString -> Bool -> Maybe CinematicsT -> Quake (Int, Maybe B.ByteString, Maybe CinematicsT)
loadPCX fileName loadPalette cinematics = do
    io (putStrLn "SCR.loadPCX") >> undefined -- TODO

{-
- =============== SCR_TouchPics
- 
- Allows rendering code to cache all needed sbar graphics ===============
-}
touchPics :: Quake ()
touchPics = do
    Just renderer <- use $ globals.re
    void $ V.mapM (renderer^.rRefExport.reRegisterPic) sbNums1

    crosshairCVar >>= \crosshair ->
      when ((crosshair^.cvValue) /= 0) $ do
        v <- if (crosshair^.cvValue) > 3 || (crosshair^.cvValue) < 0
               then do
                 CVar.update crosshair { _cvValue = 3 }
                 return 3
               else
                 return (crosshair^.cvValue)

        let i :: Int = truncate v
            pic = "ch" `B.append` BC.pack (show i)
        scrGlobals.scrCrosshairPic .= pic

        Just (width, height) <- (renderer^.rRefExport.reDrawGetPicSize) pic
        scrGlobals.scrCrosshairWidth .= width
        scrGlobals.scrCrosshairHeight .= height

        when (width == 0) $
          scrGlobals.scrCrosshairPic .= ""

{-
- ============== SCR_CenterPrint ==============
- 
- Called for important messages that should stay in the center of the
- screen for a few moments
-}
centerPrint :: B.ByteString -> Quake ()
centerPrint str = do
    centerTimeValue <- liftM (^.cvValue) scrCenterTimeCVar
    time <- use $ globals.cl.csTime

    zoom scrGlobals $ do
      scrCenterString .= str
      scrCenterTimeOff .= centerTimeValue
      scrCenterTimeStart .= fromIntegral time
      -- count the number of lines for centering
      scrCenterLines .= 1 + BC.count '\n' str

    -- echo it to the console
    Com.printf "\n\n\35\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\37\n\n"

    unless (B.null str) $
      outputLines 0

    Com.printf "\n\n\35\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\36\37\n\n"
    Console.clearNotify

  where outputLines :: Int -> Quake ()
        outputLines s = do
          -- scan the width of the line
          let len = scanLineWidth s 0 40
              line = buildSpaces 0 ((40 - len) `div` 2) mempty
              line' = (appendLine s 0 len line) <> BB.word8 10 -- 10 is '\n'

          Com.printf (BL.toStrict $ BB.toLazyByteString line')

          let s' = findEndOfLine s

          when (s' < B.length str) $
            outputLines (s' + 1) -- skip the \n

        scanLineWidth :: Int -> Int -> Int -> Int
        scanLineWidth s idx maxIdx
          | idx >= maxIdx || (s + idx) >= B.length str = idx
          | otherwise =
              if str `BC.index` (s + idx) == '\n' || str `B.index` (s + idx) == 0
                then idx
                else scanLineWidth s (idx + 1) maxIdx

        buildSpaces :: Int -> Int -> BB.Builder -> BB.Builder
        buildSpaces idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise = buildSpaces (idx + 1) maxIdx (acc <> BB.word8 32) -- 32 is SPACE

        appendLine :: Int -> Int -> Int -> BB.Builder -> BB.Builder
        appendLine s idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise = appendLine s (idx + 1) maxIdx (acc <> BB.word8 (str `B.index` (s + idx)))

        findEndOfLine :: Int -> Int
        findEndOfLine idx
          | idx >= B.length str || str `BC.index` idx == '\n' = idx
          | otherwise = findEndOfLine (idx + 1)

huff1TableInit :: Quake ()
huff1TableInit = do
    Just cinematicFileHandle <- use $ globals.cl.csCinematicFile
    tableInit cinematicFileHandle 0 256

  where tableInit :: Handle -> Int -> Int -> Quake ()
        tableInit cinematicFileHandle idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              -- read a row of counts
              counts <- io $ B.hGet cinematicFileHandle 256
              let hCount = UV.generate 512 (\i -> if i < 256 then fromIntegral (counts `B.index` i) else 0)

              -- build the nodes
              let nodeBase = idx * 256 * 2
                  (hNodes, hUsed, hCount', numHNodes) = initNodes (UV.replicate (256 * 256 * 2) 0) (UV.replicate 512 0) hCount nodeBase 256

              zoom (scrGlobals.scrCin) $ do
                cHNodes1 .= Just hNodes
                cHUsed .= hUsed
                cHCount .= hCount'
                cNumHNodes1.ix idx .= numHNodes - 1
              
              tableInit cinematicFileHandle (idx + 1) maxIdx

        initNodes :: UV.Vector Int -> UV.Vector Int -> UV.Vector Int -> Int -> Int -> (UV.Vector Int, UV.Vector Int, UV.Vector Int, Int)
        initNodes hNodes hUsed hCount nodeBase numHNodes
          | numHNodes >= 511 = (hNodes, hUsed, hCount, numHNodes)
          | otherwise =
              let index = nodeBase + (numHNodes - 256) * 2
                  -- pick two lowest counts
                  b1 = smallestNode1 hUsed hCount numHNodes
              in if b1 == -1
                   then (hNodes UV.// [(index, b1)], hUsed, hCount, numHNodes)
                   else let hNodes' = hNodes UV.// [(index, b1)]
                            hUsed' = hUsed UV.// [(b1, 1)]
                            b2 = smallestNode1 hUsed' hCount numHNodes
                        in if b2 == -1
                             then (hNodes' UV.// [(index + 1, b2)], hUsed', hCount, numHNodes)
                             else let hNodes'' = hNodes' UV.// [(index + 1, b2)]
                                      hUsed'' = hUsed' UV.// [(b2, 1)]
                                  in initNodes hNodes'' hUsed'' (hCount UV.// [(numHNodes, (hCount UV.! b1) + (hCount UV.! b2))]) nodeBase (numHNodes + 1)

smallestNode1 :: UV.Vector Int -> UV.Vector Int -> Int -> Int
smallestNode1 hUsed hCount numHNodes =
    findBestNode 99999999 (-1) 0

  where findBestNode :: Int -> Int -> Int -> Int
        findBestNode best bestNode idx
          | idx >= numHNodes = bestNode
          | otherwise = if | hUsed UV.! idx /= 0 -> findBestNode best bestNode (idx + 1)
                           | hCount UV.! idx == 0 -> findBestNode best bestNode (idx + 1)
                           | (hCount UV.! idx) < best -> findBestNode (hCount UV.! idx) idx (idx + 1)
                           | otherwise -> findBestNode best bestNode (idx + 1)
