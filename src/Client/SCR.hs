{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.SCR where

import Control.Lens ((.=), use, (^.), _1, _2, ix, preuse, zoom, (-=))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.&.), complement, shiftR)
import Data.Char (ord)
import Data.Maybe (isNothing, isJust)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLInv as CLInv
import {-# SOURCE #-} qualified Client.Console as Console
import qualified Client.Menu as Menu
import {-# SOURCE #-} qualified Client.V as V
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

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

finishCinematic :: Quake ()
finishCinematic = io (putStrLn "SCR.finishCinematic") >> undefined -- TODO

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
    io (putStrLn "SCR.drawLayout") >> undefined -- TODO

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
           
drawCinematic :: Quake ()
drawCinematic = do
    io (putStrLn "SCR.drawCinematic") >> undefined -- TODO

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
dirtyScreen = io (putStrLn "SCR.dirtyScreen") >> undefined -- TODO

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
    io (putStrLn "SCR.drawCrosshair") >> undefined -- TODO

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
              when (B.length cs > 0) $ do -- TODO: do we need to introduce Maybe ByteString in csConfigStrings ?
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
drawField _ _ _ _ _ = do
    io (putStrLn "SCR.drawField") >> undefined -- TODO

drawHUDString :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawHUDString _ _ _ _ _ = do
    io (putStrLn "SCR.drawHUDString") >> undefined -- TODO

drawCenterString :: Quake ()
drawCenterString = io (putStrLn "SCR.drawCenterString") >> undefined -- TODO

readNextFrame :: Quake (Maybe B.ByteString)
readNextFrame = do
    io (putStrLn "SCR.readNextFrame") >> undefined -- TODO

debugGraph :: Float -> Int -> Quake ()
debugGraph _ _ = io (putStrLn "IMPLEMENT ME!! SCR.debugGraph") >> return ()

playCinematic :: B.ByteString -> Quake ()
playCinematic _ = do
    io (putStrLn "SCR.playCinematic") >> undefined -- TODO

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
