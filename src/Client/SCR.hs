{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.SCR where

import Control.Lens ((.=), use, (^.), _1, _2, ix, preuse, zoom)
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.&.), complement)
import Data.Maybe (isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLInv as CLInv
import qualified Client.Console as Console
import qualified Client.Menu as Menu
import {-# SOURCE #-} qualified Client.V as V
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

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

dirtyScreen :: Quake ()
dirtyScreen = io (putStrLn "SCR.dirtyScreen") >> undefined -- TODO

addDirtyPoint :: Int -> Int -> Quake ()
addDirtyPoint _ _ = do
    io (putStrLn "SCR.addDirtyPoint") >> undefined -- TODO

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
              undefined

            "picn" -> do
              undefined

            "num" -> do
              undefined

            "hnum" -> do
              undefined
              
            "anum" -> do
              undefined

            "rnum" -> do
              undefined

            "stat_string" -> do
              undefined

            "cstring" -> do
              undefined

            "string" -> do
              undefined

            "cstring2" -> do
              undefined

            "string2" -> do
              undefined

            "if" -> do
              undefined

            _ -> return (x, y, width, idx)
