{-# LANGUAGE FlexibleContexts #-}
module Client.SCR
  ( addDirtyPoint
  , beginLoadingPlaque
  , centerPrint
  , debugGraph
  , dirtyScreen
  , endLoadingPlaque
  , finishCinematic
  , initialize
  , playCinematic
  , runCinematic
  , runConsole
  , touchPics
  , updateScreen
  ) where

import           Client.CinematicsT
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInv as CLInv
import qualified Client.Console as Console
import           Client.ConsoleT
import           Client.DirtyT
import           Client.FrameT
import qualified Client.Menu as Menu
import           Client.RefExportT
import           Client.SCRShared
import qualified Client.V as V
import           Client.VidDefT
import           Client.VRectT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import           Game.PlayerStateT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import           QCommon.NetChanT
import qualified QCommon.SZ as SZ
import           QuakeState
import           Render.Renderer
import qualified Sound.S as S
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode, getInt)

import           Control.Applicative (liftA2)
import           Control.Lens (preuse, use, ix, (^.), (.=), (-=), (%=), (+=), (&), (.~), _1, _2)
import           Control.Monad (when, unless, void, join)
import           Control.Monad.ST (ST, runST)
import           Data.Binary.Get (runGet)
import           Data.Bits (complement, shiftR, shiftL, (.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Data.Word (Word8)
import           System.IO (Handle, hTell, hSeek, SeekMode(AbsoluteSeek))

beginLoadingPlaque :: Quake ()
beginLoadingPlaque =
  do S.stopAllSounds
     globals.gCl.csSoundPrepped .= False
     join (liftA2 beginWhenReady (use (globals.gCls)) developerCVar)

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
              checkKeyDest cinematicPaletteActive =<< use (globals.gCls.csKeyDest)
          | otherwise =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 calcVrect
                 tileClear
                 V.renderView separationValue
                 drawStats
                 checkStatLayout =<< preuse (globals.gCl.csFrame.fPlayerState.psStats.ix Constants.statLayouts)
                 drawNet =<< use (globals.gCls.csNetChan)
                 checkDrawCenterString
                 drawFPS =<< fpsCVar
                 drawPause
                 drawConsole
                 Menu.draw renderer =<< use (globals.gCls.csKeyDest)
                 drawLoading renderer
        checkStatLayout Nothing = Com.fatalError "SCR.runFrames checkStatLayout has Nothing"
        checkStatLayout (Just statLayout) =
          do when (statLayout .&. 1 /= 0) drawLayout
             when (statLayout .&. 2 /= 0) CLInv.drawInventory
        dimError = Com.fatalError "SCR.runFrames reDrawGetPicSize returned Nothing"
        drawPic vidDef (width, height) =
          (renderer^.rRefExport.reDrawPic) (((vidDef^.vdWidth) - width) `div` 2) (((vidDef^.vdHeight) - height) `div` 2) "loading"
        checkKeyDest cinematicPaletteActive keyDest
          | keyDest == Constants.keyMenu =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 Menu.draw renderer =<< use (globals.gCls.csKeyDest)
          | keyDest == Constants.keyConsole =
              do when cinematicPaletteActive $
                   do (renderer^.rRefExport.reCinematicSetPalette) Nothing
                      globals.gCl.csCinematicPaletteActive .= False
                 drawConsole
          | otherwise = void drawCinematic

runCinematic :: Quake ()
runCinematic =
  do cl <- use (globals.gCl)
     cls <- use (globals.gCls)
     doRunCinematic cl cls

doRunCinematic :: ClientStateT -> ClientStaticT -> Quake ()
doRunCinematic cl cls
  | cl^.csCinematicTime <= 0 = stopCinematic
  | cl^.csCinematicFrame == -1 = return ()
  | cls^.csKeyDest /= Constants.keyGame =
      globals.gCl.csCinematicTime .= (cls^.csRealTime) - (cl^.csCinematicFrame) * 1000 `div` 14
  | frame > (cl^.csCinematicFrame) =
      do when (frame > (cl^.csCinematicFrame) + 1) $
           do Com.printf (B.concat ["Dropped frame: ", encode frame, " > ", encode ((cl^.csCinematicTime) + 1)])
              globals.gCl.csCinematicTime .= (cls^.csRealTime) - (cl^.csCinematicFrame) * 1000 `div` 14
         copyValue (use (scrGlobals.scrCin.cPicPending)) (scrGlobals.scrCin.cPic)
         copyValue readNextFrame (scrGlobals.scrCin.cPicPending)
         checkFinishCinematic =<< use (scrGlobals.scrCin.cPicPending)
  | otherwise = return ()
  where frameF = fromIntegral (cls^.csRealTime) - fromIntegral (cl^.csCinematicTime) * 14 / 1000 :: Float
        frame = truncate frameF :: Int
        copyValue a b = a >>= (b .=)

checkFinishCinematic :: Maybe (UV.Vector Word8) -> Quake ()
checkFinishCinematic (Just _) = return ()
checkFinishCinematic Nothing =
  do stopCinematic
     finishCinematic
     globals.gCl.csCinematicTime .= 1
     beginLoadingPlaque
     globals.gCl.csCinematicTime .= 0

runConsole :: Quake ()
runConsole =
  do keyDest <- use (globals.gCls.csKeyDest)
     let conLines | keyDest == Constants.keyConsole = 0.5
                  | otherwise = 0
     scrGlobals.scrConLines .= conLines
     conCurrent <- use (scrGlobals.scrConCurrent)
     frameTime <- use (globals.gCls.csFrameTime)
     conSpeed <- scrConSpeedCVar
     doRunConsole conLines conCurrent frameTime conSpeed

doRunConsole :: Float -> Float -> Float -> CVarT -> Quake ()
doRunConsole conLines conCurrent frameTime conSpeed
  | conLines < conCurrent = scrGlobals.scrConCurrent .= max v1 conLines
  | conLines > conCurrent = scrGlobals.scrConCurrent .= min v2 conLines
  | otherwise = return ()
  where v1 = conCurrent - (conSpeed^.cvValue) * frameTime
        v2 = conCurrent + (conSpeed^.cvValue) * frameTime

finishCinematic :: Quake ()
finishCinematic =
  do MSG.writeByteI (globals.gCls.csNetChan.ncMessage) Constants.clcStringCmd
     serverCount <- use (globals.gCl.csServerCount)
     SZ.printSB (globals.gCls.csNetChan.ncMessage) (B.concat ["nextserver ", encode serverCount, "\n"])

calcVrect :: Quake ()
calcVrect =
  do checkViewSize =<< viewSizeCVar
     join (liftA2 calcVrectAndSave viewSizeCVar vidDef)
  where vidDef = use (globals.gVidDef)

checkViewSize :: CVarT -> Quake ()
checkViewSize viewSize =
  do when ((viewSize^.cvValue) < 40) (void (CVar.set "viewsize" "40"))
     when ((viewSize^.cvValue) > 100) (void (CVar.set "viewsize" "100"))

calcVrectAndSave :: CVarT -> VidDefT -> Quake ()
calcVrectAndSave viewSize vidDef =
  globals.gScrVRect %= (\v -> v & vrWidth .~ w
                                & vrHeight .~ h
                                & vrX .~ ((vidDef^.vdWidth) - w) `div` 2
                                & vrY .~ ((vidDef^.vdHeight) - h) `div` 2)
  where size = truncate (viewSize^.cvValue) :: Int
        w = ((vidDef^.vdWidth) * size `div` 100) .&. (complement 7)
        h = ((vidDef^.vdHeight) * size `div` 100) .&. (complement 1)

tileClear :: Quake ()
tileClear =
  do checkDrawAll =<< scrDrawAllCVar
     conCurrent <- use (scrGlobals.scrConCurrent)
     viewSize <- viewSizeCVar
     cinematicTime <- use (globals.gCl.csCinematicTime)
     unless (conCurrent == 1 || (viewSize^.cvValue) == 100 || cinematicTime > 0) $
       doTileClear conCurrent

checkDrawAll :: CVarT -> Quake ()
checkDrawAll drawAll = when ((drawAll^.cvValue) /= 0) dirtyScreen

doTileClear :: Float -> Quake ()
doTileClear conCurrent =
  do dirty <- use (scrGlobals.scrDirty)
     oldDirty <- use (scrGlobals.scrOldDirty)
     vidDef <- use (globals.gVidDef)
     scrGlobals.scrOldDirty .= (dirty, oldDirty^._1)
     scrGlobals.scrDirty %= (\v -> v & x1 .~ 9999
                                     & x2 .~ (-9999)
                                     & y1 .~ 9999
                                     & y2 .~ (-9999))
     proceedTileClear conCurrent dirty oldDirty vidDef

proceedTileClear :: Float -> DirtyT -> (DirtyT, DirtyT) -> VidDefT -> Quake ()
proceedTileClear conCurrent dirty oldDirty vidDef =
  unless ((clear^.y2) <= (clear^.y1)) $
    do vrect <- use (globals.gScrVRect)
       renderer <- use (globals.gRenderer)
       maybe rendererError (void . startClear vrect) renderer
  where rendererError = Com.fatalError "SCR.proceedTileClear renderer is Nothing"
        startClear vrect renderer =
          clearAbove renderer (vrect^.vrY) clear
            >>= clearBelow renderer ((vrect^.vrY) + (vrect^.vrHeight) - 1)
            >>= clearLeft renderer (vrect^.vrX)
            >>= clearRight renderer ((vrect^.vrX) + (vrect^.vrWidth) - 1)
        tmpClear = calcClear dirty oldDirty 0 2
        tmp = truncate (conCurrent * fromIntegral (vidDef^.vdHeight)) :: Int
        clear | tmp > (tmpClear^.y1) = tmpClear & y1 .~ tmp
              | otherwise = tmpClear

calcClear :: DirtyT -> (DirtyT, DirtyT) -> Int -> Int -> DirtyT
calcClear clear oldDirty idx maxIdx
  | idx >= maxIdx = clear
  | otherwise =
      let oldDirtyValue = if idx == 0 then oldDirty^._1 else oldDirty^._2
          xx1 = if (oldDirtyValue^.x1) < (clear^.x1) then oldDirtyValue^.x1 else clear^.x1
          xx2 = if (oldDirtyValue^.x2) > (clear^.x2) then oldDirtyValue^.x2 else clear^.x2
          yy1 = if (oldDirtyValue^.y1) < (clear^.y1) then oldDirtyValue^.y1 else clear^.y1
          yy2 = if (oldDirtyValue^.y2) > (clear^.y2) then oldDirtyValue^.y2 else clear^.y2
          newClear = DirtyT { _x1 = xx1, _x2 = xx2, _y1 = yy1, _y2 = yy2 }
      in calcClear newClear oldDirty (idx + 1) maxIdx

clearAbove :: Renderer -> Int -> DirtyT -> Quake DirtyT
clearAbove renderer top clear
  | (clear^.y1) < top =
      do (renderer^.rRefExport.reDrawTileClear) (clear^.x1) (clear^.y1) ((clear^.x2) - (clear^.x1) + 1) (idx - (clear^.y1) + 1) "backtile"
         return (clear & y1 .~ top)
  | otherwise = return clear
  where idx | (clear^.y2) < (top - 1) = clear^.y2
            | otherwise = top - 1

clearBelow :: Renderer -> Int -> DirtyT -> Quake DirtyT
clearBelow renderer bottom clear
  | (clear^.y2) > bottom =
      do (renderer^.rRefExport.reDrawTileClear) (clear^.x1) idx ((clear^.x2) - (clear^.x1) + 1) ((clear^.y2) - idx + 1) "backtile"
         return (clear & y2 .~ bottom)
  | otherwise = return clear
  where idx | (clear^.y1) > bottom + 1 = clear^.y1
            | otherwise = bottom + 1

clearLeft :: Renderer -> Int -> DirtyT -> Quake DirtyT
clearLeft renderer left clear
  | (clear^.x1) < left =
      do (renderer^.rRefExport.reDrawTileClear) (clear^.x1) (clear^.y1) (idx - (clear^.x1) + 1) ((clear^.y2) - (clear^.y1) + 1) "backtile"
         return (clear & x1 .~ left)
  | otherwise = return clear
  where idx | (clear^.x2) < left - 1 = clear^.x2
            | otherwise = left - 1

clearRight :: Renderer -> Int -> DirtyT -> Quake DirtyT
clearRight renderer right clear
  | (clear^.x2) > right =
      do (renderer^.rRefExport.reDrawTileClear) idx (clear^.y1) ((clear^.x2) - idx + 1) ((clear^.y2) - (clear^.y1) + 1) "backtile"
         return (clear & x2 .~ right)
  | otherwise = return clear
  where idx | (clear^.x1) > right + 1 = clear^.x1
            | otherwise = right + 1

drawStats :: Quake ()
drawStats =
  do statsStr <- preuse (globals.gCl.csConfigStrings.ix Constants.csStatusBar)
     maybe statsStrError executeLayoutString statsStr
  where statsStrError = Com.fatalError "SCR.drawStats statsStr is Nothing"

drawLayout :: Quake ()
drawLayout = error "SCR.drawLayout" -- TODO

drawNet :: NetChanT -> Quake ()
drawNet nc =
  unless ((nc^.ncOutgoingSequence) - (nc^.ncIncomingAcknowledged) < Constants.cmdBackup - 1) $
    do vrect <- use (globals.gScrVRect)
       renderer <- use (globals.gRenderer)
       maybe rendererError (doDrawNet vrect) renderer
  where rendererError = Com.fatalError "SCR.drawNet renderer is Nothing"
        doDrawNet vrect renderer =
          (renderer^.rRefExport.reDrawPic) ((vrect^.vrX) + 64) (vrect^.vrY) "net"

checkDrawCenterString :: Quake ()
checkDrawCenterString =
  do frameTime <- use (globals.gCls.csFrameTime)
     scrGlobals.scrCenterTimeOff -= frameTime
     centerString =<< use (scrGlobals.scrCenterTimeOff)
  where centerString centerTimeOff =
          unless (centerTimeOff <= 0) drawCenterString

drawCenterString :: Quake ()
drawCenterString = error "SCR.drawCenterString" -- TODO

drawFPS :: CVarT -> Quake ()
drawFPS fps
  | (fps^.cvValue) > 0 =
      do checkFPSModified
         join (liftA2 (checkTimeDiff fps) realTime lastTime)
         join (liftA2 doDrawFPS vidDef fpsValue)
  | fps^.cvModified =
      do CVar.update (fps & cvModified .~ False)
         CVar.setValueI "cl_maxfps" 90
  | otherwise = return ()
  where checkFPSModified
          | fps^.cvModified =
              do CVar.update fps { _cvModified = False }
                 CVar.setValueI "cl_maxfps" 1000
          | otherwise = return ()
        lastTime = use (scrGlobals.scrLastTime)
        realTime = use (globals.gCls.csRealTime)
        vidDef = use (globals.gVidDef)
        fpsValue = use (scrGlobals.scrFPSValue)

doDrawFPS :: VidDefT -> B.ByteString -> Quake ()
doDrawFPS vidDef fpsValue = drawFPSByChar x 0 fpsValue
  where x = (vidDef^.vdWidth) - 8 * (B.length fpsValue) - 2

checkTimeDiff :: CVarT -> Int -> Int -> Quake ()
checkTimeDiff fps realTime lastTime
  | diff > truncate ((fps^.cvValue) * 1000) =
      do frameCount <- use (globals.gCls.csFrameCount)
         lastFrames <- use (scrGlobals.scrLastFrames)
         updateFrameStats diff realTime frameCount lastFrames
  | otherwise = return ()
  where diff = realTime - lastTime

updateFrameStats :: Int -> Int -> Int -> Int -> Quake ()
updateFrameStats diff realTime frameCount lastFrames =
  do scrGlobals.scrLastFrames .= frameCount
     scrGlobals.scrLastTime .= realTime
     scrGlobals.scrFPSValue .= fpsStr
  where fpsValue = fromIntegral (frameCount - lastFrames) * 100000 / fromIntegral diff / 100.0 :: Float
        fpsStr = encode fpsValue `B.append` " fps"

drawFPSByChar :: Int -> Int -> B.ByteString -> Quake ()
drawFPSByChar x idx str
  | idx >= B.length str = return ()
  | otherwise =
      do renderer <- use (globals.gRenderer)
         maybe rendererError doDrawFPSByChar renderer
  where rendererError = Com.fatalError "SCR.drawFPSByChar renderer is Nothing"
        doDrawFPSByChar renderer =
          do (renderer^.rRefExport.reDrawChar) x 2 (ord (BC.index str idx))
             drawFPSByChar (x + 8) (idx + 1) str

drawPause :: Quake ()
drawPause =
  do showPause <- scrShowPauseCVar
     paused <- pausedCVar
     unless ((showPause^.cvValue) == 0 || (paused^.cvValue) == 0) $
       do vidDef <- use (globals.gVidDef)
          renderer <- use (globals.gRenderer)
          maybe rendererError (getDimAndDraw vidDef) renderer
  where rendererError = Com.fatalError "SCR.drawPause renderer is Nothing"
        getDimAndDraw vidDef renderer =
          do dim <- (renderer^.rRefExport.reDrawGetPicSize) "pause"
             maybe dimError (doDrawPause vidDef renderer) dim
        dimError = Com.fatalError "SCR.drawPause dim is Nothing"
        doDrawPause vidDef renderer (width, _) =
          (renderer^.rRefExport.reDrawPic) (((vidDef^.vdWidth) - width) `div` 2) ((vidDef^.vdHeight) `div` 2 + 8) "pause"

drawConsole :: Quake ()
drawConsole =
  do Console.checkResize =<< use (globals.gVidDef)
     state <- use (globals.gCls.csState)
     refreshPrepped <- use (globals.gCl.csRefreshPrepped)
     doDrawConsole state refreshPrepped

doDrawConsole :: Int -> Bool -> Quake ()
doDrawConsole state refreshPrepped
  | state `elem` [Constants.caDisconnected, Constants.caConnecting] =
      Console.drawConsole 1 =<< use (globals.gVidDef)
  | state /= Constants.caActive || not refreshPrepped =
      do vidDef <- use (globals.gVidDef)
         Console.drawConsole 0.5 vidDef
         renderer <- use (globals.gRenderer)
         maybe rendererError (renderConsole vidDef) renderer
  | otherwise = checkConCurrent =<< use (scrGlobals.scrConCurrent)
  where rendererError = Com.fatalError "SCR.doDrawConsole renderer is Nothing"
        renderConsole vidDef renderer =
          (renderer^.rRefExport.reDrawFill) 0 ((vidDef^.vdHeight) `div` 2) (vidDef^.vdWidth) ((vidDef^.vdHeight) `div` 2) 0
        checkConCurrent conCurrent
          | conCurrent /= 0 = Console.drawConsole conCurrent =<< use (globals.gVidDef)
          | otherwise =
              do keyDest <- use (globals.gCls.csKeyDest)
                 when (keyDest `elem` [Constants.keyGame, Constants.keyMessage]) $
                   Console.drawNotify

drawCinematic :: Quake Bool
drawCinematic = error "SCR.drawCinematic" -- TODO

drawLoading :: Renderer -> Quake ()
drawLoading renderer =
  do loading <- use (scrGlobals.scrDrawLoading)
     unless (loading == 0) $
       do scrGlobals.scrDrawLoading .= 0
          vidDef <- use (globals.gVidDef)
          dim <- (renderer^.rRefExport.reDrawGetPicSize) "loading"
          maybe dimError (doDrawLoading vidDef) dim
  where dimError = Com.fatalError "SCR.drawLoading dim is Nothing"
        doDrawLoading vidDef (width, height) =
          (renderer^.rRefExport.reDrawPic) (((vidDef^.vdWidth) - width) `div` 2) (((vidDef^.vdHeight) - height) `div` 2) "loading"

executeLayoutString :: B.ByteString -> Quake ()
executeLayoutString str =
  do skip <- shouldSkip
     unless skip $
       parseLayoutString 0 0 3 0
  where shouldSkip =
          do state <- use (globals.gCls.csState)
             refreshPrepped <- use (globals.gCl.csRefreshPrepped)
             return (state /= Constants.caActive || not refreshPrepped || B.null str)

parseLayoutString :: Int -> Int -> Int -> Int -> Quake ()
parseLayoutString = error "SCR.parseLayoutString" -- TODO

{-
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
                                -}

readNextFrame :: Quake (Maybe (UV.Vector Word8))
readNextFrame =
  do cinematicFileHandle <- use (globals.gCl.csCinematicFile)
     maybe cinematicFileHandleError proceedReadNextFrame cinematicFileHandle
  where cinematicFileHandleError =
          do Com.fatalError "SCR.readNextFrame cinematicFileHandle is Nothing"
             return Nothing
        proceedReadNextFrame cinematicFileHandle =
          do command <- request (io (BL.hGet cinematicFileHandle 4))
             doReadNextFrame cinematicFileHandle (runGet getInt command)

doReadNextFrame :: Handle -> Int -> Quake (Maybe (UV.Vector Word8))
doReadNextFrame cinematicFileHandle command
  | command == 2 = return Nothing -- last frame marker
  | otherwise =
      do when (command == 1) $
           do palette <- request (io (B.hGet cinematicFileHandle 768))
              globals.gCl.csCinematicPalette .= palette
              globals.gCl.csCinematicPaletteActive .= False -- dubious... exposes an edge case
         size <- fmap (runGet getInt) (request (io (BL.hGet cinematicFileHandle 4)))
         checkSize size
         compressed <- getCompressedData size
         join (liftA2 (readSounds cinematicFileHandle) (use (globals.gCl.csCinematicFrame)) (use (scrGlobals.scrCin)))
         pic <- huff1Decompress compressed size
         globals.gCl.csCinematicFrame += 1
         return (Just pic)
  where checkSize size
          | size > 0x20000 || size < 1 =
              Com.comError Constants.errDrop ("Bad compressed frame size:" `B.append` encode size)
          | otherwise = return ()
        getCompressedData size =
          do compressed <- request (io (B.hGet cinematicFileHandle size))
             return (compressed `B.append` (B.replicate (0x20000 - size) 0))

readSounds :: Handle -> Int -> CinematicsT -> Quake ()
readSounds cinematicFileHandle cinematicFrame cin =
  do position <- request (io (hTell cinematicFileHandle))
     S.rawSamples count (cin^.cSRate) (cin^.cSWidth) (cin^.cSChannels) cinematicFileHandle
     request (io (hSeek cinematicFileHandle AbsoluteSeek (fromIntegral (fromIntegral position + count * (cin^.cSWidth) * (cin^.cSChannels)))))
  where start = cinematicFrame * (cin^.cSRate) `div` 14
        end = (cinematicFrame + 1) * (cin^.cSRate) `div` 14
        count = end - start

stopCinematic :: Quake ()
stopCinematic = doStopCinematic =<< use (scrGlobals.scrCin.cRestartSound)
  where doStopCinematic False = return ()
        doStopCinematic True =
          do globals.gCl.csCinematicTime .= 0
             scrGlobals.scrCin %= (\v -> v & cPic .~ Nothing
                                           & cPicPending .~ Nothing
                                           & cHNodes1 .~ Nothing)
             checkCinematicPalette =<< use (globals.gCl.csCinematicPaletteActive)
             globals.gCl.csCinematicFile .= Nothing -- IMPROVE: research if need to close handle?
             S.disableStreaming
             scrGlobals.scrCin.cRestartSound .= False
        checkCinematicPalette False = return ()
        checkCinematicPalette True =
          do renderer <- use (globals.gRenderer)
             maybe rendererError unsetCinematicPalette renderer
        rendererError = Com.fatalError "SCR.stopCinematic renderer is Nothing"
        unsetCinematicPalette renderer =
          do (renderer^.rRefExport.reCinematicSetPalette) Nothing
             globals.gCl.csCinematicPaletteActive .= False

centerPrint :: B.ByteString -> Quake ()
centerPrint = error "SCR.centerPrint" -- TODO

playCinematic :: B.ByteString -> Quake ()
playCinematic arg =
  do globals.gCl.csCinematicFrame .= 0
     proceedPlayCinematic
  where proceedPlayCinematic
          | ".pcx" `BC.isSuffixOf` arg = staticPCXImage ("pics/" `B.append` arg)
          | otherwise = playVideo ("video/" `B.append` arg)

staticPCXImage :: B.ByteString -> Quake ()
staticPCXImage name = error "SCR.staticPCXImage" -- TODO

playVideo :: B.ByteString -> Quake ()
playVideo name =
  do cinematicFile <- FS.fOpenFile name
     globals.gCl.csCinematicFile .= cinematicFile
     maybe finishPlayVideo doPlayVideo cinematicFile
  where finishPlayVideo =
          do finishCinematic
             globals.gCl.csCinematicTime .= 0

doPlayVideo :: Handle -> Quake ()
doPlayVideo cinematicFileHandle =
  do endLoadingPlaque
     globals.gCls.csState .= Constants.caActive
     cinematicData <- request (io (BL.hGet cinematicFileHandle 20))
     let (width, height, sRate, sWidth, sChannels) = runGet getCinematicData cinematicData
     scrGlobals.scrCin %= (\v -> v & cWidth .~ width
                                   & cHeight .~ height
                                   & cSRate .~ sRate
                                   & cSWidth .~ sWidth
                                   & cSChannels .~ sChannels)
     huff1TableInit
     scrGlobals.scrCin.cRestartSound .= True
     globals.gCl.csCinematicFrame .= 0
     pic <- readNextFrame
     scrGlobals.scrCin.cPic .= pic
     ms <- Timer.milliseconds
     globals.gCl.csCinematicTime .= ms
  where getCinematicData = (,,,,) <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt

debugGraph :: Float -> Int -> Quake ()
debugGraph _ _ = request (io (putStrLn "SCR.debugGraph IMPLEMENT ME!"))

huff1TableInit :: Quake ()
huff1TableInit =
  do cinematicFileHandle <- use (globals.gCl.csCinematicFile)
     maybe cinematicFileHandleError proceedTableInit cinematicFileHandle
  where cinematicFileHandleError = Com.fatalError "SCR.huff1TableInit cinematicFileHandle is Nothing"
        proceedTableInit cinematicFileHandle =
          do allCounts <- request (io (B.hGet cinematicFileHandle (256 * 256)))
             let (hNodes, numHNodes) = calcNodes allCounts
             scrGlobals.scrCin.cHNodes1 .= Just hNodes
             scrGlobals.scrCin.cNumHNodes1 .= numHNodes
        calcNodes allCounts = runST $
          do hNodes <- MUV.replicate (256 * 256 * 2) 0
             numHNodes <- UV.generateM 256 (tableInit hNodes allCounts)
             hNodes' <- UV.unsafeFreeze hNodes
             return (hNodes', numHNodes)

-- TODO: find all code entires with MV.new (or unsafeThaw) and check if it
-- can be turned to Unboxed version
tableInit :: MUV.STVector s Int -> B.ByteString -> Int -> ST s Int
tableInit hNodes allCounts idx =
  do hCount <- UV.unsafeThaw (UV.generate 512 (\i -> if i < 256 then fromIntegral (counts `B.index` i) else 0))
     hUsed <- MUV.replicate 512 0
     initNodes hNodes hUsed hCount (idx * 256 * 2) 256
  where counts = B.take 256 (B.drop (idx * 256) allCounts)

initNodes :: MUV.STVector s Int -> MUV.STVector s Int -> MUV.STVector s Int -> Int -> Int -> ST s Int
initNodes hNodes hUsed hCount nodeBase numHNodes
  | numHNodes >= 511 = return numHNodes
  | otherwise =
      do b1 <- smallestNode1 hUsed hCount numHNodes
         MUV.write hNodes index b1
         checkB1 b1
  where index = nodeBase + (numHNodes - 256) * 2
        checkB1 b1
          | b1 == -1 = return numHNodes
          | otherwise =
              do MUV.write hUsed b1 1
                 b2 <- smallestNode1 hUsed hCount numHNodes
                 checkB2 b1 b2
        checkB2 b1 b2
          | b2 == -1 = return numHNodes
          | otherwise =
              do MUV.write hUsed b2 1
                 v1 <- MUV.read hCount b1
                 v2 <- MUV.read hCount b2
                 MUV.write hCount numHNodes (v1 + v2)
                 initNodes hNodes hUsed hCount nodeBase (numHNodes + 1)

smallestNode1 :: MUV.STVector s Int -> MUV.STVector s Int -> Int -> ST s Int
smallestNode1 hUsed hCount numHNodes = findBestNode 99999999 (-1) 0
  where findBestNode best bestNode idx
          | idx >= numHNodes = return bestNode
          | otherwise =
              do u <- MUV.read hUsed idx
                 c <- MUV.read hCount idx
                 checkBest best bestNode idx u c
        checkBest best bestNode idx u c
          | u /= 0 = findBestNode best bestNode (idx + 1)
          | c == 0 = findBestNode best bestNode (idx + 1)
          | c < best = findBestNode c idx (idx + 1)
          | otherwise = findBestNode best bestNode (idx + 1)

huff1Decompress :: B.ByteString -> Int -> Quake (UV.Vector Word8)
huff1Decompress frame size =
  do hNodes <- use (scrGlobals.scrCin.cHNodes1)
     numHNodes <- use (scrGlobals.scrCin.cNumHNodes1)
     maybe hNodesError (return . proceedHuff1Decompress numHNodes) hNodes
  where a = fromIntegral (frame `B.index` 0) :: Int
        b = (fromIntegral (frame `B.index` 1) :: Int) `shiftL` 8
        c = (fromIntegral (frame `B.index` 2) :: Int) `shiftL` 16
        d = (fromIntegral (frame `B.index` 3) :: Int) `shiftL` 24
        count = a .|. b .|. c .|. d
        index = (-256) * 2
        hNodesError =
          do Com.fatalError "SCR.huff1Decompress hNodes is Nothing"
             return UV.empty
        proceedHuff1Decompress numHNodes hNodes = runST $
          do out <- MUV.new count
             decompress frame hNodes numHNodes (numHNodes UV.! 0) 4 0 out index count

decompress :: B.ByteString -> UV.Vector Int -> UV.Vector Int -> Int -> Int -> Int -> MUV.STVector s Word8 -> Int -> Int -> ST s (UV.Vector Word8)
decompress frame hNodes numHNodes nodeNum inIdx outIdx out index count
  | count == 0 = UV.unsafeFreeze out -- TODO: Decompression overread by ... error ...
  | otherwise =
      do (index', count', nodeNum', outIdx') <- processByte hNodes numHNodes nodeNum (frame `B.index` inIdx) outIdx out index count 0 8
         decompress frame hNodes numHNodes nodeNum' (inIdx + 1) outIdx' out index' count'

processByte :: UV.Vector Int -> UV.Vector Int -> Int -> Word8 -> Int -> MUV.STVector s Word8 -> Int -> Int -> Int -> Int -> ST s (Int, Int, Int, Int)
processByte hNodes numHNodes nodeNum inByte outIdx out index count idx maxIdx
  | idx >= maxIdx = return (index, count, nodeNum, outIdx)
  | nodeNum < 256 =
      do let index' = (-256) * 2 + (nodeNum `shiftL` 9)
             count' = count - 1
         MUV.write out outIdx (fromIntegral nodeNum)
         case count' == 0 of
           True -> return (index', count', nodeNum, outIdx + 1)
           False ->
             do let nodeNum' = hNodes UV.! (index' + (numHNodes UV.! nodeNum) * 2 + fromIntegral (inByte .&. 1))
                processByte hNodes numHNodes nodeNum' (inByte `shiftR` 1) (outIdx + 1) out index' count' (idx + 1) maxIdx
  | otherwise =
      do let nodeNum' = hNodes UV.! (index + nodeNum * 2 + fromIntegral (inByte .&. 1))
         processByte hNodes numHNodes nodeNum' (inByte `shiftR` 1) outIdx out index count (idx + 1) maxIdx

touchPics :: Quake ()
touchPics = error "SCR.touchPics" -- TODO