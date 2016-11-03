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
import           Client.ClientInfoT
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInv as CLInv
import qualified Client.Console as Console
import           Client.ConsoleT
import           Client.DirtyT
import           Client.FrameT
import qualified Client.Menu as Menu
import           Client.RefDefT
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
import qualified Util.Lib as Lib

import           Control.Applicative (liftA, liftA2)
import           Control.Lens (preuse, use, ix, (^.), (.=), (-=), (%=), (+=), (&), (.~), _1, _2)
import           Control.Monad (when, unless, void, join)
import           Control.Monad.ST (ST, runST)
import           Data.Binary.Get (runGet)
import           Data.Bits (complement, shiftR, shiftL, (.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Int (Int16)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Data.Word (Word8)
import           Linear (V3(..), _y)
import           System.IO (Handle, hTell, hSeek, SeekMode(AbsoluteSeek))
import           Text.Printf (printf)

sbNums1 :: V.Vector B.ByteString
sbNums1 = V.fromList [ "num_0" , "num_1" , "num_2"
                     , "num_3" , "num_4" , "num_5"
                     , "num_6" , "num_7" , "num_8"
                     , "num_9" , "num_minus"
                     ]

sbNums2 :: V.Vector B.ByteString
sbNums2 = V.fromList [ "anum_0" , "anum_1" , "anum_2"
                     , "anum_3" , "anum_4" , "anum_5"
                     , "anum_6" , "anum_7" , "anum_8"
                     , "anum_9" , "anum_minus"
                     ]

charWidth :: Int
charWidth = 16

statMinus :: Int
statMinus = 10 -- num frame for '-' stats digit

statLayouts :: Int
statLayouts = 13

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
timeRefreshF = XCommandT "SCR.timeRefreshF" $
  do state <- use (globals.gCls.csState)
     renderer <- use (globals.gRenderer)
     maybe rendererError (timeRefresh state) renderer
  where rendererError = Com.fatalError "SCR.timeRefreshF renderer is Nothing"

timeRefresh :: Int -> Renderer -> Quake ()
timeRefresh state renderer
  | state == Constants.caActive =
      do start <- Timer.milliseconds
         c <- Cmd.argc
         renderFrame renderer c
         stop <- Timer.milliseconds
         printTime start stop
  | otherwise = return ()

renderFrame :: Renderer -> Int -> Quake ()
renderFrame renderer c
  | c == 2 =
      do (renderer^.rRefExport.reBeginFrame) 0
         mapM_ noPageFlipRender [0..127]
         renderer^.rRefExport.reEndFrame
  | otherwise = mapM_ pageFlipRender [0..127]
  where noPageFlipRender idx =
          do globals.gCl.csRefDef.rdViewAngles._y .= idx / 128.0 * 360.0
             (renderer^.rRefExport.reRenderFrame) =<< use (globals.gCl.csRefDef)
        pageFlipRender idx =
          do globals.gCl.csRefDef.rdViewAngles._y .= idx / 128.0 * 360.0
             (renderer^.rRefExport.reBeginFrame) 0
             (renderer^.rRefExport.reRenderFrame) =<< use (globals.gCl.csRefDef)
             renderer^.rRefExport.reEndFrame

printTime :: Int -> Int -> Quake ()
printTime start stop =
  Com.printf (B.concat [encode time, " seconds (", encode (128.0 / time), " fps)\n"])
  where time = fromIntegral (stop - start) / 1000 :: Float

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
skyF = XCommandT "SCR.skyF" $
  sky =<< Cmd.argc

sky :: Int -> Quake ()
sky c
  | c < 2 = Com.printf "Usage: sky <basename> <rotate> <axis x y z>\n"
  | otherwise =
      do rotate <- if c > 2 then fmap Lib.atof (Cmd.argv 2) else return 0
         axis <- if c == 6
                   then V3 <$> (Lib.atof <$> Cmd.argv 3)
                           <*> (Lib.atof <$> Cmd.argv 4)
                           <*> (Lib.atof <$> Cmd.argv 5)
                   else return (V3 0 0 1)
         v <- Cmd.argv 1
         renderer <- use (globals.gRenderer)
         maybe rendererError (setSky v rotate axis) renderer
  where setSky v rotate axis renderer = (renderer^.rRefExport.reSetSky) v rotate axis
        rendererError = Com.fatalError "SCR.sky renderer is Nothing"

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
drawLayout =
  do stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
     case stats UV.!? statLayouts of
       Nothing -> return ()
       Just v -> when (v /= 0) (executeLayoutString =<< use (globals.gCl.csLayout))

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
drawCenterString =
  do str <- use (scrGlobals.scrCenterString)
     unless (B.null str) $
       do centerLines <- use (scrGlobals.scrCenterLines)
          vidDef <- use (globals.gVidDef)
          renderer <- use (globals.gRenderer)
          maybe rendererError (drawLines str vidDef 9999 (calcY centerLines vidDef) 0) renderer
  where calcY centerLines vidDef
          | centerLines <= 4 = truncate (fromIntegral (vidDef^.vdHeight) * 0.35 :: Float)
          | otherwise = 48
        rendererError = Com.fatalError "SCR.drawCenterString renderer is Nothing"

drawLines :: B.ByteString -> VidDefT -> Int -> Int -> Int -> Renderer -> Quake ()
drawLines str vidDef remaining y start renderer =
  do addDirtyPoint x y
     (remaining', x') <- drawLine renderer str remaining start x y 0 len
     addDirtyPoint x' (y + 8)
     when (start' < B.length str) $
       drawLines str vidDef remaining' (y + 8) (start' + 1) renderer
  where len = scanLineWidth str start 0 40
        x = ((vidDef^.vdWidth) - len * 8) `div` 2
        start' = findEndOfLine str start

scanLineWidth :: B.ByteString -> Int -> Int -> Int -> Int
scanLineWidth str s idx maxIdx
  | idx >= maxIdx || (s + idx) >= B.length str = idx
  | str `BC.index` (s + idx) == '\n' = idx
  | otherwise = scanLineWidth str s (idx + 1) maxIdx

drawLine :: Renderer -> B.ByteString -> Int -> Int -> Int -> Int -> Int -> Int -> Quake (Int, Int)
drawLine renderer str remaining start x y idx maxIdx
  | idx >= maxIdx = return (remaining, x)
  | otherwise =
      do (renderer^.rRefExport.reDrawChar) x y (ord (str `BC.index` (start + idx)))
         if remaining == 0
           then return (0, x)
           else drawLine renderer str (remaining - 1) start (x + 8) y (idx + 1) maxIdx

-- IMPROVE: use some built in function to do this?
findEndOfLine :: B.ByteString -> Int -> Int
findEndOfLine str idx
  | idx >= B.length str || str `BC.index` idx == '\n' = idx
  | otherwise = findEndOfLine str (idx + 1)

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
              do CVar.update (fps & cvModified .~ False)
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
drawCinematic =
  do cinematicTime <- use (globals.gCl.csCinematicTime)
     keyDest <- use (globals.gCls.csKeyDest)
     renderer <- use (globals.gRenderer)
     maybe rendererError (proceedDrawCinematic cinematicTime keyDest) renderer
  where rendererError =
          do Com.fatalError "SCR.drawCinematic renderer is Nothing"
             return False

proceedDrawCinematic :: Int -> Int -> Renderer -> Quake Bool
proceedDrawCinematic cinematicTime keyDest renderer
  | cinematicTime <= 0 = return False
  | keyDest == Constants.keyMenu =
      do (renderer^.rRefExport.reCinematicSetPalette) Nothing
         globals.gCl.csCinematicPaletteActive .= False
         return True
  | otherwise =
      do cinematicPaletteActive <- use (globals.gCl.csCinematicPaletteActive)
         unless cinematicPaletteActive $
           do cinematicPalette <- use (globals.gCl.csCinematicPalette)
              (renderer^.rRefExport.reCinematicSetPalette) (Just cinematicPalette)
              globals.gCl.csCinematicPaletteActive .= True
         cin <- use (scrGlobals.scrCin)
         maybe (return True) (doDraw cin) (cin^.cPic)
  where doDraw cin picture =
          do vidDef <- use (globals.gVidDef)
             (renderer^.rRefExport.reDrawStretchRaw) 0 0 (vidDef^.vdWidth) (vidDef^.vdHeight) (cin^.cWidth) (cin^.cHeight) picture
             return True

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
       parseLayoutString str 0 0 3 0
  where shouldSkip =
          do state <- use (globals.gCls.csState)
             refreshPrepped <- use (globals.gCl.csRefreshPrepped)
             return (state /= Constants.caActive || not refreshPrepped || B.null str)

parseLayoutString :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
parseLayoutString str x y width idx
  | idx >= B.length str = return ()
  | otherwise =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         maybe (parseLayoutString str x y width newIdx) (parseWithToken newIdx) mToken
  where parseWithToken newIdx token =
          do (x', y', width', finalIdx) <- processToken str x y width newIdx token
             parseLayoutString str x' y' width' finalIdx

processToken :: B.ByteString -> Int -> Int -> Int -> Int -> B.ByteString -> Quake (Int, Int, Int, Int)
processToken str x y width idx token
  | token == "xl" = 
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         maybe (tokenError "xl") (\tkn -> return (Lib.atoi tkn, y, width, newIdx)) mToken
  | token == "xr" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         w <- use (globals.gVidDef.vdWidth)
         maybe (tokenError "xr") (\tkn -> return (w + Lib.atoi tkn, y, width, newIdx)) mToken
  | token == "xv" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         w <- use (globals.gVidDef.vdWidth)
         maybe (tokenError "xv") (\tkn -> return (w `div` 2 - 160 + Lib.atoi tkn, y, width, newIdx)) mToken
  | token == "yt" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         maybe (tokenError "yt") (\tkn -> return (x, Lib.atoi tkn, width, newIdx)) mToken
  | token == "yb" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         h <- use (globals.gVidDef.vdHeight)
         maybe (tokenError "yb") (\tkn -> return (x, h + Lib.atoi tkn, width, newIdx)) mToken
  | token == "yv" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         h <- use (globals.gVidDef.vdHeight)
         maybe (tokenError "yv") (\tkn -> return (x, h `div` 2 - 120 + Lib.atoi tkn, width, newIdx)) mToken
  | token == "pic" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         maybe (tokenError "pic") (processPicToken x y width stats newIdx) mToken
  | token == "client" =
      do vidDef <- use (globals.gVidDef)
         (mToken1, newIdx1) <- Com.parse str (B.length str) idx
         (mToken2, newIdx2) <- Com.parse str (B.length str) newIdx1
         (mToken3, newIdx3) <- Com.parse str (B.length str) newIdx2
         (mToken4, newIdx4) <- Com.parse str (B.length str) newIdx3
         (mToken5, newIdx5) <- Com.parse str (B.length str) newIdx4
         (mToken6, newIdx6) <- Com.parse str (B.length str) newIdx5
         processClientToken width vidDef newIdx6 mToken1 mToken2 mToken3 mToken4 mToken5 mToken6
  | token == "ctf" =
      do vidDef <- use (globals.gVidDef)
         (mToken1, newIdx1) <- Com.parse str (B.length str) idx
         (mToken2, newIdx2) <- Com.parse str (B.length str) newIdx1
         (mToken3, newIdx3) <- Com.parse str (B.length str) newIdx2
         (mToken4, newIdx4) <- Com.parse str (B.length str) newIdx3
         (mToken5, newIdx5) <- Com.parse str (B.length str) newIdx4
         processCtfToken width vidDef newIdx5 mToken1 mToken2 mToken3 mToken4 mToken5
  | token == "picn" =
      do (mToken1, newIdx1) <- Com.parse str (B.length str) idx
         renderer <- use (globals.gRenderer)
         processPicnToken x y width mToken1 newIdx1 renderer
  | token == "num" =
      do (mToken1, newIdx1) <- Com.parse str (B.length str) idx
         (mToken2, newIdx2) <- Com.parse str (B.length str) newIdx1
         processNumToken x y newIdx2 mToken1 mToken2
  | token == "hnum" =
      do stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         serverFrame <- use (globals.gCl.csFrame.fServerFrame)
         processHnumToken x y idx serverFrame (stats UV.! Constants.statHealth) (stats UV.! Constants.statFlashes)
  | token == "anum" =
      do stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         serverFrame <- use (globals.gCl.csFrame.fServerFrame)
         processAnumToken x y idx serverFrame (stats UV.! Constants.statAmmo) (stats UV.! Constants.statFlashes)
  | token == "rnum" =
      do stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         processRnumToken x y idx (stats UV.! Constants.statArmor) (stats UV.! Constants.statFlashes)
  | token == "stat_string" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         configStrings <- use (globals.gCl.csConfigStrings)
         processStatStringToken stats configStrings x y width newIdx mToken
  | token == "cstring" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         drawHUDString mToken x y 320 0
         return (x, y, width, newIdx)
  | token == "string" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         processStringToken x y width newIdx mToken
  | token == "cstring2" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         drawHUDString mToken x y 320 0x80
         return (x, y, width, newIdx)
  | token == "string2" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         processString2Token x y width newIdx mToken
  | token == "if" =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
         processIfToken str stats x y width newIdx mToken -- TODO: make sure we pass arguments in the same order for all functions here (like stats)
  | otherwise = error "ohno" >> undefined -- TODO
  where tokenError tkn =
          do Com.fatalError ("SCR.processToken Com.parse returned Nothing for token " `B.append` tkn)
             return (0, 0, 0, 0)

processStatStringToken :: UV.Vector Int16 -> V.Vector B.ByteString -> Int -> Int -> Int -> Int -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processStatStringToken stats configStrings x y width newIdx (Just token) =
  do when (index < 0 || index >= Constants.maxConfigStrings) $
       Com.comError Constants.errDrop "Bad stat_string index"
     when (csIndex < 0 || csIndex >= Constants.maxConfigStrings) $
       Com.comError Constants.errDrop "Bad stat_string index"
     Console.drawString x y (configStrings V.! csIndex)
     return (x, y, width, newIdx)
  where index = Lib.atoi token
        csIndex = fromIntegral (stats UV.! index)
processStatStringToken _ _ _ _ _ _ Nothing = processingError "SCR.processStatStringToken token is Nothing"

processIfToken :: B.ByteString -> UV.Vector Int16 -> Int -> Int -> Int -> Int -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processIfToken str stats x y width newIdx (Just token)
  | value == 0 =
      do finalIdx <- skipToEndIf str newIdx
         return (x, y, width, finalIdx)
  | otherwise = return (x, y, width, newIdx)
  where index = Lib.atoi token
        value = stats UV.! index
processIfToken _ _ _ _ _ _ Nothing = processingError "SCR.processIfToken token is Nothing"

skipToEndIf :: B.ByteString -> Int -> Quake Int
skipToEndIf str idx
  | idx >= B.length str = return idx
  | otherwise =
      do (mToken, newIdx) <- Com.parse str (B.length str) idx
         maybe (skipToEndIf str newIdx) (checkToken newIdx) mToken
  where checkToken newIdx token
          | token == "endif" = return newIdx
          | otherwise = skipToEndIf str newIdx

processStringToken :: Int -> Int -> Int -> Int -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processStringToken x y width newIdx (Just token) =
  do Console.drawString x y token
     return (x, y, width, newIdx)
processStringToken _ _ _ _ Nothing = processingError "SCR.processStringToken token is Nothing"

processString2Token :: Int -> Int -> Int -> Int -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processString2Token x y width newIdx (Just token) =
  do Console.drawAltString x y token
     return (x, y, width, newIdx)
processString2Token _ _ _ _ Nothing = processingError "SCR.processString2Token token is Nothing"

processPicToken :: Int -> Int -> Int -> UV.Vector Int16 -> Int -> B.ByteString -> Quake (Int, Int, Int, Int)
processPicToken x y width stats newIdx token =
  maybe valueError proceedProcessPicToken (stats UV.!? (Lib.atoi token))
  where proceedProcessPicToken value =
          do when (fromIntegral value >= Constants.maxImages) $
               Com.comError Constants.errDrop "Pic >= MAX_IMAGES"
             configStrings <- use (globals.gCl.csConfigStrings)
             maybe configStringError doProcessPicToken (configStrings V.!? (Constants.csImages + fromIntegral value))
        doProcessPicToken cs =
          do unless (B.null cs) $
               do addDirtyPoint x y
                  addDirtyPoint (x + 23) (y + 23)
                  renderer <- use (globals.gRenderer)
                  maybe rendererError (drawPicAndReturn cs) renderer
             return (x, y, width, newIdx)
        drawPicAndReturn cs renderer = (renderer^.rRefExport.reDrawPic) x y cs
        valueError = processingError "SCR.processPicToken mValue is Nothing"
        configStringError = processingError "SCR.processPicToken configString is Nothing"
        rendererError = Com.fatalError "SCR.processPicToken renderer is Nothing"

processClientToken :: Int -> VidDefT -> Int -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processClientToken width vidDef newIdx6 (Just token1) (Just token2) (Just token3) (Just token4) (Just token5) (Just token6) =
  do addDirtyPoint x' y'
     addDirtyPoint (x' + 159) (y' + 31)
     clientInfo <- use (globals.gCl.csClientInfo)
     when (clientInfoIdx >= Constants.maxClients || clientInfoIdx < 0) $
       Com.comError Constants.errDrop "client >= MAX_CLIENTS"
     maybe clientInfoError proceedProcessClientToken (clientInfo V.!? clientInfoIdx)
  where x' = (vidDef^.vdWidth) `div` 2 - 160 + Lib.atoi token1
        y' = (vidDef^.vdHeight) `div` 2 - 120 + Lib.atoi token2
        clientInfoIdx = Lib.atoi token3
        score = Lib.atoi token4
        ping = Lib.atoi token5
        time = Lib.atoi token6
        clientInfoError = processingError "SCR.processClientToken clientInfo is Nothing"
        rendererError = Com.fatalError "SCR.processClientToken renderer is Nothing"
        proceedProcessClientToken clientInfo =
          do Console.drawAltString (x' + 32) y' (clientInfo^.ciName)
             Console.drawString (x' + 32) (y' + 8) "Score: "
             Console.drawAltString (x' + 32 + 7 * 8) (y' + 8) (encode score)
             Console.drawString (x' + 32) (y' + 16) ("Ping:  " `B.append` encode ping)
             Console.drawString (x' + 32) (y' + 24) ("Time:  " `B.append` encode time)
             renderer <- use (globals.gRenderer)
             maybe rendererError (drawPicAndReturn clientInfo) renderer
             return (x', y', width, newIdx6)
        drawPicAndReturn clientInfo renderer =
          case clientInfo^.ciIcon of
            Nothing ->
              do iconName <- use (globals.gCl.csBaseClientInfo.ciIconName)
                 (renderer^.rRefExport.reDrawPic) x' y' iconName
            Just _ -> (renderer^.rRefExport.reDrawPic) x' y' (clientInfo^.ciIconName)
processClientToken _ _ _ _ _ _ _ _ _ = processingError "SCR.processClientToken one of the mTokens is Nothing"

processCtfToken :: Int -> VidDefT -> Int -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processCtfToken width vidDef newIdx5 (Just token1) (Just token2) (Just token3) (Just token4) (Just token5) =
  do addDirtyPoint x' y'
     addDirtyPoint (x' + 159) (y' + 31)
     clientInfo <- use (globals.gCl.csClientInfo)
     when (clientInfoIdx >= Constants.maxClients || clientInfoIdx < 0) $
       Com.comError Constants.errDrop "client >= MAX_CLIENTS"
     maybe clientInfoError proceedProcessCtfToken (clientInfo V.!? clientInfoIdx)
  where x' = (vidDef^.vdWidth) `div` 2 - 160 + Lib.atoi token1
        y' = (vidDef^.vdHeight) `div` 2 - 120 + Lib.atoi token2
        clientInfoIdx = Lib.atoi token3
        score = Lib.atoi token4
        ping = min 999 (Lib.atoi token5)
        block clientInfo = BC.pack (printf "%3d %3d %-12.12s" score ping (BC.unpack (clientInfo^.ciName)))
        clientInfoError = processingError "SCR.processCtfToken clientInfo is Nothing"
        proceedProcessCtfToken clientInfo =
          do playerNum <- use (globals.gCl.csPlayerNum)
             drawNumString clientInfo playerNum
             return (x', y', width, newIdx5)
        drawNumString clientInfo playerNum
          | playerNum == clientInfoIdx = Console.drawAltString x' y' (block clientInfo)
          | otherwise = Console.drawString x' y' (block clientInfo)
processCtfToken _ _ _ _ _ _ _ _ = processingError "SCR.processCtfToken one of the mTokens is Nothing"

processPicnToken :: Int -> Int -> Int -> Maybe B.ByteString -> Int -> Maybe Renderer -> Quake (Int, Int, Int, Int)
processPicnToken x y width (Just token) newIdx (Just renderer) =
  do addDirtyPoint x y
     addDirtyPoint (x + 23) (y + 23)
     (renderer^.rRefExport.reDrawPic) x y token
     return (x, y, width, newIdx)
processPicnToken _ _ _ _ _ _ = processingError "SCR.processPicToken either mToken or renderer is Nothing"

processNumToken :: Int -> Int -> Int -> Maybe B.ByteString -> Maybe B.ByteString -> Quake (Int, Int, Int, Int)
processNumToken x y newIdx2 (Just token1) (Just token2) =
  do stats <- use (globals.gCl.csFrame.fPlayerState.psStats)
     maybe statsError doDrawStat (stats UV.!? statsIdx)
  where width = Lib.atoi token1
        statsIdx = Lib.atoi token2
        statsError = processingError "SCR.processNumToken stats value is Nothing"
        doDrawStat value =
          do drawField x y 0 width (fromIntegral value)
             return (x, y, width, newIdx2)
processNumToken _ _ _ _ _ = processingError "SCR.processNumToken one of the mTokens is Nothing"

processHnumToken :: Int -> Int -> Int -> Int -> Int16 -> Int16 -> Quake (Int, Int, Int, Int)
processHnumToken x y idx serverFrame health flashes =
  do when (flashes .&. 1 /= 0) $
       do renderer <- use (globals.gRenderer)
          maybe rendererError (\r -> (r^.rRefExport.reDrawPic) x y "field_3") renderer
     drawField x y color 3 (fromIntegral health)
     return (x, y, 3, idx)
  where color | health > 25 = 0
              | health > 0 = (serverFrame `shiftR` 2) .&. 1
              | otherwise = 1
        rendererError = Com.fatalError "SCR.processHnumToken renderer is Nothing"

processAnumToken :: Int -> Int -> Int -> Int -> Int16 -> Int16 -> Quake (Int, Int, Int, Int)
processAnumToken x y idx serverFrame ammo flahes
  | color == -1 = return (x, y, 3, idx)
  | otherwise =
      do when (flahes .&. 4 /= 0) $
           do renderer <- use (globals.gRenderer)
              maybe rendererError (\r -> (r^.rRefExport.reDrawPic) x y "field_3") renderer
         drawField x y color 3 (fromIntegral ammo)
         return (x, y, 3, idx)
  where color | ammo > 5 = 0
              | ammo >= 0 = (serverFrame `shiftR` 2) .&. 1
              | otherwise = -1
        rendererError = Com.fatalError "SCR.processAnumToken renderer is Nothing"

processRnumToken :: Int -> Int -> Int -> Int16 -> Int16 -> Quake (Int, Int, Int, Int)
processRnumToken x y idx armor flashes
  | armor < 1 = return (x, y, 3, idx)
  | otherwise =
      do when (flashes .&. 2 /= 0) $
           do renderer <- use (globals.gRenderer)
              maybe rendererError (\r -> (r^.rRefExport.reDrawPic) x y "field_3") renderer
         drawField x y 0 3 (fromIntegral armor)
         return (x, y, 3, idx)
  where rendererError = Com.fatalError "SCR.processRnumToken renderer is Nothing"

processingError :: B.ByteString -> Quake (Int, Int, Int, Int)
processingError msg =
  do Com.fatalError msg
     return (0, 0, 0, 0)

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

touchPics :: Renderer -> Quake ()
touchPics renderer =
  do V.mapM_ (renderer^.rRefExport.reRegisterPic) sbNums1
     processCrosshair renderer =<< crosshairCVar

processCrosshair :: Renderer -> CVarT -> Quake ()
processCrosshair renderer crosshair
  | (crosshair^.cvValue) == 0 = return ()
  | otherwise = join (liftA (updateCrosshair renderer) crossHairValue)
  where crossHairValue
          | (crosshair^.cvValue) > 3 || (crosshair^.cvValue) < 0 =
              do CVar.update (crosshair & cvValue .~ 3)
                 return 3
          | otherwise = return (crosshair^.cvValue)

updateCrosshair :: Renderer -> Float -> Quake ()
updateCrosshair renderer v =
  do scrGlobals.scrCrosshairPic .= pic
     dim <- (renderer^.rRefExport.reDrawGetPicSize) pic
     maybe updateCrosshairError setDimensions dim
  where i = truncate v :: Int
        pic = "ch" `B.append` (encode i)
        updateCrosshairError = Com.fatalError "SCR.updateCrosshair pic size is Nothing"
        setDimensions (width, height) =
          do scrGlobals.scrCrosshairWidth .= width
             scrGlobals.scrCrosshairHeight .= height
             when (width == 0) $ scrGlobals.scrCrosshairPic .= ""

drawField :: Int -> Int -> Int -> Int -> Int -> Quake ()
drawField x y color width value
  | width < 1 = return ()
  | otherwise =
      do addDirtyPoint x y
         addDirtyPoint (x + w * charWidth + 2) (y + 23)
         renderer <- use (globals.gRenderer)
         maybe rendererError (\r -> mapM_ (doDrawField r num color x' y) [0..len-1]) renderer
  where w = min width 5
        num = encode value
        len = min (B.length num) w
        x' = x + 2 + charWidth * (w - len)
        rendererError = Com.fatalError "SCR.drawField renderer is Nothing"

doDrawField :: Renderer -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
doDrawField renderer num color x y idx = (renderer^.rRefExport.reDrawPic) x y pic
  where ptr = num `BC.index` idx
        frame | ptr == '-' = statMinus
              | otherwise = ord ptr - ord '0'
        pic | color == 0 = sbNums1 V.! frame
            | otherwise = sbNums2 V.! frame

drawHUDString :: Maybe B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawHUDString = error "SCR.drawHUDString" -- TODO