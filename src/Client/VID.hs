{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Client.VID where

import Control.Lens ((.=), ix, (^.), zoom, use, preuse, (-=))
import Control.Monad (void, liftM, when, unless)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust)
import Graphics.UI.GLFW (VideoMode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.Console as Console
import qualified Client.Menu as Menu
import qualified Client.MenuConstants as MenuConstants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Graphics.UI.GLFW as GLFW
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified Render.QRenderer as QRenderer
import qualified Sound.S as S
import qualified Sys.IN as IN

resolutions :: V.Vector B.ByteString
resolutions =
    V.fromList [ "[320 240  ]"
               , "[400 300  ]"
               , "[512 384  ]"
               , "[640 480  ]"
               , "[800 600  ]"
               , "[960 720  ]"
               , "[1024 768 ]"
               , "[1152 864 ]"
               , "[1280 1024]"
               , "[1600 1200]"
               , "[2048 1536]"
               , "user mode"
               ]

yesNoNames :: V.Vector B.ByteString
yesNoNames = V.fromList ["no", "yes"]

init :: Quake ()
init = do
    -- Create the video variables so we know how to start the graphics drivers
    void $ CVar.get "vid_ref" QRenderer.getPreferredName Constants.cvarArchive
    void $ CVar.get "vid_xpos" "3" Constants.cvarArchive
    void $ CVar.get "vid_ypos" "22" Constants.cvarArchive
    void $ CVar.get "vid_width" "640" Constants.cvarArchive
    void $ CVar.get "vid_height" "480" Constants.cvarArchive
    void $ CVar.get "vid_fullscreen" "0" Constants.cvarArchive
    void $ CVar.get "vid_gamma" "1" Constants.cvarArchive

    vidGlobals.vgVidModes.ix 11.vmWidth .= 640
    vidGlobals.vgVidModes.ix 11.vmHeight .= 480

    -- Add some console commands that we want to handle
    Cmd.addCommand "vid_restart" (Just restartF)

    -- Disable the 3Dfx splash screen
    -- putenv("FX_GLIDE_NO_SPLASH=0");

    -- Start the graphics mode and load refresh DLL
    checkChanges

{-
============
VID_Restart_f

Console command to re-start the video mode and refresh DLL. We do this
simply by setting the modified flag for the vid_ref variable, which will
cause the entire video mode and refresh DLL to be reset on the next frame.
============
-}
restartF :: XCommandT
restartF = do
    vidWidthValue <- liftM (truncate . (^.cvValue)) vidWidthCVar
    vidHeightValue <- liftM (truncate . (^.cvValue)) vidHeightCVar

    zoom (vidGlobals.vgVidModes.ix 11) $ do
      vmWidth .= vidWidthValue
      vmHeight .= vidHeightValue

    vidRef <- vidRefCVar
    CVar.update vidRef { _cvModified = True }

{-
============
VID_CheckChanges

This function gets called once just before drawing each frame, and it's sole purpose in life
is to check to see if any of the video mode parameters have changed, and if they have to 
update the rendering DLL and/or video mode to match.
============
-}
checkChanges :: Quake ()
checkChanges = do
    vd <- use $ globals.vidDef
    globals.vidDef .= vd { _vdWidth = (vd^.vdNewWidth), _vdHeight = (vd^.vdNewHeight) }

    vidRef <- vidRefCVar

    when (vidRef^.cvModified) $
      S.stopAllSounds
    
    changeRefresh (vidRef^.cvModified)

  where changeRefresh :: Bool -> Quake ()
        changeRefresh False = return ()
        -- refresh has changed
        changeRefresh True = do
          vidRef <- vidRefCVar
          vidFullScreen <- vidFullScreenCVar

          CVar.update vidRef { _cvModified = False }
          CVar.update vidFullScreen { _cvModified = True }

          globals.cl.csRefreshPrepped .= False
          globals.cls.csDisableScreen .= 1 -- True

          loaded <- loadRefresh (vidRef^.cvString) True

          unless loaded $ do
            let renderer = if (vidRef^.cvString) == QRenderer.getPreferredName
                             -- try the default renderer as fallback after preferred
                             then QRenderer.getDefaultName
                             -- try the preferred renderer as first fallback
                             else QRenderer.getPreferredName

            renderer' <- if (vidRef^.cvString) == QRenderer.getDefaultName
                           then do
                             Com.printf "Refresh failed\n"
                             Just glMode <- CVar.get "gl_mode" "0" 0

                             if (glMode^.cvValue) /= 0
                               then do
                                 Com.printf "Trying mode 0\n"
                                 CVar.setValueF "gl_mode" 0

                                 loaded' <- loadRefresh (vidRef^.cvString) False
                                 unless loaded' $
                                   Com.comError Constants.errFatal ("Couldn't fall back to " `B.append` (vidRef^.cvString) `B.append` " refresh!")
                               else 
                                 Com.comError Constants.errFatal ("Couldn't fall back to " `B.append` (vidRef^.cvString) `B.append` " refresh!")

                             return (vidRef^.cvString)

                           else return renderer

            void $ CVar.set "vid_ref" renderer'

            -- drop the console if we fail to load a refresh
            keyDest <- use $ globals.cls.csKeyDest
            when (keyDest /= Constants.keyConsole) $
              Console.toggleConsoleF -- TODO: catch exception?

          globals.cls.csDisableScreen .= 0 -- False

loadRefresh :: B.ByteString -> Bool -> Quake Bool
loadRefresh name fast = do
    refLibActive <- use $ vidGlobals.vgRefLibActive

    when refLibActive $ do
      Just renderer <- use $ globals.re
      let Just kbd = renderer^.rRefExport.reGetKeyboardHandler
      kbd^.kbdClose

      IN.shutdown

      renderer^.rRefExport.reShutDown

      freeRefLib

    Com.printf $ "------- Loading " `B.append` name `B.append` " -------\n"

    let found = V.elem name QRenderer.getDriverNames

    if not found
      then do
        Com.printf $ "LoadLibrary(\"" `B.append` name `B.append` "\") failed\n"
        return False
      else do
        Com.printf $ "LoadLibrary(\"" `B.append` name `B.append` "\")\n"
        let r = QRenderer.getDriver name fast
        globals.re .= r

        when (isNothing r) $
          Com.comError Constants.errFatal (name `B.append` " can't load but registered")

        let Just renderer = r

        when ((renderer^.rRefExport.reApiVersion) /= Constants.apiVersion) $ do
          freeRefLib
          Com.comError Constants.errFatal (name `B.append` " has incompatible api_version")

        IN.realINInit

        xpos <- liftM (truncate . (^.cvValue)) vidXPosCVar
        ypos <- liftM (truncate . (^.cvValue)) vidYPosCVar
        ok <- (renderer^.rRefExport.reInit) xpos ypos

        if not ok
          then do
            renderer^.rRefExport.reShutDown
            freeRefLib
            return False
          else do
            -- init KBD
            let Just kbd = renderer^.rRefExport.reGetKeyboardHandler
            kbd^.kbdInit

            Com.printf "------------------------------------\n"
            vidGlobals.vgRefLibActive .= True
            return True

freeRefLib :: Quake ()
freeRefLib = do
    r <- use $ globals.re

    when (isJust r) $ do
      let Just renderer = r
          Just kbd = renderer^.rRefExport.reGetKeyboardHandler
      kbd^.kbdClose
      IN.shutdown

    globals.re .= Nothing
    vidGlobals.vgRefLibActive .= False

printf :: Int -> B.ByteString -> Quake ()
printf printLevel str =
    if printLevel == Constants.printAll
      then
        Com.printf str
      else
        Com.dprintf str

menuInit :: Quake ()
menuInit = do
    initRefs

    setNonExistingCVar "gl_driver" QRenderer.getPreferredName 0
    setNonExistingCVar "gl_picmip" "0" 0
    setNonExistingCVar "gl_mode" "3" 0
    setNonExistingCVar "gl_ext_palettedtexture" "1" Constants.cvarArchive
    setNonExistingCVar "gl_swapinterval" "0" Constants.cvarArchive

    liftM (truncate . (^.cvValue)) glModeCVar >>= \v ->
      menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue .= v

    liftM (^.cvValue) vidFullScreenCVar >>= \fullscreenValue ->
      if fullscreenValue /= 0
        then do
          res <- use $ vidGlobals.vgFSResolutions
          menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlItemNames .= Just res

          preuse (menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue) >>= \(Just v) ->
            when (v >= V.length res - 1) $
              menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue .= 0

          preuse (menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue) >>= \(Just v) -> do
            Just fsModes <- use $ vidGlobals.vgFSModes
            vidGlobals.vgModeX .= (fsModes V.! v)^.vmWidth

        else do
          menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlItemNames .= Just resolutions

          preuse (menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue) >>= \(Just v) ->
            when (v >= V.length resolutions - 1) $
              menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue .= 0

          preuse (menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlCurValue) >>= \(Just v) -> do
            Just width <- preuse $ vidGlobals.vgVidModes.ix v.vmWidth
            vidGlobals.vgModeX .= width

    setNonExistingCVar "viewsize" "100" Constants.cvarArchive

    liftM (^.cvValue) viewSizeCVar >>= \v -> do
      let intv :: Int = truncate (v / 10)
      menuGlobals.mgMenuItems.ix (MenuConstants.screenSizeSlider).msCurValue .= fromIntegral intv

    use (vidGlobals.vgDrivers) >>= \drivers -> do
      str <- liftM (^.cvString) vidRefCVar
      let idx = V.findIndex (== str) drivers
      
      when (isJust idx) $
        menuGlobals.mgMenuItems.ix (MenuConstants.refList).mlCurValue .= fromJust idx

    use (globals.vidDef.vdWidth) >>= \width -> do
      let widthF :: Float = fromIntegral width
      zoom (menuGlobals.mgMenuFrameworks.ix (MenuConstants.openGLMenu)) $ do
        mfX .= truncate (widthF * 0.5)
        mfNItems .= 0

    use (vidGlobals.vgRefs) >>= \refs -> do
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.refList)) $ do
        mlGeneric.mcType .= Constants.mtypeSpinControl
        mlGeneric.mcName .= "driver"
        mlGeneric.mcX .= 0
        mlGeneric.mcY .= 0
        mlGeneric.mcCallback .= Just (vidGlobals.vgCurrentMenu .= Just (MenuFrameworkSReference MenuConstants.openGLMenu))
        mlItemNames .= Just refs

    zoom (menuGlobals.mgMenuItems.ix (MenuConstants.modeList).mlGeneric) $ do
      mcType .= Constants.mtypeSpinControl
      mcName .= "video mode"
      mcX .= 0
      mcY .= 10

    zoom (menuGlobals.mgMenuItems.ix (MenuConstants.screenSizeSlider)) $ do
      msGeneric.mcType .= Constants.mtypeSlider
      msGeneric.mcX .= 0
      msGeneric.mcY .= 20
      msGeneric.mcName .= "screen size"
      msMinValue .= 3
      msMaxValue .= 12
      msGeneric.mcCallback .= Just (preuse (menuGlobals.mgMenuItems.ix (MenuConstants.screenSizeSlider).msCurValue) >>= \(Just v) -> CVar.setValueF "viewsize" (v * 10))

    liftM (^.cvValue) vidGammaCVar >>= \gammaValue ->
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.brightnessSlider)) $ do
        msGeneric.mcType .= Constants.mtypeSlider
        msGeneric.mcX .= 0
        msGeneric.mcY .= 30
        msGeneric.mcName .= "brightness"
        msMinValue .= 5
        msMaxValue .= 13
        msCurValue .= (1.3 - gammaValue + 0.5) * 10
        msGeneric.mcCallback .= Just ( do
                                         str <- liftM ((BC.map toLower) . (^.cvString)) vidRefCVar
                                         when (str == "soft" || str == "softx") $
                                          preuse (menuGlobals.mgMenuItems.ix (MenuConstants.brightnessSlider).msCurValue) >>= \(Just v) ->
                                             CVar.setValueF "vid_gamma" ((0.8 - (v / 10 - 0.5)) + 0.5)
                                     )

    liftM (truncate . (^.cvValue)) vidFullScreenCVar >>= \fullscreenValue ->
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.fsBox)) $ do
        mlGeneric.mcType .= Constants.mtypeSpinControl
        mlGeneric.mcX .= 0
        mlGeneric.mcY .= 40
        mlGeneric.mcName .= "fullscreen"
        mlItemNames .= Just yesNoNames
        mlCurValue .= fullscreenValue
        mlGeneric.mcCallback .= Just (io (putStrLn "VID.vgFSBox callback") >> undefined) -- TODO

    liftM (^.cvValue) glPicMipCVar >>= \picmipValue ->
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.tqSlider)) $ do
        msGeneric.mcType .= Constants.mtypeSlider
        msGeneric.mcX .= 0
        msGeneric.mcY .= 60
        msGeneric.mcName .= "texture quality"
        msMinValue .= 0
        msMaxValue .= 3
        msCurValue .= 3 - picmipValue

    liftM (truncate . (^.cvValue)) glExtPalettedTextureCVar >>= \palettedTextureValue ->
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.palettedTextureBox)) $ do
        mlGeneric.mcType .= Constants.mtypeSpinControl
        mlGeneric.mcX .= 0
        mlGeneric.mcY .= 70
        mlGeneric.mcName .= "8-bit textures"
        mlItemNames .= Just yesNoNames
        mlCurValue .= palettedTextureValue

    liftM (truncate . (^.cvValue)) glSwapIntervalCVar >>= \swapIntervalValue ->
      zoom (menuGlobals.mgMenuItems.ix (MenuConstants.vSyncBox)) $ do
        mlGeneric.mcType .= Constants.mtypeSpinControl
        mlGeneric.mcX .= 0
        mlGeneric.mcY .= 80
        mlGeneric.mcName .= "sync every frame"
        mlItemNames .= Just yesNoNames
        mlCurValue .= swapIntervalValue

    zoom (menuGlobals.mgMenuItems.ix (MenuConstants.defaultsAction).maGeneric) $ do
      mcType .= Constants.mtypeAction
      mcName .= "reset to default"
      mcX .= 0
      mcY .= 100
      mcCallback .= Just menuInit

    zoom (menuGlobals.mgMenuItems.ix (MenuConstants.applyAction).maGeneric) $ do
      mcType .= Constants.mtypeAction
      mcName .= "apply"
      mcX .= 0
      mcY .= 110
      mcCallback .= Just applyChanges

    Menu.addItem MenuConstants.openGLMenu MenuConstants.refList
    Menu.addItem MenuConstants.openGLMenu MenuConstants.modeList
    Menu.addItem MenuConstants.openGLMenu MenuConstants.screenSizeSlider
    Menu.addItem MenuConstants.openGLMenu MenuConstants.brightnessSlider
    Menu.addItem MenuConstants.openGLMenu MenuConstants.fsBox

    Menu.addItem MenuConstants.openGLMenu MenuConstants.tqSlider
    Menu.addItem MenuConstants.openGLMenu MenuConstants.palettedTextureBox
    Menu.addItem MenuConstants.openGLMenu MenuConstants.vSyncBox

    Menu.addItem MenuConstants.openGLMenu MenuConstants.defaultsAction
    Menu.addItem MenuConstants.openGLMenu MenuConstants.applyAction

    Menu.center MenuConstants.openGLMenu
    menuGlobals.mgMenuFrameworks.ix (MenuConstants.openGLMenu).mfX -= 8

  where setNonExistingCVar :: B.ByteString -> B.ByteString -> Int -> Quake ()
        setNonExistingCVar name value flags =
          CVar.findVar name >>= \v ->
            when (isNothing v) $
              void $ CVar.get name value flags

applyChanges :: Quake ()
applyChanges = io (putStrLn "VID.applyChanges") >> undefined -- TODO

initRefs :: Quake ()
initRefs = do
    let drivers = QRenderer.getDriverNames
    vidGlobals.vgDrivers .= drivers
    vidGlobals.vgRefs .= V.map constructRef drivers

  where constructRef :: B.ByteString -> B.ByteString
        constructRef driver =
          let a = "[OpenGL " `B.append` driver
              b = if B.length a < 16 then a `B.append` BC.replicate (16 - B.length a) ' ' else a
              c = b `B.append` "]"
          in c

getModeInfo :: Int -> Quake (Maybe (Int, Int))
getModeInfo mode = do
    use (vidGlobals.vgFSModes) >>= \v ->
      when (isNothing v) initModeList

    fullscreenValue <- liftM (^.cvValue) vidFullScreenCVar
    modes <- if fullscreenValue /= 0
               then liftM fromJust (use $ vidGlobals.vgFSModes)
               else use $ vidGlobals.vgVidModes

    if mode < 0 || mode > V.length modes
      then return Nothing
      else do
        let m = modes V.! mode
        return $ Just (m^.vmWidth, m^.vmHeight)

initModeList :: Quake ()
initModeList = do
    Just renderer <- use $ globals.re

    modes <- renderer^.rRefExport.reGetModeList

    let (fsResolutions, fsModes) = V.unzip $ V.imap parseMode modes

    vidGlobals.vgFSModes .= Just fsModes
    vidGlobals.vgFSResolutions .= fsResolutions

  where parseMode :: Int -> VideoMode -> (B.ByteString, VidModeT)
        parseMode idx mode =
          let width = GLFW.videoModeWidth mode
              height = GLFW.videoModeHeight mode
              widthS = BC.pack (show width)
              heightS = BC.pack (show height)
              res = "[" `B.append` widthS `B.append` " " `B.append` heightS
              len = B.length res
              res' = (if len < 10 then res `B.append` BC.replicate (10 - len) ' ' else res) `B.append` "]"
              m = "Mode " `B.append` BC.pack (show idx) `B.append` widthS `B.append` "x" `B.append` heightS
          in (res', VidModeT m width height idx)

newWindow :: Int -> Int -> Quake ()
newWindow width height =
    zoom (globals.vidDef) $ do
      vdNewWidth .= width
      vdNewHeight .= height
