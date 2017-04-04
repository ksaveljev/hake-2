{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Client.VID where

import Control.Lens ((.=), ix, (^.), zoom, use, preuse, (-=), (&), (.~), (-~), (+~))
import Control.Monad (void, liftM, when, unless)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Client.ClientStateT
import Client.ClientStaticT
import Client.RefExportT
import Client.VidModeT
import Client.MenuItem
import Client.MenuCommonS
import Render.Renderer
import Sys.KBD
import Types
import QuakeState
import CVarVariables
import QCommon.XCommandT
import Render.VideoMode
import qualified Constants
import qualified Client.Console as Console
import qualified Client.KeyConstants as KeyConstants
import {-# SOURCE #-} qualified Client.Menu as Menu
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import {-# SOURCE #-} qualified QCommon.Com as Com
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
restartF =
  XCommandT "VID.restartF" (do
    vidWidthValue <- liftM (truncate . (^.cvValue)) vidWidthCVar
    vidHeightValue <- liftM (truncate . (^.cvValue)) vidHeightCVar

    zoom (vidGlobals.vgVidModes.ix 11) $ do
      vmWidth .= vidWidthValue
      vmHeight .= vidHeightValue

    vidRef <- vidRefCVar
    CVar.update vidRef { _cvModified = True }
  )

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
    vd <- use $ globals.gVidDef
    globals.gVidDef .= vd { _vdWidth = (vd^.vdNewWidth), _vdHeight = (vd^.vdNewHeight) }

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

          globals.gCl.csRefreshPrepped .= False
          globals.gCls.csDisableScreen .= 1 -- True

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
            keyDest <- use $ globals.gCls.csKeyDest
            when (keyDest /= Constants.keyConsole) $
              (Console.toggleConsoleF)^.xcCmd -- IMPROVE: catch exception?

          globals.gCls.csDisableScreen .= 0 -- False

loadRefresh :: B.ByteString -> Bool -> Quake Bool
loadRefresh name fast = do
    refLibActive <- use $ vidGlobals.vgRefLibActive

    when refLibActive $ do
      Just renderer <- use $ globals.gRenderer
      renderer^.rRefExport.reGetKeyboardHandler.kbdClose

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
        globals.gRenderer .= r

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
            renderer^.rRefExport.reGetKeyboardHandler.kbdInit

            Com.printf "------------------------------------\n"
            vidGlobals.vgRefLibActive .= True
            return True

freeRefLib :: Quake ()
freeRefLib = do
    r <- use $ globals.gRenderer

    when (isJust r) $ do
      let Just renderer = r
      renderer^.rRefExport.reGetKeyboardHandler.kbdClose
      IN.shutdown

    globals.gRenderer .= Nothing
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

    liftM (truncate . (^.cvValue)) glModeCVar >>= \n ->
      modifyMenuListSReference modeListRef (\v -> v & mlCurValue .~ n)

    liftM (^.cvValue) vidFullScreenCVar >>= \fullscreenValue ->
      if fullscreenValue /= 0
        then do
          res <- use $ vidGlobals.vgFSResolutions

          menuItem <- readMenuListSReference modeListRef
          let curValue = if (menuItem^.mlCurValue) >= V.length res - 1
                           then 0
                           else menuItem^.mlCurValue

          modifyMenuListSReference modeListRef (\v -> v & mlCurValue .~ curValue
                                                        & mlItemNames .~ res
                                                        )
          Just fsModes <- use $ vidGlobals.vgFSModes
          vidGlobals.vgModeX .= (fsModes V.! curValue)^.vmWidth

        else do
          menuItem <- readMenuListSReference modeListRef
          let curValue = if (menuItem^.mlCurValue) >= V.length resolutions - 1
                           then 0
                           else menuItem^.mlCurValue

          modifyMenuListSReference modeListRef (\v -> v & mlCurValue .~ curValue
                                                        & mlItemNames .~ resolutions
                                                        )

          Just width <- preuse $ vidGlobals.vgVidModes.ix curValue.vmWidth
          vidGlobals.vgModeX .= width

    setNonExistingCVar "viewsize" "100" Constants.cvarArchive

    liftM (^.cvValue) viewSizeCVar >>= \v -> do
      let intv = truncate (v / 10) :: Int
      modifyMenuSliderSReference screenSizeSliderRef (\v -> v & msCurValue .~ fromIntegral intv)

    use (vidGlobals.vgDrivers) >>= \drivers -> do
      str <- liftM (^.cvString) vidRefCVar
      case V.findIndex (== str) drivers of
        Nothing -> return ()
        Just idx -> modifyMenuListSReference refListRef (\v -> v & mlCurValue .~ idx)

    use (globals.gVidDef.vdWidth) >>= \width -> do
      let widthF = fromIntegral width :: Float
      modifyMenuFrameworkSReference openGLMenuRef (\v -> v & mfX .~ truncate (widthF * 0.5)
                                                           & mfNItems .~ 0
                                                           )

    use (vidGlobals.vgRefs) >>= \refs -> do
      modifyMenuListSReference refListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                   & mlGeneric.mcName .~ Just "driver"
                                                   & mlGeneric.mcX .~ 0
                                                   & mlGeneric.mcY .~ 0
                                                   & mlGeneric.mcCallback .~ Just (vidGlobals.vgCurrentMenu .= Just openGLMenuRef)
                                                   & mlItemNames .~ refs
                                                   )

    modifyMenuListSReference modeListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                  & mlGeneric.mcName .~ Just "video mode"
                                                  & mlGeneric.mcX .~ 0
                                                  & mlGeneric.mcY .~ 10
                                                  )

    modifyMenuSliderSReference screenSizeSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                            & msGeneric.mcX .~ 0
                                                            & msGeneric.mcY .~ 20
                                                            & msGeneric.mcName .~ Just "screen size"
                                                            & msMinValue .~ 3
                                                            & msMaxValue .~ 12
                                                            & msGeneric.mcCallback .~ Just screenSizeCallback
                                                            )

    liftM (^.cvValue) vidGammaCVar >>= \gammaValue ->
      modifyMenuSliderSReference brightnessSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                              & msGeneric.mcX .~ 0
                                                              & msGeneric.mcY .~ 30
                                                              & msGeneric.mcName .~ Just "brightness"
                                                              & msMinValue .~ 5
                                                              & msMaxValue .~ 13
                                                              & msCurValue .~ (1.3 - gammaValue + 0.5) * 10
                                                              & msGeneric.mcCallback .~ Just brightnessCallback
                                                              )

    liftM (truncate . (^.cvValue)) vidFullScreenCVar >>= \fullscreenValue ->
      modifyMenuListSReference fsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                 & mlGeneric.mcX .~ 0
                                                 & mlGeneric.mcY .~ 40
                                                 & mlGeneric.mcName .~ Just "fullscreen"
                                                 & mlItemNames .~ yesNoNames
                                                 & mlCurValue .~ fullscreenValue
                                                 & mlGeneric.mcCallback .~ Just fsBoxCallback
                                                 )

    liftM (^.cvValue) glPicMipCVar >>= \picmipValue ->
      modifyMenuSliderSReference tqSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                                      & msGeneric.mcX .~ 0
                                                      & msGeneric.mcY .~ 60
                                                      & msGeneric.mcName .~ Just "texture quality"
                                                      & msMinValue .~ 0
                                                      & msMaxValue .~ 3
                                                      & msCurValue .~ 3 - picmipValue
                                                      )

    liftM (truncate . (^.cvValue)) glExtPalettedTextureCVar >>= \palettedTextureValue ->
      modifyMenuListSReference palettedTextureBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                              & mlGeneric.mcX .~ 0
                                                              & mlGeneric.mcY .~ 70
                                                              & mlGeneric.mcName .~ Just "8-bit textures"
                                                              & mlItemNames .~ yesNoNames
                                                              & mlCurValue .~ palettedTextureValue
                                                              )

    liftM (truncate . (^.cvValue)) glSwapIntervalCVar >>= \swapIntervalValue ->
      modifyMenuListSReference vSyncBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                                    & mlGeneric.mcX .~ 0
                                                    & mlGeneric.mcY .~ 80
                                                    & mlGeneric.mcName .~ Just "sync every frame"
                                                    & mlItemNames .~ yesNoNames
                                                    & mlCurValue .~ swapIntervalValue
                                                    )

    modifyMenuActionSReference defaultsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                          & maGeneric.mcName .~ Just "reset to default"
                                                          & maGeneric.mcX .~ 0
                                                          & maGeneric.mcY .~ 100
                                                          & maGeneric.mcCallback .~ Just menuInit
                                                          )

    modifyMenuActionSReference applyActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                                       & maGeneric.mcName .~ Just "apply"
                                                       & maGeneric.mcX .~ 0
                                                       & maGeneric.mcY .~ 110
                                                       & maGeneric.mcCallback .~ Just applyChanges
                                                       )

    Menu.menuAddItem openGLMenuRef (MenuListRef refListRef)
    Menu.menuAddItem openGLMenuRef (MenuListRef modeListRef)
    Menu.menuAddItem openGLMenuRef (MenuSliderRef screenSizeSliderRef)
    Menu.menuAddItem openGLMenuRef (MenuSliderRef brightnessSliderRef)
    Menu.menuAddItem openGLMenuRef (MenuListRef fsBoxRef)

    Menu.menuAddItem openGLMenuRef (MenuSliderRef tqSliderRef)
    Menu.menuAddItem openGLMenuRef (MenuListRef palettedTextureBoxRef)
    Menu.menuAddItem openGLMenuRef (MenuListRef vSyncBoxRef)

    Menu.menuAddItem openGLMenuRef (MenuActionRef defaultsActionRef)
    Menu.menuAddItem openGLMenuRef (MenuActionRef applyActionRef)

    Menu.menuCenter openGLMenuRef
    modifyMenuFrameworkSReference openGLMenuRef (\v -> v & mfX -~ 8)

  where setNonExistingCVar :: B.ByteString -> B.ByteString -> Int -> Quake ()
        setNonExistingCVar name value flags =
          CVar.findVar name >>= \v ->
            when (isNothing v) $
              void $ CVar.get name value flags

        screenSizeCallback :: Quake ()
        screenSizeCallback = do
          menuItem <- readMenuSliderSReference screenSizeSliderRef
          CVar.setValueF "viewsize" ((menuItem^.msCurValue) * 10)

        brightnessCallback :: Quake ()
        brightnessCallback = do
          str <- liftM ((BC.map toLower) . (^.cvString)) vidRefCVar

          when (str == "soft" || str == "softx") $ do
            menuItem <- readMenuSliderSReference brightnessSliderRef
            CVar.setValueF "vid_gamma" ((0.8 - ((menuItem^.msCurValue) / 10 - 0.5)) + 0.5)

        fsBoxCallback :: Quake ()
        fsBoxCallback = do
          io (putStrLn "VID.vgFSBox callback") >> undefined -- TODO

applyChanges :: Quake ()
applyChanges = do
    brightnessSlider <- readMenuSliderSReference brightnessSliderRef
    tqSlider <- readMenuSliderSReference tqSliderRef
    fsBox <- readMenuListSReference fsBoxRef
    vSyncBox <- readMenuListSReference vSyncBoxRef
    palettedTextureBox <- readMenuListSReference palettedTextureBoxRef
    modeList <- readMenuListSReference modeListRef
    refList <- readMenuListSReference refListRef

    -- invert sense so greater = brighter, and scale to a range of 0.5 to 1.3
    --
    -- the original was modified, because on CRTs it was too dark.
    -- the slider range is [5; 13]
        -- gamma: [1.1; 0.7]
    let gamma = 0.4 - ((brightnessSlider^.msCurValue) / 20.0 - 0.25) + 0.7
        -- modulate: [1.0; 2.6]
        modulate = (brightnessSlider^.msCurValue) * 0.2

    CVar.setValueF "vid_gamma" gamma
    CVar.setValueF "gl_modulate" modulate
    CVar.setValueF "gl_picmip" (3 - (tqSlider^.msCurValue))
    CVar.setValueI "vid_fullscreen" (fsBox^.mlCurValue)
    CVar.setValueI "gl_swapinterval" (vSyncBox^.mlCurValue)
    -- set always true because of vid_ref or mode changes
    glSwapIntervalCVar >>= \cvar -> CVar.update cvar { _cvModified = True }
    CVar.setValueI "gl_ext_palettedtexture" (palettedTextureBox^.mlCurValue)
    CVar.setValueI "gl_mode" (modeList^.mlCurValue)

    drivers <- use $ vidGlobals.vgDrivers

    CVar.set "vid_ref" (drivers V.! (refList^.mlCurValue))
    CVar.set "gl_driver" (drivers V.! (refList^.mlCurValue))

    glDriverModified <- liftM (^.cvModified) glDriverCVar
    when glDriverModified $ do
      vidRefCVar >>= \cvar -> CVar.update cvar { _cvModified = True }

    Menu.forceMenuOff

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
    Just renderer <- use $ globals.gRenderer

    modes <- renderer^.rRefExport.reGetModeList

    let (fsResolutions, fsModes) = V.unzip $ V.imap parseMode modes

    vidGlobals.vgFSModes .= Just fsModes
    vidGlobals.vgFSResolutions .= fsResolutions

  where parseMode :: Int -> VideoMode -> (B.ByteString, VidModeT)
        parseMode idx mode =
          let width = getVideoModeWidth mode
              height = getVideoModeHeight mode
              widthS = BC.pack (show width)
              heightS = BC.pack (show height)
              res = "[" `B.append` widthS `B.append` " " `B.append` heightS
              len = B.length res
              res' = (if len < 10 then res `B.append` BC.replicate (10 - len) ' ' else res) `B.append` "]"
              m = "Mode " `B.append` BC.pack (show idx) `B.append` widthS `B.append` "x" `B.append` heightS
          in (res', VidModeT m width height idx)

newWindow :: Int -> Int -> Quake ()
newWindow width height =
    zoom (globals.gVidDef) $ do
      vdNewWidth .= width
      vdNewHeight .= height

shutdown :: Quake ()
shutdown = do
    refLibActive <- use $ vidGlobals.vgRefLibActive

    when refLibActive $ do
      Just renderer <- use $ globals.gRenderer

      renderer^.rRefExport.reGetKeyboardHandler.kbdClose
      IN.shutdown

      renderer^.rRefExport.reShutDown
      freeRefLib

menuDraw :: XCommandT
menuDraw =
  XCommandT "VID.menuDraw" (do
    vidGlobals.vgCurrentMenu .= Just openGLMenuRef

    -- draw the banner
    Just renderer <- use $ globals.gRenderer
    vidDef' <- use $ globals.gVidDef
    Just (width, _) <- (renderer^.rRefExport.reDrawGetPicSize) "m_banner_video"
    (renderer^.rRefExport.reDrawPic) ((vidDef'^.vdWidth) `div` 2 - width `div` 2) ((vidDef'^.vdHeight) `div` 2 - 110) "m_banner_video"

    -- move cursor to a reasonable starting position
    Menu.menuAdjustCursor openGLMenuRef 1

    -- draw the menu
    Menu.menuDraw openGLMenuRef
  )

menuKey :: KeyFuncT
menuKey =
  KeyFuncT "VID.menuKey" (\key -> do
    Just menuRef <- use $ vidGlobals.vgCurrentMenu
    let sound = Just "misc/menu1.wav"

    if | key == KeyConstants.kEscape -> do
           Menu.popMenu
           return Nothing

       | key == KeyConstants.kUpArrow -> do
           modifyMenuFrameworkSReference menuRef (\v -> v & mfCursor -~ 1)
           Menu.menuAdjustCursor menuRef (-1)
           return sound

       | key == KeyConstants.kDownArrow -> do
           modifyMenuFrameworkSReference menuRef (\v -> v & mfCursor +~ 1)
           Menu.menuAdjustCursor menuRef 1
           return sound

       | key == KeyConstants.kLeftArrow -> do
           Menu.menuSlideItem menuRef (-1)
           return sound

       | key == KeyConstants.kRightArrow -> do
           Menu.menuSlideItem menuRef 1
           return sound

       | key == KeyConstants.kEnter -> do
           Menu.menuSelectItem menuRef
           return sound

       | otherwise ->
           return sound
  )
