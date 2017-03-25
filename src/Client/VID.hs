{-# LANGUAGE FlexibleContexts #-}
module Client.VID
    ( checkChanges
    , getModeInfo
    , initialize
    , menuDrawF
    , menuInit
    , menuKeyF
    , newWindow
    , printf
    , shutdown
    ) where

import           Control.Lens          (use, ix, (.=), (%=), (^.), (&), (.~), (-~))
import           Control.Monad         (when, unless, void)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import qualified Data.Vector           as V
import qualified Graphics.UI.GLFW      as GLFW

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.Console        as Console
import qualified Client.Menu           as Menu
import           Client.MenuActionS
import           Client.MenuCommonS
import           Client.MenuFrameworkS
import           Client.MenuListS
import           Client.MenuSliderS
import           Client.RefExportT
import           Client.VidDefT
import           Client.VidModeT
import qualified Constants
import qualified Game.Cmd              as Cmd
import           Game.CVarT
import qualified QCommon.Com           as Com
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import           QCommon.XCommandT     (runXCommandT)
import           QuakeRef
import           QuakeState
import           Render.DummyRenderer  (dummyRenderer)
import qualified Render.QRenderer      as QRenderer
import           Render.Renderer
import qualified Sound.S               as S
import qualified Sys.IN                as IN
import           Sys.KBD
import           Types

resolutions :: V.Vector B.ByteString
resolutions = V.fromList
    [ "[320 240  ]"
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

checkChanges :: Quake ()
checkChanges = do
    vidDef <- use (globals.gVidDef)
    globals.gVidDef %= (\v -> v & vdWidth .~ (vidDef^.vdNewWidth)
                                & vdHeight .~ (vidDef^.vdNewHeight))
    changeRefresh =<< vidRefCVar

changeRefresh :: CVarT -> Quake ()
changeRefresh vidRef
    | vidRef^.cvModified = do
        S.stopAllSounds
        updateState =<< vidFullScreenCVar
        loaded <- loadRefresh (vidRef^.cvString) True
        unless loaded $
            loadPreferredOrDefault vidRef
        globals.gCls.csDisableScreen .= 0 -- False
    | otherwise = return ()
  where
    updateState vidFullScreen = do
        CVar.update (vidRef & cvModified .~ False)
        CVar.update (vidFullScreen & cvModified .~ True)
        globals.gCl.csRefreshPrepped .= False
        globals.gCls.csDisableScreen .= 1 -- True

loadRefresh :: B.ByteString -> Bool -> Quake Bool
loadRefresh name fast = do
    freeActiveRefLib =<< use (vidGlobals.vgRefLibActive)
    Com.printf (B.concat ["------- Loading ", name, " -------\n"])
    loadLibrary name fast driverFound
  where
    driverFound = V.elem name QRenderer.getDriverNames

freeActiveRefLib :: Bool -> Quake ()
freeActiveRefLib False = return ()
freeActiveRefLib True =
    freeLib =<< use (globals.gRenderer)
  where
    freeLib renderer = do
        renderer^.rRefExport.reGetKeyboardHandler.kbdClose
        IN.shutdown
        renderer^.rRefExport.reShutDown
        freeRefLib

freeRefLib :: Quake ()
freeRefLib = do
    shutItDown =<< use (globals.gRenderer)
    globals.gRenderer .= dummyRenderer
    vidGlobals.vgRefLibActive .= False
  where
    shutItDown renderer = do
        renderer^.rRefExport.reGetKeyboardHandler.kbdClose
        IN.shutdown

loadLibrary :: B.ByteString -> Bool -> Bool -> Quake Bool
loadLibrary name _ False = do
    Com.printf (B.concat ["LoadLibrary(\"", name, "\") failed\n"])
    return False
loadLibrary name fast True = do
    Com.printf (B.concat ["LoadLibrary(\"", name, "\")\n"])
    globals.gRenderer .= renderer
    proceedLoadLibrary name renderer
  where
    renderer = QRenderer.getDriver name fast

proceedLoadLibrary :: B.ByteString -> Renderer -> Quake Bool
proceedLoadLibrary name renderer
    | renderer^.rRefExport.reApiVersion /= Constants.apiVersion = do
        freeRefLib
        Com.fatalError (name `B.append` " has incompatibe api_version")
        return False
    | otherwise = do
        IN.realINInit
        xpos <- fmap (truncate . (^.cvValue)) vidXPosCVar
        ypos <- fmap (truncate . (^.cvValue)) vidYPosCVar
        (renderer^.rRefExport.reInit) xpos ypos >>= finishLoadLibrary renderer

finishLoadLibrary :: Renderer -> Bool -> Quake Bool
finishLoadLibrary renderer False = do
    renderer^.rRefExport.reShutDown
    freeRefLib
    return False
finishLoadLibrary renderer True = do
    renderer^.rRefExport.reGetKeyboardHandler.kbdInit
    Com.printf "------------------------------------\n"
    vidGlobals.vgRefLibActive .= True
    return True

loadPreferredOrDefault :: CVarT -> Quake ()
loadPreferredOrDefault vidRef = do
    void (CVar.set "vid_ref" =<< pickRenderer)
    keyDest <- use (globals.gCls.csKeyDest)
    when (keyDest /= Constants.keyConsole) $
        runXCommandT Console.toggleConsoleF
  where
    rendererToTry
        | (vidRef^.cvString) == QRenderer.getPreferredName = QRenderer.getDefaultName
        | otherwise = QRenderer.getPreferredName
    pickRenderer
        | (vidRef^.cvString) == QRenderer.getDefaultName = do
            Com.printf "Refresh failed\n"
            CVar.get "gl_mode" "0" 0 >>= tryMode vidRef
            return (vidRef^.cvString)
        | otherwise = return rendererToTry

tryMode :: CVarT -> Maybe CVarT -> Quake ()
tryMode _ Nothing = Com.fatalError "VID.tryMode glModeCVar is Nothing"
tryMode vidRef (Just glMode)
    | (glMode^.cvValue) /= 0 = do
        Com.printf "Trying mode 0\n"
        CVar.setValueF "gl_mode" 0
        loaded <- loadRefresh (vidRef^.cvString) False
        unless loaded fallBackError
    | otherwise = fallBackError
  where
    fallBackError = Com.fatalError (B.concat ["Couldn't fall back to ", vidRef^.cvString, " refresh!"])

getModeInfo :: Int -> Quake (Maybe (Int, Int))
getModeInfo = error "VID.getModeInfo" -- TODO

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
    [ ("vid_ref", QRenderer.getPreferredName, Constants.cvarArchive)
    , ("vid_xpos", "3", Constants.cvarArchive)
    , ("vid_ypos", "22", Constants.cvarArchive)
    , ("vid_width", "640", Constants.cvarArchive)
    , ("vid_height", "480", Constants.cvarArchive)
    , ("vid_fullscreen", "0", Constants.cvarArchive)
    , ("vid_gamma", "1", Constants.cvarArchive)
    ]

initialize :: Quake ()
initialize = do
    CVar.initializeCVars initialCVars
    vidGlobals.vgVidModes.ix 11.vmWidth .= 640
    vidGlobals.vgVidModes.ix 11.vmHeight .= 480
    Cmd.addCommand "vid_restart" (Just restartF)
    checkChanges

menuDrawF :: XCommandT
menuDrawF = error "VID.menuDrawF" -- TODO

menuInitCVars :: [(B.ByteString, B.ByteString, Int)]
menuInitCVars =
    [ ("gl_driver", QRenderer.getPreferredName, 0)
    , ("gl_picmip", "0", 0), ("gl_mode", "3", 0)
    , ("gl_ext_palettedtexture", "1", Constants.cvarArchive)
    , ("gl_swapinterval", "0", Constants.cvarArchive)
    ]

menuInit :: Quake ()
menuInit = do
    initRefs
    setMenuInitCVars
    setMenuGLMode =<< glModeCVar
    setMenuResolutionList =<< vidFullScreenCVar
    setNonExistingCVar ("viewsize", "100", Constants.cvarArchive)
    setMenuViewSize =<< viewSizeCVar
    setMenuDriver =<< vidRefCVar
    setMenuVidDef =<< use (globals.gVidDef)
    setMenuRefs =<< use (vidGlobals.vgRefs)
    setMenuModeListRef
    setMenuScreenSizeSlider
    setMenuBrightnessSlider =<< vidGammaCVar
    setMenuFullscreenCheckbox =<< vidFullScreenCVar
    setMenuTextureQualitySlider =<< glPicMipCVar
    setMenuPalettedTextureCheckbox =<< glExtPalettedTextureCVar
    setMenuSwapIntervalCheckbox =<< glSwapIntervalCVar
    setMenuResetToDefault
    setMenuApplyAction
    setOpenGLMenu

initRefs :: Quake ()
initRefs = do
    vidGlobals.vgDrivers .= drivers
    vidGlobals.vgRefs .= V.map constructRef drivers
  where
    drivers = QRenderer.getDriverNames

constructRef :: B.ByteString -> B.ByteString
constructRef driver = ref
  where
    start = "[OpenGL " `B.append` driver
    mid | len < 16 = start `B.append` BC.replicate (16 - len) ' '
        | otherwise = start
    len = B.length start
    ref = mid `B.append` "]"

setMenuInitCVars :: Quake ()
setMenuInitCVars = mapM_ setNonExistingCVar menuInitCVars

setNonExistingCVar :: (B.ByteString, B.ByteString, Int) -> Quake ()
setNonExistingCVar (name, value, flags) = do
    var <- CVar.findVar name
    maybe setVar (const (return ())) var
  where
    setVar = void (CVar.get name value flags)

setMenuGLMode :: CVarT -> Quake ()
setMenuGLMode glMode =
    modifyRef modeListRef (\v -> v & mlCurValue .~ truncate (glMode^.cvValue))

setMenuResolutionList :: CVarT -> Quake ()
setMenuResolutionList fullScreen
    | fullScreen^.cvValue /= 0 = do
        res <- use (vidGlobals.vgFSResolutions)
        curValue <- updateResolutionMenu res =<< readRef modeListRef
        maybe fsModesError (setModeX curValue) =<< use (vidGlobals.vgFSModes)
    | otherwise = do
        curValue <- updateResolutionMenu resolutions =<< readRef modeListRef
        setModeX curValue =<< use (vidGlobals.vgVidModes)
  where
    fsModesError = Com.fatalError "VID.setMenuResolutionList fsModes is Nothing"
    setModeX curValue modes = vidGlobals.vgModeX .= (modes V.! curValue)^.vmWidth

updateResolutionMenu :: V.Vector B.ByteString -> MenuListS -> Quake Int
updateResolutionMenu res menuItem = do
    modifyRef modeListRef (\v -> v & mlCurValue .~ curValue
                                   & mlItemNames .~ res)
    return curValue
  where
    curValue | (menuItem^.mlCurValue) >= V.length res - 1 = 0
             | otherwise = menuItem^.mlCurValue

setMenuViewSize :: CVarT -> Quake ()
setMenuViewSize viewSize =
    modifyRef screenSizeSliderRef (\v -> v & msCurValue .~ fromIntegral intv)
  where
    intv = truncate ((viewSize^.cvValue) / 10) :: Int

setMenuDriver :: CVarT -> Quake ()
setMenuDriver vidRef = do
    drivers <- use (vidGlobals.vgDrivers)
    updateMenu (V.findIndex (== (vidRef^.cvString)) drivers)
  where
    updateMenu Nothing = return ()
    updateMenu (Just idx) = modifyRef refListRef (\v -> v & mlCurValue .~ idx)

setMenuVidDef :: VidDefT -> Quake ()
setMenuVidDef vidDef =
    modifyRef openGLMenuRef (\v -> v & mfX .~ truncate (widthF / 2)
                                     & mfNItems .~ 0)
  where
    widthF = fromIntegral (vidDef^.vdWidth) :: Float

setMenuRefs :: V.Vector B.ByteString -> Quake ()
setMenuRefs refs =
    modifyRef refListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                  & mlGeneric.mcName .~ Just "driver"
                                  & mlGeneric.mcX .~ 0
                                  & mlGeneric.mcY .~ 0
                                  & mlGeneric.mcCallback .~ Just (vidGlobals.vgCurrentMenu .= Just openGLMenuRef)
                                  & mlItemNames .~ refs)

setMenuModeListRef :: Quake ()
setMenuModeListRef =
    modifyRef modeListRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                   & mlGeneric.mcName .~ Just "video mode"
                                   & mlGeneric.mcX .~ 0
                                   & mlGeneric.mcY .~ 10)

setMenuScreenSizeSlider :: Quake ()
setMenuScreenSizeSlider =
    modifyRef screenSizeSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                           & msGeneric.mcX .~ 0
                                           & msGeneric.mcY .~ 20
                                           & msGeneric.mcName .~ Just "screen size"
                                           & msMinValue .~ 3
                                           & msMaxValue .~ 12
                                           & msGeneric.mcCallback .~ Just screenSizeCallback)

screenSizeCallback :: Quake ()
screenSizeCallback = do
    menuItem <- readRef screenSizeSliderRef
    CVar.setValueF "viewsize" ((menuItem^.msCurValue) * 10)

setMenuBrightnessSlider :: CVarT -> Quake ()
setMenuBrightnessSlider vidGamma =
    modifyRef brightnessSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                           & msGeneric.mcX .~ 0
                                           & msGeneric.mcY .~ 30
                                           & msGeneric.mcName .~ Just "brightness"
                                           & msMinValue .~ 5
                                           & msMaxValue .~ 13
                                           & msCurValue .~ (1.3 - (vidGamma^.cvValue) + 0.5) * 10
                                           & msGeneric.mcCallback .~ Just brightnessCallback)

brightnessCallback :: Quake ()
brightnessCallback = setBrightness =<< vidRefCVar

setBrightness :: CVarT -> Quake ()
setBrightness vidRef
    | str == "soft" || str == "softx" = do
        menuItem <- readRef brightnessSliderRef
        CVar.setValueF "vid_gamma" ((0.8 - ((menuItem^.msCurValue) / 10 - 0.5)) + 0.5)
    | otherwise = return ()
  where
    str = BC.map toLower (vidRef^.cvString)

setMenuFullscreenCheckbox :: CVarT -> Quake ()
setMenuFullscreenCheckbox fullScreen =
    modifyRef fsBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                & mlGeneric.mcX .~ 0
                                & mlGeneric.mcY .~ 40
                                & mlGeneric.mcName .~ Just "fullscreen"
                                & mlItemNames .~ yesNoNames
                                & mlCurValue .~ truncate (fullScreen^.cvValue)
                                & mlGeneric.mcCallback .~ Just fsBoxCallback)

fsBoxCallback :: Quake ()
fsBoxCallback = error "VID.fsBoxCallback" -- TODO

setMenuTextureQualitySlider :: CVarT -> Quake ()
setMenuTextureQualitySlider picMip =
    modifyRef tqSliderRef (\v -> v & msGeneric.mcType .~ Constants.mtypeSlider
                                   & msGeneric.mcX .~ 0
                                   & msGeneric.mcY .~ 60
                                   & msGeneric.mcName .~ Just "texture quality"
                                   & msMinValue .~ 0
                                   & msMaxValue .~ 3
                                   & msCurValue .~ 3 - (picMip^.cvValue))

setMenuPalettedTextureCheckbox :: CVarT -> Quake ()
setMenuPalettedTextureCheckbox palettedTexture =
    modifyRef palettedTextureBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                             & mlGeneric.mcX .~ 0
                                             & mlGeneric.mcY .~ 70
                                             & mlGeneric.mcName .~ Just "8-bit textures"
                                             & mlItemNames .~ yesNoNames
                                             & mlCurValue .~ truncate (palettedTexture^.cvValue))

setMenuSwapIntervalCheckbox :: CVarT -> Quake ()
setMenuSwapIntervalCheckbox swapInterval =
    modifyRef vSyncBoxRef (\v -> v & mlGeneric.mcType .~ Constants.mtypeSpinControl
                                   & mlGeneric.mcX .~ 0
                                   & mlGeneric.mcY .~ 80
                                   & mlGeneric.mcName .~ Just "sync every frame"
                                   & mlItemNames .~ yesNoNames
                                   & mlCurValue .~ truncate (swapInterval^.cvValue))

setMenuResetToDefault :: Quake ()
setMenuResetToDefault =
    modifyRef defaultsActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                         & maGeneric.mcName .~ Just "reset to default"
                                         & maGeneric.mcX .~ 0
                                         & maGeneric.mcY .~ 100
                                         & maGeneric.mcCallback .~ Just menuInit)

setMenuApplyAction :: Quake ()
setMenuApplyAction =
    modifyRef applyActionRef (\v -> v & maGeneric.mcType .~ Constants.mtypeAction
                                      & maGeneric.mcName .~ Just "apply"
                                      & maGeneric.mcX .~ 0
                                      & maGeneric.mcY .~ 110
                                      & maGeneric.mcCallback .~ Just applyChanges)

applyChanges :: Quake ()
applyChanges = error "VID.applyChanges" -- TODO

setOpenGLMenu :: Quake ()
setOpenGLMenu = do
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
    modifyRef openGLMenuRef (\v -> v & mfX -~ 8)

menuKeyF :: KeyFuncT
menuKeyF = error "VID.menuKeyF" -- TODO

newWindow :: Int -> Int -> Quake ()
newWindow = error "VID.newWindow" -- TODO

printf :: Int -> B.ByteString -> Quake ()
printf printLevel str
    | printLevel == Constants.printAll = Com.printf str
    | otherwise = Com.dprintf str

restartF :: XCommandT
restartF = error "VID.restartF" -- TODO

shutdown :: Quake ()
shutdown = error "VID.shutdown" -- TODO