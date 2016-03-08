module Client.VIDShared
  ( getModeInfo
  , newWindow
  , printf
  ) where

import           Client.RefExportT
import           Client.VidDefT
import           Client.VidModeT
import           Game.CVarT
import qualified Constants
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeState
import           Render.Renderer
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW

printf :: Int -> B.ByteString -> Quake ()
printf printLevel str
  | printLevel == Constants.printAll = Com.printf str
  | otherwise = Com.dprintf str

newWindow :: Int -> Int -> Quake ()
newWindow width height =
  do globals.gVidDef.vdNewWidth .= width
     globals.gVidDef.vdNewHeight .= height

getModeInfo :: Int -> Quake (Maybe (Int, Int))
getModeInfo mode =
  do maybe initModeList (const (return ())) =<< use (vidGlobals.vgFSModes)
     modes <- getModes =<< fullScreenValue
     return (modeInfo modes mode)
  where fullScreenValue = fmap (^.cvValue) vidFullScreenCVar
        getModes fullScreen
          | fullScreen /= 0 = maybe vidModesError return =<< use (vidGlobals.vgFSModes)
          | otherwise = use (vidGlobals.vgVidModes)
        vidModesError =
          do Com.fatalError "VID.getModeInfo vidGlobals.vgFSModes is Nothing"
             return V.empty

initModeList :: Quake ()
initModeList =
  do renderer <- use (globals.gRenderer)
     maybe rendererError proceedInitModeList renderer
  where rendererError = Com.fatalError "VID.initModeList renderer is Nothing"
        proceedInitModeList renderer =
          updateModes =<< renderer^.rRefExport.reGetModeList

updateModes :: V.Vector VideoMode -> Quake ()
updateModes modes =
  do vidGlobals.vgFSModes .= Just fsModes
     vidGlobals.vgFSResolutions .= fsResolutions
  where (fsResolutions, fsModes) = V.unzip (V.imap parseMode modes)

parseMode :: Int -> VideoMode -> (B.ByteString, VidModeT)
parseMode idx mode = (res', VidModeT m width height idx)
  where width = getVideoModeWidth mode
        height = getVideoModeHeight mode
        widthS = encode width
        heightS = encode height
        res = B.concat ["[", widthS, " ", heightS]
        len = B.length res
        resLine | len < 10 = res `B.append` BC.replicate (10 - len) ' '
                | otherwise = res
        res' = resLine `B.append` "]"
        m = B.concat ["Mode ", encode idx, widthS, "x", heightS]

modeInfo :: V.Vector VidModeT -> Int -> Maybe (Int, Int)
modeInfo modes mode
  | mode < 0 || mode > V.length modes = Nothing
  | otherwise = Just (m^.vmWidth, m^.vmHeight)
  where m = modes V.! mode

getVideoModeWidth :: VideoMode -> Int
getVideoModeWidth (GLFWbVideoMode mode) = GLFW.videoModeWidth mode

getVideoModeHeight :: VideoMode -> Int
getVideoModeHeight (GLFWbVideoMode mode) = GLFW.videoModeHeight mode
