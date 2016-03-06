module Client.VID
  ( initialize
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.Console as Console
import           Client.RefExportT
import           Client.VidDefT
import           Client.VidModeT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import qualified Render.QRenderer as QRenderer
import           Render.Renderer
import qualified Sound.S as S
import qualified Sys.IN as IN
import           Sys.KBD
import           Types

import           Control.Lens (use, ix, (.=), (%=), (^.), (&), (.~))
import           Control.Monad (when, unless, void)
import qualified Data.ByteString as B
import qualified Data.Vector as V

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars = [ ("vid_ref", QRenderer.getPreferredName, Constants.cvarArchive)
               , ("vid_xpos", "3", Constants.cvarArchive)
               , ("vid_ypos", "22", Constants.cvarArchive)
               , ("vid_width", "640", Constants.cvarArchive)
               , ("vid_height", "480", Constants.cvarArchive)
               , ("vid_fullscreen", "0", Constants.cvarArchive)
               , ("vid_gamma", "1", Constants.cvarArchive)
               ]

initialize :: Quake ()
initialize =
  do CVar.initializeCVars initialCVars
     vidGlobals.vgVidModes.ix 11.vmWidth .= 640
     vidGlobals.vgVidModes.ix 11.vmHeight .= 480
     Cmd.addCommand "vid_restart" (Just restartF)
     checkChanges

restartF :: XCommandT
restartF = error "VID.restartF"

checkChanges :: Quake ()
checkChanges =
  do vidDef <- use (globals.gVidDef)
     globals.gVidDef %= (\v -> v & vdWidth .~ (vidDef^.vdNewWidth)
                                & vdHeight .~ (vidDef^.vdNewHeight))
     changeRefresh =<< vidRefCVar

changeRefresh :: CVarT -> Quake ()
changeRefresh vidRef
  | vidRef^.cvModified =
      do S.stopAllSound
         updateState =<< vidFullScreenCVar
         loaded <- loadRefresh (vidRef^.cvString) True
         unless loaded $
           loadPreferredOrDefault vidRef
         globals.gCls.csDisableScreen .= 0 -- False
  | otherwise = return ()
  where updateState vidFullScreen =
          do CVar.update (vidRef & cvModified .~ False)
             CVar.update (vidFullScreen & cvModified .~ True)
             globals.gCl.csRefreshPrepped .= False
             globals.gCls.csDisableScreen .= 1 -- True

loadRefresh :: B.ByteString -> Bool -> Quake Bool
loadRefresh name fast =
  do freeActiveRefLib name =<< use (vidGlobals.vgRefLibActive)
     Com.printf (B.concat ["------- Loading ", name, " -------\n"])
     loadLibrary name fast driverFound
  where driverFound = V.elem name QRenderer.getDriverNames

freeActiveRefLib :: B.ByteString -> Bool -> Quake ()
freeActiveRefLib _ False = return ()
freeActiveRefLib name True =
  maybe (rendererError name) freeLib =<< use (globals.gRenderer)
  where freeLib renderer =
          do renderer^.rRefExport.reGetKeyboardHandler.kbdClose
             IN.shutdown
             renderer^.rRefExport.reShutDown
             freeRefLib

rendererError :: B.ByteString -> Quake ()
rendererError name = Com.fatalError (name `B.append` " can't load but registered")

freeRefLib :: Quake ()
freeRefLib =
  do shutItDown =<< use (globals.gRenderer)
     globals.gRenderer .= Nothing
     vidGlobals.vgRefLibActive .= False
  where shutItDown Nothing = return ()
        shutItDown (Just renderer) =
          do renderer^.rRefExport.reGetKeyboardHandler.kbdClose
             IN.shutdown

loadLibrary :: B.ByteString -> Bool -> Bool -> Quake Bool
loadLibrary name _ False =
  do Com.printf (B.concat ["LoadLibrary(\"", name, "\") failed\n"])
     return False
loadLibrary name fast True =
  do Com.printf (B.concat ["LoadLibrary(\"", name, "\")\n"])
     globals.gRenderer .= renderer
     maybe missingRenderer (proceedLoadLibrary name) renderer
  where renderer = QRenderer.getDriver name fast
        missingRenderer =
          do rendererError name
             return False

proceedLoadLibrary :: B.ByteString -> Renderer -> Quake Bool
proceedLoadLibrary name renderer
  | renderer^.rRefExport.reApiVersion /= Constants.apiVersion =
      do freeRefLib
         Com.fatalError (name `B.append` " has incompatibe api_version")
         return False
  | otherwise =
      do IN.realINInit
         xpos <- fmap (truncate . (^.cvValue)) vidXPosCVar
         ypos <- fmap (truncate . (^.cvValue)) vidYPosCVar
         (renderer^.rRefExport.reInit) xpos ypos >>= finishLoadLibrary renderer

finishLoadLibrary :: Renderer -> Bool -> Quake Bool
finishLoadLibrary renderer False =
  do renderer^.rRefExport.reShutDown
     freeRefLib
     return False
finishLoadLibrary renderer True =
  do renderer^.rRefExport.reGetKeyboardHandler.kbdInit
     Com.printf "------------------------------------\n"
     vidGlobals.vgRefLibActive .= True
     return True

loadPreferredOrDefault :: CVarT -> Quake ()
loadPreferredOrDefault vidRef =
  do void (CVar.set "vid_ref" =<< pickRenderer)
     keyDest <- use (globals.gCls.csKeyDest)
     when (keyDest /= Constants.keyConsole) $
       runXCommandT Console.toggleConsoleF
  where rendererToTry
          | (vidRef^.cvString) == QRenderer.getPreferredName = QRenderer.getDefaultName
          | otherwise = QRenderer.getPreferredName
        pickRenderer
          | (vidRef^.cvString) == QRenderer.getDefaultName =
              do Com.printf "Refresh failed\n"
                 CVar.get "gl_mode" "0" 0 >>= tryMode vidRef
                 return (vidRef^.cvString)
          | otherwise = return rendererToTry

tryMode :: CVarT -> Maybe CVarT -> Quake ()
tryMode _ Nothing = Com.fatalError "VID.tryMode glModeCVar is Nothing"
tryMode vidRef (Just glMode)
  | (glMode^.cvValue) /= 0 =
      do Com.printf "Trying mode 0\n"
         CVar.setValueF "gl_mode" 0
         loaded <- loadRefresh (vidRef^.cvString) False
         unless loaded fallBackError
  | otherwise = fallBackError
  where fallBackError = Com.fatalError (B.concat ["Couldn't fall back to ", vidRef^.cvString, " refresh!"])

