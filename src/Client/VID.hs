module Client.VID
  ( initialize
  ) where

import           Client.VidModeT
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import           QuakeState
import qualified Render.QRenderer as QRenderer
import           Types

import           Control.Lens (ix, (.=))
import qualified Data.ByteString as B

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
checkChanges = error "VID.checkChanges"
