module Client.VID
  ( initialize
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Render.QRenderer as QRenderer
import           Types

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
     error "VID.initialize" -- TODO
{-
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
    -}