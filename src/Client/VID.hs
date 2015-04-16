{-# LANGUAGE OverloadedStrings #-}
module Client.VID where

import Control.Lens ((.=), ix, (^.), zoom)
import Control.Monad (void, liftM)

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified Render.Renderer as Renderer

init :: Quake ()
init = do
    -- Create the video variables so we know how to start the graphics drivers
    void $ CVar.get "vid_ref" Renderer.getPreferredName Constants.cvarArchive
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
checkChanges = io (putStrLn "VID.checkChanges") >> undefined -- TODO
