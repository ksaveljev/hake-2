{-# LANGUAGE OverloadedStrings #-}
module Client.VID where

import Control.Lens ((.=), ix, (^.), zoom, use)
import Control.Monad (void, liftM, when, unless)
import Data.Maybe (isJust, isNothing)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.Console as Console
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Render.Renderer as Renderer
import qualified Sound.S as S
import qualified Sys.IN as IN

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
            let renderer = if (vidRef^.cvString) == Renderer.getPreferredName
                             -- try the default renderer as fallback after preferred
                             then Renderer.getDefaultName
                             -- try the preferred renderer as first fallback
                             else Renderer.getPreferredName

            renderer' <- if (vidRef^.cvString) == Renderer.getDefaultName
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

    let found = elem name Renderer.getDriverNames

    if not found
      then do
        Com.printf $ "LoadLibrary(\"" `B.append` name `B.append` "\") failed\n"
        return False
      else do
        Com.printf $ "LoadLibrary(\"" `B.append` name `B.append` "\")\n"
        let r = Renderer.getDriver name fast
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
