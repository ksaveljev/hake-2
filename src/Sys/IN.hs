module Sys.IN
  ( centerView
  , commands
  , frame
  , initialize
  , realINInit
  , shutdown
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.RefExportT
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Render.Renderer
import           Sys.KBD
import           Types

import           Control.Lens (use, (^.), (.=))
import           Control.Monad (when)
import qualified Data.ByteString as B

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars = [ ("in_mouse", "1", Constants.cvarArchive)
               , ("in_joystick", "0", Constants.cvarArchive)
               ]

otherInitialCVars :: [(B.ByteString, B.ByteString, Int)]
otherInitialCVars = [ ("m_filter", "0", 0)
                    , ("in_mouse", "1", Constants.cvarArchive)
                    , ("freelook", "1", 0)
                    , ("lookstrafe", "0", 0)
                    , ("sensitivity", "3", 0)
                    , ("m_pitch", "0.022", 0)
                    , ("m_yaw", "0.022", 0)
                    , ("m_forward", "1", 0)
                    , ("m_side", "0.8", 0)
                    ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands = [ ("+mlook", Just mLookDown)
                  , ("-mlook", Just mLookUp)
                  , ("force_centerview", Just forceCenterViewF)
                  , ("togglemouse", Just toggleMouse)
                  ]

initialize :: Quake ()
initialize = CVar.initializeCVars initialCVars

shutdown :: Quake ()
shutdown = inGlobals.inMouseAvail .= False

realINInit :: Quake ()
realINInit =
  do CVar.initializeCVars otherInitialCVars
     Cmd.addInitialCommands initialCommands
     inGlobals.inMouseAvail .= True

mLookDown :: XCommandT
mLookDown = XCommandT "IN.mLookDown" (inGlobals.inMLooking .= True)

mLookUp :: XCommandT
mLookUp = XCommandT "IN.mLookUp" $
  do inGlobals.inMLooking .= False
     runXCommandT centerView

forceCenterViewF :: XCommandT
forceCenterViewF = error "IN.forceCenterViewF" -- TODO

toggleMouse :: XCommandT
toggleMouse = error "IN.toggleMouse" -- TODO

centerView :: XCommandT
centerView = error "IN.centerView" -- TODO

frame :: Quake ()
frame =
  do cl <- use (globals.gCl)
     keyDest <- use (globals.gCls.csKeyDest)
     runFrame cl keyDest
  where runFrame cl keyDest
          | not (cl^.csCinematicPaletteActive) && (not (cl^.csRefreshPrepped) || keyDest == Constants.keyConsole || keyDest == Constants.keyMenu) =
              deactivateMouse
          | otherwise = activateMouse

activateMouse :: Quake ()
activateMouse =
  do mouseAvail <- use (inGlobals.inMouseAvail)
     mouseActive <- use (inGlobals.inMouseActive)
     when (mouseAvail && not mouseActive) $
       do kbdGlobals.kbdMx .= 0
          kbdGlobals.kbdMy .= 0
          installGrabs
          inGlobals.inMouseActive .= True

deactivateMouse :: Quake ()
deactivateMouse =
  do mouseActive <- use (inGlobals.inMouseActive)
     when mouseActive $
       do uninstallGrabs
          inGlobals.inMouseActive .= False
          
installGrabs :: Quake ()
installGrabs =
  do renderer <- use (globals.gRenderer)
     maybe rendererError doInstallGrabs renderer
  where rendererError = error "IN.installGrabs renderer is Nothing"
        doInstallGrabs renderer =
          do renderer^.rRefExport.reGetKeyboardHandler.kbdInstallGrabs
             inGlobals.inIgnoreFirst .= True

uninstallGrabs :: Quake ()
uninstallGrabs =
  do renderer <- use (globals.gRenderer)
     maybe rendererError doUninstallGrabs renderer
  where rendererError = error "IN.uninstallGrabs renderer is Nothing"
        doUninstallGrabs renderer = renderer^.rRefExport.reGetKeyboardHandler.kbdUninstallGrabs

commands :: Quake ()
commands = error "IN.commands" -- TODO