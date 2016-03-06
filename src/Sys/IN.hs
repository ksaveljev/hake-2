module Sys.IN
  ( initialize
  , realINInit
  , shutdown
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import           QCommon.XCommandT (runXCommandT)
import           QuakeState
import           Types

import           Control.Lens ((.=))
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
shutdown = error "IN.shutdown" -- TODO

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
