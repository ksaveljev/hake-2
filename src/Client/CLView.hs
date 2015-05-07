{-# LANGUAGE OverloadedStrings #-}
module Client.CLView where

import Control.Lens ((^.), use, preuse, ix, (.=))
import Control.Monad (liftM, unless)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Client.CLParse as CLParse
import qualified Client.CLTEnt as CLTEnt
import {-# SOURCE #-} qualified Client.Console as Console
import qualified Client.SCR as SCR
import qualified QCommon.Com as Com

{-
- =================
- 
- CL_PrepRefresh
- 
- Call before entering a new level, or after changing dlls
- =================
-}
prepRefresh :: Quake ()
prepRefresh = do
    Just str <- preuse $ globals.cl.csConfigStrings.ix (Constants.csModels + 1)
    let len = B.length str

            -- no map loaded
    unless (len == 0) $ do
      vidDef' <- use $ globals.vidDef
      configStrings <- use $ globals.cl.csConfigStrings
      Just renderer <- use $ globals.re

      SCR.addDirtyPoint 0 0
      SCR.addDirtyPoint ((vidDef'^.vdWidth) - 1) ((vidDef'^.vdHeight) - 1)

      -- let the render dll load the map
      let mapName = B.drop 5 (B.take (len - 4) str) -- skip "maps/" and cut off ".bsp"

      -- register models, pics, and skins
      Com.printf $ "Map: " `B.append` mapName `B.append` "\r"
      SCR.updateScreen
      (renderer^.rRefExport.reBeginRegistration) mapName
      Com.printf "                                     \r"

      -- precache status bar pics
      Com.printf "pics\r"
      SCR.updateScreen
      SCR.touchPics
      Com.printf "                                     \r"

      CLTEnt.registerTEntModels

      clientGlobals.cgNumCLWeaponModels .= 1
      clientGlobals.cgWeaponModels.ix 0 .= "weapon.md2"

      registerModels configStrings 1 Constants.maxModels
      
      Com.printf "images\r"
      SCR.updateScreen

      registerImages configStrings 1 Constants.maxImages

      Com.printf "                                     \r"
      processClients configStrings 0 Constants.maxClients

      CLParse.loadClientInfo (globals.cl.csBaseClientInfo) "unnamed\\male/grunt"

      -- set sky textures and speed
      Com.printf "sky\r"
      SCR.updateScreen

      setSky configStrings

      Com.printf "                                     \r"

      -- the renderer can now free unneeded stuff
      renderer^.rRefExport.reEndRegistration

      -- clear any lines of console text
      Console.clearNotify

      SCR.updateScreen
      globals.cl.csRefreshPrepped .= True
      globals.cl.csForceRefDef .= True -- make sure we have a valid refdef

  where registerModels :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        registerModels _ _ _ = do
          io (putStrLn "CLView.prepRefresh#registerModels") >> undefined -- TODO

        registerImages :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        registerImages _ _ _ = do
          io (putStrLn "CLView.prepRefresh#registerImages") >> undefined -- TODO

        processClients :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        processClients _ _ _ = do
          io (putStrLn "CLView.prepRefresh#processClients") >> undefined -- TODO

        setSky :: V.Vector B.ByteString -> Quake ()
        setSky _ = do
          io (putStrLn "CLView.prepRefresh#setSky") >> undefined -- TODO

addNetGraph :: Quake ()
addNetGraph = do
    -- if using the debuggraph for something else, don't
    -- add the net lines
    debugGraphValue <- liftM (^.cvValue) scrDebugGraphCVar
    timeGraphValue <- liftM (^.cvValue) scrTimeGraphCVar

    unless (debugGraphValue == 0 || timeGraphValue == 0) $ do
      dropped <- use $ globals.cls.csNetChan.ncDropped
      mapM_ (\_ -> SCR.debugGraph 30 0x40) [0..dropped-1]

      surpressCount <- use $ globals.cl.csSurpressCount
      mapM_ (\_ -> SCR.debugGraph 30 0xDF) [0..surpressCount-1]

      -- see what the latency was on this packet
      inAck <- use $ globals.cls.csNetChan.ncIncomingAcknowledged
      let idx = inAck .&. (Constants.cmdBackup - 1)

      realTime <- use $ globals.cls.csRealTime
      Just time <- preuse $ globals.cl.csCmdTime.ix idx
      let ping = (realTime - time) `div` 30
          ping' = if ping > 30 then 30 else ping

      SCR.debugGraph (fromIntegral ping') 0xD0
