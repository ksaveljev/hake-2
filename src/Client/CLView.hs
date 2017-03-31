{-# LANGUAGE OverloadedStrings #-}
module Client.CLView where

import Control.Lens ((^.), use, preuse, ix, (.=), (+=))
import Control.Monad (liftM, unless, when)
import Data.Bits ((.&.))
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Types
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Client.CLParse as CLParse
import qualified Client.CLTEnt as CLTEnt
import {-# SOURCE #-} qualified Client.Console as Console
import qualified Client.SCR as SCR
import qualified QCommon.CM as CM
import {-# SOURCE #-} qualified QCommon.Com as Com
import {-# SOURCE #-} qualified Sys.Sys as Sys
import qualified Util.Lib as Lib

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
    Just str <- preuse $ globals.gCl.csConfigStrings.ix (Constants.csModels + 1)
    let len = B.length str

            -- no map loaded
    unless (len == 0) $ do
      vidDef' <- use $ globals.gVidDef
      configStrings <- use $ globals.gCl.csConfigStrings
      Just renderer <- use $ globals.gRenderer

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

      CLParse.loadClientInfo (globals.gCl.csBaseClientInfo) "unnamed\\male/grunt"

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
      globals.gCl.csRefreshPrepped .= True
      globals.gCl.csForceRefDef .= True -- make sure we have a valid refdef

  where registerModels :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        registerModels configStrings idx maxIdx
          | idx >= maxIdx || B.length (configStrings V.! (Constants.csModels + idx)) == 0 = return ()
          | otherwise = do
              let fullName = configStrings V.! (Constants.csModels + idx)
                  name = if B.length fullName > 37
                           then B.take 36 fullName
                           else configStrings V.! (Constants.csModels + idx)

              when (name `BC.index` 0 /= '*') $
                Com.printf (name `B.append` "\r")

              SCR.updateScreen
              Sys.sendKeyEvents -- pump message loop
              
              if name `BC.index` 0 == '#'
                then do -- special player weapon model
                  numWeaponModels <- use $ clientGlobals.cgNumCLWeaponModels
                  when (numWeaponModels < Constants.maxClientWeaponModels) $ do
                    clientGlobals.cgWeaponModels.ix numWeaponModels .= B.drop 1 fullName
                    clientGlobals.cgNumCLWeaponModels += 1
                else do
                  Just renderer <- use $ globals.gRenderer
                  modelRef <- (renderer^.rRefExport.reRegisterModel) fullName
                  globals.gCl.csModelDraw.ix idx .= modelRef

                  if name `BC.index` 0 == '*'
                    then do
                      inlineModelRef <- CM.inlineModel fullName
                      globals.gCl.csModelClip.ix idx .= Just inlineModelRef
                    else
                      globals.gCl.csModelClip.ix idx .= Nothing

              when (name `BC.index` 0 /= '*') $
                Com.printf "                                     \r"

              registerModels configStrings (idx + 1) maxIdx

        registerImages :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        registerImages configStrings idx maxIdx
          | idx >= maxIdx || B.length (configStrings V.! (Constants.csImages + idx)) == 0 = return ()
          | otherwise = do
              Just renderer <- use $ globals.gRenderer
              picRef <- (renderer^.rRefExport.reRegisterPic) (configStrings V.! (Constants.csImages + idx))
              globals.gCl.csImagePrecache.ix idx .= picRef
              Sys.sendKeyEvents -- pump message loop
              registerImages configStrings (idx + 1) maxIdx

        processClients :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        processClients configStrings idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              if B.length (configStrings V.! (Constants.csPlayerSkins + idx)) == 0
                then processClients configStrings (idx + 1) maxIdx
                else do
                  Com.printf ("client " `B.append` BC.pack (show idx) `B.append` "\r") -- IMPROVE?
                  SCR.updateScreen
                  Sys.sendKeyEvents
                  CLParse.parseClientInfo idx
                  Com.printf "                                     \r"
                  processClients configStrings (idx + 1) maxIdx

        setSky :: V.Vector B.ByteString -> Quake ()
        setSky configStrings = do
          let rotate = Lib.atof (configStrings V.! Constants.csSkyRotate)
              (a:b:c:_) = BC.split ' ' (configStrings V.! Constants.csSkyAxis)
              axis = V3 (Lib.atof a) (Lib.atof b) (Lib.atof c)
          Just renderer <- use $ globals.gRenderer
          (renderer^.rRefExport.reSetSky) (configStrings V.! Constants.csSky) rotate axis

addNetGraph :: Quake ()
addNetGraph = do
    -- if using the debuggraph for something else, don't
    -- add the net lines
    debugGraphValue <- liftM (^.cvValue) scrDebugGraphCVar
    timeGraphValue <- liftM (^.cvValue) scrTimeGraphCVar

    unless (debugGraphValue == 0 || timeGraphValue == 0) $ do
      dropped <- use $ globals.gCls.csNetChan.ncDropped
      mapM_ (\_ -> SCR.debugGraph 30 0x40) [0..dropped-1]

      surpressCount <- use $ globals.gCl.csSurpressCount
      mapM_ (\_ -> SCR.debugGraph 30 0xDF) [0..surpressCount-1]

      -- see what the latency was on this packet
      inAck <- use $ globals.gCls.csNetChan.ncIncomingAcknowledged
      let idx = inAck .&. (Constants.cmdBackup - 1)

      realTime <- use $ globals.gCls.csRealTime
      Just time <- preuse $ globals.gCl.csCmdTime.ix idx
      let ping = (realTime - time) `div` 30
          ping' = if ping > 30 then 30 else ping

      SCR.debugGraph (fromIntegral ping') 0xD0
