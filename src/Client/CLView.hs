module Client.CLView
  ( addNetGraph
  , prepRefresh
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLParseShared as CLParse
import qualified Client.CLTEnt as CLTEnt
import qualified Client.Console as Console
import           Client.RefExportT
import qualified Client.SCR as SCR
import           Client.VidDefT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QCommon.NetChanT
import           QuakeState
import           Render.Renderer
import           Types

import           Control.Applicative (liftA2)
import           Control.Lens (use, preuse, ix, (^.), (.=))
import           Control.Monad (join, unless)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.Vector as V

addNetGraph :: Quake ()
addNetGraph = join (liftA2 proceedAddNetGraph scrDebugGraphCVar scrTimeGraphCVar)
  where proceedAddNetGraph debugGraph timeGraph
          | (debugGraph^.cvValue) == 0 || (timeGraph^.cvValue) == 0 = return ()
          | otherwise =
              do dropped <- use (globals.gCls.csNetChan.ncDropped)
                 mapM_ (const (SCR.debugGraph 30 0x40)) [0..dropped-1]
                 surpressCount <- use (globals.gCl.csSurpressCount)
                 mapM_ (const (SCR.debugGraph 30 0xDF)) [0..surpressCount-1]
                 idx <- fmap (.&. (Constants.cmdBackup - 1)) (use (globals.gCls.csNetChan.ncIncomingAcknowledged))
                 realTime <- use (globals.gCls.csRealTime)
                 time <- preuse (globals.gCl.csCmdTime.ix idx)
                 maybe timeError (doAddNetGraph realTime) time
        timeError = Com.fatalError "CLView.addNetGraph time is Nothing"
        doAddNetGraph realTime time =
          SCR.debugGraph (fromIntegral (min ((realTime - time `div` 30)) 30)) 0xD0

prepRefresh :: Quake ()
prepRefresh =
  do str <- preuse (globals.gCl.csConfigStrings.ix (Constants.csModels + 1))
     renderer <- use (globals.gRenderer)
     proceedPrepRefresh str renderer

proceedPrepRefresh :: Maybe B.ByteString -> Maybe Renderer -> Quake ()
proceedPrepRefresh Nothing _ = Com.fatalError "CLView.prepRefresh str is Nothing"
proceedPrepRefresh _ Nothing = Com.fatalError "CLView.prepRefresh renderer is Nothing"
proceedPrepRefresh (Just str) (Just renderer) =
  unless (B.length str == 0) $
    do vidDef <- use (globals.gVidDef)
       configStrings <- use (globals.gCl.csConfigStrings)
       SCR.addDirtyPoint 0 0
       SCR.addDirtyPoint ((vidDef^.vdWidth) - 1) ((vidDef^.vdHeight) - 1)
       -- register models, pics, and skins
       Com.printf (B.concat ["Map: ", mapName, "\r"])
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
       renderer^.rRefExport.reEndRegistration
       Console.clearNotify
       SCR.updateScreen
       globals.gCl.csRefreshPrepped .= True
       globals.gCl.csForceRefDef .= True
  where mapName = B.drop 5 (B.take (B.length str - 4) str) -- skip "maps/" and cut off ".bsp"


registerModels :: V.Vector B.ByteString -> Int -> Int -> Quake ()
registerModels = error "CLView.registerModels" -- TODO

registerImages :: V.Vector B.ByteString -> Int -> Int -> Quake ()
registerImages = error "CLView.registerImages" -- TODO

processClients :: V.Vector B.ByteString -> Int -> Int -> Quake ()
processClients = error "CLView.processClients" -- TODO

setSky :: V.Vector B.ByteString -> Quake ()
setSky = error "CLView.setSky" -- TODO

{-

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
                  Just renderer <- use $ globals.re
                  modelRef <- (renderer^.rRefExport.reRegisterModel) fullName
                  globals.cl.csModelDraw.ix idx .= modelRef

                  if name `BC.index` 0 == '*'
                    then do
                      inlineModelRef <- CM.inlineModel fullName
                      globals.cl.csModelClip.ix idx .= Just inlineModelRef
                    else
                      globals.cl.csModelClip.ix idx .= Nothing

              when (name `BC.index` 0 /= '*') $
                Com.printf "                                     \r"

              registerModels configStrings (idx + 1) maxIdx

        registerImages :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        registerImages configStrings idx maxIdx
          | idx >= maxIdx || B.length (configStrings V.! (Constants.csImages + idx)) == 0 = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              picRef <- (renderer^.rRefExport.reRegisterPic) (configStrings V.! (Constants.csImages + idx))
              globals.cl.csImagePrecache.ix idx .= picRef
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
          Just renderer <- use $ globals.re
          (renderer^.rRefExport.reSetSky) (configStrings V.! Constants.csSky) rotate axis
          -}