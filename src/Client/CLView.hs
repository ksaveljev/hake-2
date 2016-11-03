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
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QCommon.NetChanT
import           QuakeState
import           Render.Renderer
import qualified Sys.Sys as Sys
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Applicative (liftA2)
import           Control.Lens (use, preuse, ix, (^.), (.=), (+=))
import           Control.Monad (join, unless, when)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import           Linear (V3(..))

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
       SCR.touchPics renderer
       Com.printf "                                     \r"
       CLTEnt.registerTEntModels
       clientGlobals.cgNumCLWeaponModels .= 1
       clientGlobals.cgWeaponModels.ix 0 .= "weapon.md2"
       mapM_ (registerModel renderer configStrings) [1..Constants.maxImages-1]
       Com.printf "images\r"
       SCR.updateScreen
       mapM_ (registerImage renderer configStrings) [1..Constants.maxImages-1]
       Com.printf "                                     \r"
       mapM_ (processClient configStrings) [0..Constants.maxClients-1]
       CLParse.loadClientInfo (globals.gCl.csBaseClientInfo) "unnamed\\male/grunt"
       -- set sky textures and speed
       Com.printf "sky\r"
       SCR.updateScreen
       setSky renderer configStrings
       Com.printf "                                     \r"
       renderer^.rRefExport.reEndRegistration
       Console.clearNotify
       SCR.updateScreen
       globals.gCl.csRefreshPrepped .= True
       globals.gCl.csForceRefDef .= True
  where mapName = B.drop 5 (B.take (B.length str - 4) str) -- skip "maps/" and cut off ".bsp"


registerModel :: Renderer -> V.Vector B.ByteString -> Int -> Quake ()
registerModel renderer configStrings idx
  | B.null fullName = return ()
  | otherwise =
      do when (name `BC.index` 0 /= '*') $
           Com.printf (name `B.append` "\r")
         SCR.updateScreen
         Sys.sendKeyEvents -- pump message loop
         doRegisterModel renderer fullName idx
         when (name `BC.index` 0 /= '*') $
           Com.printf "                                     \r"
  where fullName = configStrings V.! (Constants.csModels + idx)
        name | B.length fullName > 37 = B.take 36 fullName
             | otherwise = fullName

doRegisterModel :: Renderer -> B.ByteString -> Int -> Quake ()
doRegisterModel renderer fullName idx
  | fullName `BC.index` 0 == '#' =
      do numWeaponModels <- use (clientGlobals.cgNumCLWeaponModels)
         when (numWeaponModels < Constants.maxClientWeaponModels) $
           do clientGlobals.cgWeaponModels.ix numWeaponModels .= B.drop 1 fullName
              clientGlobals.cgNumCLWeaponModels += 1
  | otherwise =
      do modelRef <- (renderer^.rRefExport.reRegisterModel) fullName
         globals.gCl.csModelDraw.ix idx .= modelRef
         setModelClip fullName idx

setModelClip :: B.ByteString -> Int -> Quake ()
setModelClip fullName idx
  | fullName `BC.index` 0 == '*' =
      do inlineModelRef <- CM.inlineModel fullName
         globals.gCl.csModelClip.ix idx .= Just inlineModelRef
  | otherwise = globals.gCl.csModelClip.ix idx .= Nothing

registerImage :: Renderer -> V.Vector B.ByteString -> Int -> Quake ()
registerImage renderer configStrings idx
  | B.null configString = return ()
  | otherwise =
      do picRef <- (renderer^.rRefExport.reRegisterPic) configString
         globals.gCl.csImagePrecache.ix idx .= picRef
         Sys.sendKeyEvents -- pump message loop
  where configString = configStrings V.! (Constants.csImages + idx)

processClient :: V.Vector B.ByteString -> Int -> Quake ()
processClient configStrings idx
  | B.null configString = return ()
  | otherwise =
      do Com.printf (B.concat ["client ", encode idx, "\r"])
         SCR.updateScreen
         Sys.sendKeyEvents
         CLParse.parseClientInfo idx
         Com.printf "                                     \r"
  where configString = configStrings V.! (Constants.csPlayerSkins + idx)

setSky :: Renderer -> V.Vector B.ByteString -> Quake ()
setSky renderer configStrings =
  (renderer^.rRefExport.reSetSky) (configStrings V.! Constants.csSky) rotate axis
  where rotate = Lib.atof (configStrings V.! Constants.csSkyRotate)
        (a:b:c:_) = BC.split ' ' (configStrings V.! Constants.csSkyAxis)
        axis = fmap Lib.atof (V3 a b c)