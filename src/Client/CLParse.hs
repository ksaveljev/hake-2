{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CLParse where

import Control.Lens (use, (^.), (.=), preuse, ix)
import Control.Monad (when, liftM, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import qualified Client.CLFX as CLFX
import qualified Client.CLView as CLView
import {-# SOURCE #-} qualified Client.SCR as SCR
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified Sound.S as S

downloadF :: XCommandT
downloadF = io (putStrLn "CLParse.downloadF") >> undefined -- TODO

showNet :: B.ByteString -> Quake ()
showNet str = do
    showNetValue <- liftM (^.cvValue) clShowNetCVar
    when (showNetValue >= 2) $ do
      readCount <- use $ globals.netMessage.sbReadCount
      Com.printf $ BC.pack (show (readCount - 1)) `B.append` ":" `B.append` str `B.append` "\n"

parseServerMessage :: Quake ()
parseServerMessage = do
    parseMessage

    CLView.addNetGraph

    -- we don't know if it is ok to save a demo message until
    -- after we have parsed the frame
    demoRecording <- use $ globals.cls.csDemoRecording
    demoWaiting <- use $ globals.cls.csDemoWaiting
    when (demoRecording && not demoWaiting) $
      CL.writeDemoMessage

  where parseMessage :: Quake ()
        parseMessage = do
          nm <- use $ globals.netMessage

          if (nm^.sbReadCount) > (nm^.sbCurSize)
            then Com.comError Constants.errFatal "CL_ParseServerMessage: Bad server message:"
            else do
              cmd <- MSG.readByte (globals.netMessage)

              if cmd == -1
                then showNet "END OF MESSAGE"
                else do
                  showNetValue <- liftM (^.cvValue) clShowNetCVar

                  when (showNetValue >= 2) $ do
                    io (putStrLn "CLParse.parseServerMessage#parseMessage") >> undefined -- TODO

                  if | cmd == Constants.svcNop -> return ()

                     | cmd == Constants.svcDisconnect -> Com.comError Constants.errDisconnect "Server disconnected\n"

                     | cmd == Constants.svcReconnect -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcReconnect") >> undefined -- TODO

                     | cmd == Constants.svcPrint -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcPrint") >> undefined -- TODO

                     | cmd == Constants.svcCenterPrint -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcCenterPrint") >> undefined -- TODO

                     | cmd == Constants.svcStuffText -> do
                         str <- MSG.readString (globals.netMessage)
                         Com.dprintf $ "stufftext: " `B.append` str `B.append` "\n"
                         CBuf.addText str

                     | cmd == Constants.svcServerData -> do
                         CBuf.execute -- make sure any stuffed commands are done
                         parseServerData

                     | cmd == Constants.svcConfigString -> parseConfigString

                     | cmd == Constants.svcSound -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcSound") >> undefined -- TODO

                     | cmd == Constants.svcSpawnBaseline -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcSpawnBaseline") >> undefined -- TODO

                     | cmd == Constants.svcTempEntity -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcTempEntity") >> undefined -- TODO

                     | cmd == Constants.svcMuzzleFlash -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcMuzzleFlash") >> undefined -- TODO

                     | cmd == Constants.svcMuzzleFlash2 -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcMuzzleFlash2") >> undefined -- TODO

                     | cmd == Constants.svcDownload -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcDownload") >> undefined -- TODO

                     | cmd == Constants.svcFrame -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcFrame") >> undefined -- TODO

                     | cmd == Constants.svcInventory -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcInventory") >> undefined -- TODO

                     | cmd == Constants.svcLayout -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcLayout") >> undefined -- TODO

                     | cmd == Constants.svcPlayerInfo || cmd == Constants.svcPacketEntities || cmd == Constants.svcDeltaPacketEntities -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcFinalErrorSmth") >> undefined -- TODO

                     | otherwise -> io (putStrLn "CLParse.otherwise") >> undefined -- TODO

                  parseMessage

parseServerData :: Quake ()
parseServerData = do
    -- wite the client_state_t struct
    CL.clearState
    globals.cls.csState .= Constants.caConnected

    -- parse protocol version number
    version <- MSG.readLong (globals.netMessage)
    globals.cls.csServerProtocol .= version

    -- BIG HACK to let demos from release work with the 3.0x patch!!
    use (globals.serverState) >>= \state ->
      if state /= 0 && Constants.protocolVersion == 34
        then return ()
        else if version /= Constants.protocolVersion
               then Com.comError Constants.errDrop $ "Server returned version " `B.append` BC.pack (show version) `B.append`
                                                     ", not " `B.append` BC.pack (show Constants.protocolVersion)
               else return ()

    serverCount <- MSG.readLong (globals.netMessage)
    attractLoop <- MSG.readByte (globals.netMessage)
    globals.cl.csServerCount .= serverCount
    globals.cl.csAttractLoop .= if attractLoop /= 0 then True else False

    -- game directory
    str <- MSG.readString (globals.netMessage)
    globals.cl.csGameDir .= str
    Com.dprintf ("gamedir=" `B.append` str `B.append` "\n")

    -- set gamedir
    gameDirVar <- liftM (^.cvString) fsGameDirVarCVar
    when (B.length str > 0 && (B.length gameDirVar == 0 || str == gameDirVar) || (B.length str == 0 && B.length gameDirVar == 0)) $
      void $ CVar.set "game" str

    -- parse player entity number
    playerNum <- MSG.readShort (globals.netMessage)
    globals.cl.csPlayerNum .= playerNum
    Com.dprintf $ "numplayers=" `B.append` BC.pack (show playerNum) `B.append` "\n"
    -- get the full level name
    levelName <- MSG.readString (globals.netMessage)
    Com.dprintf $ "levelname=" `B.append` levelName `B.append` "\n"

    if playerNum == -1 -- playing a cinematic or showing a pic, not a level
      then SCR.playCinematic levelName
      else do
        Com.printf $ "Levelname:" `B.append` levelName `B.append` "\n"
        globals.cl.csRefreshPrepped .= False

parseConfigString :: Quake ()
parseConfigString = do
    i <- MSG.readShort (globals.netMessage)

    when (i < 0 || i >= Constants.maxConfigStrings) $
      Com.comError Constants.errDrop "configstring > MAX_CONFIGSTRINGS"

    str <- MSG.readString (globals.netMessage)

    Just oldStr <- preuse $ globals.cl.csConfigStrings.ix i
    globals.cl.csConfigStrings.ix i .= str

    refreshPrepped <- use $ globals.cl.csRefreshPrepped

    if | i >= Constants.csLights && i < Constants.csLights + Constants.maxLightStyles ->
           CLFX.setLightStyle (i - Constants.csLights)

       | i >= Constants.csModels && i < Constants.csModels + Constants.maxModels -> do
           when refreshPrepped $ do
             Just renderer <- use $ globals.re
             model <- (renderer^.rRefExport.reRegisterModel) str
             globals.cl.csModelDraw.ix (i - Constants.csModels) .= model

             if B.take 1 str == "*"
               then do
                 idx <- CM.inlineModel str
                 globals.cl.csModelClip.ix (i - Constants.csModels) .= Just idx
               else
                 globals.cl.csModelClip.ix (i - Constants.csModels) .= Nothing

       | i >= Constants.csSounds && i < Constants.csSounds + Constants.maxSounds -> do
           when refreshPrepped $ do
             sound <- S.registerSound str
             globals.cl.csSoundPrecache.ix (i - Constants.csSounds) .= sound

       | i >= Constants.csImages && i < Constants.csImages + Constants.maxImages -> do
           when refreshPrepped $ do
             Just renderer <- use $ globals.re
             image <- (renderer^.rRefExport.reRegisterPic) str
             globals.cl.csImagePrecache.ix (i - Constants.csImages) .= image

       | i >= Constants.csPlayerSkins && i < Constants.csPlayerSkins + Constants.maxClients -> do
           when (refreshPrepped && not (oldStr == str)) $
             parseClientInfo (i - Constants.csPlayerSkins)

       | otherwise -> return ()

{-
- ================ CL_ParseClientinfo
- 
- Load the skin, icon, and model for a client ================
-}
parseClientInfo :: Int -> Quake ()
parseClientInfo player = do
    Just str <- preuse $ globals.cl.csConfigStrings.ix (player + Constants.csPlayerSkins)
    clientInfo <- loadClientInfo str
    globals.cl.csClientInfo.ix player .= clientInfo

loadClientInfo :: B.ByteString -> Quake ClientInfoT
loadClientInfo str = do
    io (putStrLn "CLParse.loadClientInfo") >> undefined -- TODO
