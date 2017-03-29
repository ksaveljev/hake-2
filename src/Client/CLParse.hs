{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Client.CLParse where

import Control.Exception (handle, IOException)
import Control.Lens (use, (^.), (.=), preuse, ix, zoom, (+=), Traversal')
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), shiftR)
import Data.Char (toLower)
import Data.IORef (IORef)
import Data.Maybe (isNothing)
import System.IO (IOMode(ReadWriteMode), hFileSize, hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Types
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import qualified Client.CLEnts as CLEnts
import qualified Client.CLFX as CLFX
import qualified Client.CLInv as CLInv
import qualified Client.CLTEnt as CLTEnt
import qualified Client.CLView as CLView
import {-# SOURCE #-} qualified Client.SCR as SCR
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import qualified Sound.S as S
import qualified Sys.Sys as Sys
import {-# SOURCE #-} qualified Util.Lib as Lib

svcStrings :: V.Vector B.ByteString
svcStrings =
    V.fromList [ "svc_bad", "svc_muzzleflash", "svc_muzzlflash2"
               , "svc_temp_entity", "svc_layout", "svc_inventory"
               , "svc_nop", "svc_disconnect", "svc_reconnect"
               , "svc_sound", "svc_print", "svc_stufftext"
               , "svc_serverdata", "svc_configstring", "svc_spawnbaseline"
               , "svc_centerprint", "svc_download", "svc_playerinfo"
               , "svc_packetentities", "svc_deltapacketentities", "svc_frame"
               ]

-- Request a download from the server
downloadF :: XCommandT
downloadF =
  XCommandT "CLParse.downloadF" (do
    c <- Com.argc

    if c /= 2
      then
        Com.printf "Usage: download <filename>\n"

      else do
        fileName <- Com.argv 1

        if ".." `BC.isInfixOf` fileName
          then
            Com.printf "Refusing to download a path with ..\n"

          else do
            loadedFile <- FS.loadFile fileName

            case loadedFile of
              Just _ -> -- it exists, no need to download
                Com.printf "File already exists.\n"

              Nothing -> do
                globals.cls.csDownloadName .= fileName
                Com.printf ("Downloading " `B.append` fileName `B.append` "\n")

                -- download to a temp name, and only rename
                -- to the real name when done, so if interrupted
                -- a runt file wont be left
                globals.cls.csDownloadTempName .= (Com.stripExtension fileName) `B.append` ".tmp"

                MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
                MSG.writeString (globals.cls.csNetChan.ncMessage) ("download " `B.append` fileName)

                globals.cls.csDownloadNumber += 1
  )

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

                     | cmd == Constants.svcReconnect -> do
                         Com.printf "Server disconnected, reconnecting\n"

                         download <- use $ globals.cls.csDownload

                         case download of
                           Nothing ->
                             return ()

                           Just downloadFile -> do
                             -- ZOID, close download
                             io $ handle (\(e :: IOException) -> return ()) (hClose downloadFile)
                             globals.cls.csDownload .= Nothing

                         globals.cls.csState .= Constants.caConnecting
                         globals.cls.csConnectTime .= (-99999)
                         -- fire immediately

                     | cmd == Constants.svcPrint -> do
                         i <- MSG.readByte (globals.netMessage)

                         when (i == Constants.printChat) $ do
                           S.startLocalSound "misc/talk.wav"
                           globals.con.cOrMask .= 128

                         str <- MSG.readString (globals.netMessage)
                         Com.printf str

                         globals.con.cOrMask .= 0

                     | cmd == Constants.svcCenterPrint -> do
                         str <- MSG.readString (globals.netMessage)
                         SCR.centerPrint str

                     | cmd == Constants.svcStuffText -> do
                         str <- MSG.readString (globals.netMessage)
                         Com.dprintf $ "stufftext: " `B.append` str `B.append` "\n"
                         CBuf.addText str

                     | cmd == Constants.svcServerData -> do
                         CBuf.execute -- make sure any stuffed commands are done
                         parseServerData

                     | cmd == Constants.svcConfigString -> parseConfigString

                     | cmd == Constants.svcSound -> parseStartSoundPacket

                     | cmd == Constants.svcSpawnBaseline -> parseBaseline

                     | cmd == Constants.svcTempEntity -> CLTEnt.parseTEnt

                     | cmd == Constants.svcMuzzleFlash -> CLFX.parseMuzzleFlash

                     | cmd == Constants.svcMuzzleFlash2 -> CLFX.parseMuzzleFlash2

                     | cmd == Constants.svcDownload -> parseDownload

                     | cmd == Constants.svcFrame -> CLEnts.parseFrame

                     | cmd == Constants.svcInventory -> CLInv.parseInventory

                     | cmd == Constants.svcLayout -> do
                         layout <- MSG.readString (globals.netMessage)
                         globals.cl.csLayout .= layout

                     | cmd == Constants.svcPlayerInfo || cmd == Constants.svcPacketEntities || cmd == Constants.svcDeltaPacketEntities ->
                         Com.comError Constants.errDrop "Out of place frame data"

                     | otherwise -> io (print cmd) >> Com.comError Constants.errDrop "CL_ParseServerMessage: Illegible server message\n"

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
    loadClientInfo (globals.cl.csClientInfo.ix player) str

loadClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> Quake ()
loadClientInfo clientInfoLens str = do
    let t = '\\' `BC.elemIndex` str
        (name, str') = case t of
                         Nothing -> (str, str)
                         Just idx -> (B.take idx str, B.drop (idx + 1) str)

    Just renderer <- use $ globals.re
    noSkinsValue <- liftM (^.cvValue) clNoSkinsCVar

    (modelRef, weaponModels, skinRef, iconName, iconRef) <-
      if noSkinsValue /= 0 || B.length str' == 0
        then do
          modelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
          weaponModelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/weapon.md2"
          skinRef <- (renderer^.rRefExport.reRegisterSkin) "players/male/grunt.pcx"
          let iconName = "/players/male/grunt_i.pcx"
              weaponModels = V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef else Nothing)
          iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
          return (modelRef, weaponModels, skinRef, iconName, iconRef)
        else do
          -- isolate the model name
          pos <- case '/' `BC.elemIndex` str' of
                   Nothing -> case '\\' `BC.elemIndex` str' of
                                Nothing -> do
                                  Com.comError Constants.errFatal ("Invalid model name:" `B.append` str')
                                  return 0
                                Just idx ->
                                  return idx
                   Just idx -> return idx

          let modelName = B.take pos str'
              -- isolate the skin name
              skinName = B.drop (pos + 1) str'
              -- model file
              modelFileName = "players/" `B.append` modelName `B.append` "/tris.md2"

          modelRef <- (renderer^.rRefExport.reRegisterModel) modelFileName
          (modelName', modelRef') <- case modelRef of
                                       Just _ -> return (modelName, modelRef)
                                       Nothing -> do
                                         ref <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
                                         return ("male", ref)

          -- skin file
          let skinFileName = "players/" `B.append` modelName' `B.append` "/" `B.append` skinName `B.append` ".pcx"
          skinRef <- (renderer^.rRefExport.reRegisterSkin) skinFileName

          -- if we don't have the skin and the model wasn't male,
          -- see if the male has it (this is for CTF's skins)
          (modelName'', modelRef'', skinRef') <-
            if isNothing skinRef && (BC.map toLower modelName') /= "male"
              then do
                -- change model to male
                ref <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
                -- see if the skin exists for the male model
                let skinFileName' = "players/male/" `B.append` skinName `B.append` ".pcx"
                sr <- (renderer^.rRefExport.reRegisterSkin) skinFileName'
                return ("male", ref, sr)
              else
                return (modelName', modelRef', skinRef)
          
          -- if we still don't have a skin, it means that the male model
          -- didn't have it, so default to grunt
          skinRef'' <- if isNothing skinRef'
                         then do
                           -- see if the skin exists for the male model
                           let skinFileName' = "players/" `B.append` modelName'' `B.append` "/grunt.pcx"
                           (renderer^.rRefExport.reRegisterSkin) skinFileName'
                         else
                           return skinRef'

          -- weapon file
          vwepValue <- liftM (^.cvValue) clVwepCVar
          clWeaponModels <- use $ clientGlobals.cgWeaponModels
          weaponModels <- if vwepValue == 0
                            then do
                              let weaponFileName = "players/" `B.append` modelName'' `B.append` "/" `B.append` (clWeaponModels V.! 0)
                              weaponModelRef <- (renderer^.rRefExport.reRegisterModel) weaponFileName
                              if isNothing weaponModelRef && (BC.map toLower modelName'') == "cyborg"
                                then do
                                  -- try male
                                  let weaponFileName' = "players/male/" `B.append` (clWeaponModels V.! 0)
                                  weaponModelRef' <- (renderer^.rRefExport.reRegisterModel) weaponFileName'
                                  return $ V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef' else Nothing)
                                else
                                  return $ V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef else Nothing)
                            else do
                              numWeaponModels <- use $ clientGlobals.cgNumCLWeaponModels
                              let emptyWeapons = V.replicate Constants.maxClientWeaponModels Nothing
                              updates <- loadWeapons clWeaponModels modelName'' 0 numWeaponModels []
                              return $ emptyWeapons V.// updates

          -- icon file
          let iconName = "/players/" `B.append` modelName'' `B.append` "/" `B.append` skinName `B.append` "_i.pcx"
          iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
          return (modelRef'', weaponModels, skinRef'', iconName, iconRef)

    -- must have all loaded data types to be valid
    if isNothing skinRef || isNothing iconRef || isNothing modelRef || isNothing (weaponModels V.! 0)
      then
        clientInfoLens .= newClientInfoT { _ciName        = name
                                         , _ciCInfo       = str
                                         , _ciSkin        = Nothing
                                         , _ciIcon        = Nothing
                                         , _ciIconName    = iconName
                                         , _ciModel       = Nothing
                                         , _ciWeaponModel = V.replicate Constants.maxClientWeaponModels Nothing
                                         }
      else
        clientInfoLens .= newClientInfoT { _ciName        = name
                                         , _ciCInfo       = str
                                         , _ciSkin        = skinRef
                                         , _ciIcon        = iconRef
                                         , _ciIconName    = iconName
                                         , _ciModel       = modelRef
                                         , _ciWeaponModel = weaponModels
                                         }

  where loadWeapons :: V.Vector B.ByteString -> B.ByteString -> Int -> Int -> [(Int, Maybe (IORef ModelT))] -> Quake [(Int, Maybe (IORef ModelT))]
        loadWeapons clWeaponModels modelName idx maxIdx acc
          | idx >= maxIdx = return acc
          | otherwise = do
              Just renderer <- use $ globals.re
              let weaponFileName = "players/" `B.append` modelName `B.append` "/" `B.append` (clWeaponModels V.! idx)
              weaponModelRef <- (renderer^.rRefExport.reRegisterModel) weaponFileName
              if isNothing weaponModelRef && (BC.map toLower modelName) == "cyborg"
                then do
                  -- try male
                  let weaponFileName' = "players/male/" `B.append` (clWeaponModels V.! idx)
                  weaponModelRef' <- (renderer^.rRefExport.reRegisterModel) weaponFileName'
                  loadWeapons clWeaponModels modelName (idx + 1) maxIdx ((idx, weaponModelRef') : acc)
                else
                  loadWeapons clWeaponModels modelName (idx + 1) maxIdx ((idx, weaponModelRef) : acc)

{-
- CL_CheckOrDownloadFile returns true if the file exists, 
- otherwise it attempts to start a
- download from the server.
-}
checkOrDownloadFile :: B.ByteString -> Quake Bool
checkOrDownloadFile fileName = do
    done <- checkIfShouldProceed

    if done
      then
        return True
      else do
        let tmpFileName = (Com.stripExtension fileName) `B.append` ".tmp"

        zoom (globals.cls) $ do
          csDownloadName .= fileName
          -- download to a temp name, and only rename
          -- to the real name when done, so if interrupted
          -- a runt file wont be left
          csDownloadTempName .= tmpFileName

        -- check to see if we already have a tmp for this file, if so, try
        -- to resume; open the file if not opened yet
        name <- downloadFileName tmpFileName
        fp <- Lib.fOpenBinary name ReadWriteMode

        case fp of
          Just h -> do
            -- it exists
            len <- io (handle (\(_ :: IOException) -> return 0) $ hFileSize h)

            globals.cls.csDownload .= Just h

            -- give the server an offset to start the download
            Com.printf $ "Resuming " `B.append` fileName `B.append` "\n"
            MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
            MSG.writeString (globals.cls.csNetChan.ncMessage) ("download " `B.append` fileName `B.append` " " `B.append` BC.pack (show len)) -- IMPROVE?

          Nothing -> do
            Com.printf $ "Downloading " `B.append` fileName `B.append` "\n"
            MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
            MSG.writeString (globals.cls.csNetChan.ncMessage) ("download " `B.append` fileName)

        globals.cls.csDownloadNumber += 1

        return False

  where checkIfShouldProceed :: Quake Bool
        checkIfShouldProceed = do
          if ".." `BC.isInfixOf` fileName
            then do
              Com.printf "Refusing to download a path with ..\n"
              return True
            else do
              len <- FS.fileLength fileName
              -- if file exists then there is no need to download
              return $ if len > 0 then True else False

downloadFileName :: B.ByteString -> Quake B.ByteString
downloadFileName fileName = do
    gameDir <- FS.gameDir
    return $ gameDir `B.append` "/" `B.append` fileName

registerSounds :: Quake ()
registerSounds = do
    S.beginRegistration
    CLTEnt.registerTEntSounds

    configStrings <- use $ globals.cl.csConfigStrings
    precacheSounds configStrings 1 Constants.maxSounds

    S.endRegistration

  where precacheSounds :: V.Vector B.ByteString -> Int -> Int -> Quake ()
        precacheSounds configSrings idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let cs = configSrings V.! (Constants.csSounds + idx)

              unless (cs == "" || cs == "\0") $ do
                  sfxRef <- S.registerSound cs
                  globals.cl.csSoundPrecache.ix idx .= sfxRef
                  Sys.sendKeyEvents -- pump message loop
                  precacheSounds configSrings (idx + 1) maxIdx

parseBaseline :: Quake ()
parseBaseline = do
    let nullState = newEntityStateT Nothing
    (newNum, bits) <- CLEnts.parseEntityBits [0]
    CLEnts.parseDelta nullState (globals.clEntities.ix newNum.ceBaseline) newNum (head bits)

parseStartSoundPacket :: Quake ()
parseStartSoundPacket = do
    flags <- MSG.readByte (globals.netMessage)
    soundNum <- MSG.readByte (globals.netMessage)

    volume <- if flags .&. Constants.sndVolume /= 0
                then liftM ((/ 255) . fromIntegral) (MSG.readByte (globals.netMessage))
                else return Constants.defaultSoundPacketVolume

    attenuation <- if flags .&. Constants.sndAttenuation /= 0
                     then liftM ((/ 64) . fromIntegral) (MSG.readByte (globals.netMessage))
                     else return Constants.defaultSoundPacketAttenuation

    ofs <- if flags .&. Constants.sndOffset /= 0
             then liftM ((/ 1000) . fromIntegral) (MSG.readByte (globals.netMessage))
             else return 0

    (channel, ent) <- if flags .&. Constants.sndEnt /= 0 -- entity reletive
                        then do
                          channel <- MSG.readShort (globals.netMessage)
                          let ent = channel `shiftR` 3

                          when (ent > Constants.maxEdicts) $
                            Com.comError Constants.errDrop ("CL_ParseStartSoundPacket: ent = " `B.append` BC.pack (show ent)) -- IMPROVE?

                          return (channel .&. 7, ent)
                        else
                          return (0, 0)

    pos <- if flags .&. Constants.sndPos /= 0 -- positioned in space
             then liftM Just (MSG.readPos (globals.netMessage))
             else return Nothing

    Just sound <- preuse $ globals.cl.csSoundPrecache.ix soundNum

    -- TODO: research if we need this:
    -- if (null == Globals.cl.sound_precache[sound_num])
    --      return;
    S.startSound pos (newEdictReference ent) channel sound volume attenuation ofs

parseDownload :: Quake ()
parseDownload = do
    io (putStrLn "CLParse.parseDownload") >> undefined -- TODO
