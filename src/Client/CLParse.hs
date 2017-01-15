module Client.CLParse
    ( downloadF
    , loadClientInfo
    , parseServerMessage
    , registerSounds
    ) where

import           Control.Applicative   (liftA2)
import           Control.Lens          (use, preuse, ix, (.=), (^.))
import           Control.Monad         (join, when, void, unless)
import           Data.Bits             (shiftR, (.&.))
import qualified Data.ByteString       as B
import qualified Data.Vector           as V

import           Client.CEntityT
import {-# SOURCE #-} qualified Client.CL as CL
import qualified Client.CLEnts         as CLEnts
import qualified Client.CLFX           as CLFX
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInv          as CLInv
import           Client.CLParseShared
import qualified Client.CLTEnt         as CLTEnt
import qualified Client.CLView         as CLView
import           Client.ConsoleT
import           Client.RefExportT
import qualified Client.SCR            as SCR
import qualified Constants
import           Game.CVarT
import           Game.EntityStateT
import qualified QCommon.CBuf          as CBuf
import qualified QCommon.CM            as CM
import qualified QCommon.Com           as Com
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import qualified QCommon.MSG           as MSG
import           QCommon.SizeBufT
import           QuakeState
import           Render.Renderer
import qualified Sound.S               as S
import qualified Sys.Sys               as Sys
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib

downloadF :: XCommandT
downloadF = error "CLParse.downloadF" -- TODO

parseServerMessage :: Quake ()
parseServerMessage = do
    parseMessage =<< use (globals.gNetMessage)
    CLView.addNetGraph
    join (liftA2 checkSaveDemo demoRecording demoWaiting)
  where
    demoRecording = use (globals.gCls.csDemoRecording)
    demoWaiting = use (globals.gCls.csDemoWaiting)

parseMessage :: SizeBufT -> Quake ()
parseMessage netMessage
    | (netMessage^.sbReadCount) > (netMessage^.sbCurSize) =
        Com.fatalError "CL_ParseServerMessage: Bad server message:"
    | otherwise = proceedParseMessage =<< MSG.readByte (globals.gNetMessage)

proceedParseMessage :: Int -> Quake ()
proceedParseMessage cmd
    | cmd == -1 = showNet "END OF MESSAGE" =<< clShowNetCVar
    | otherwise = do
        checkShowNet =<< clShowNetCVar
        doParseMessage cmd
        parseMessage =<< use (globals.gNetMessage)
  where
    checkShowNet showNetVar
        | (showNetVar^.cvValue) >= 2 = error "CLParse.proceedParseMessage#checkShowNet" -- TODO
        | otherwise = return ()

doParseMessage :: Int -> Quake ()
doParseMessage cmd
    | cmd == Constants.svcNop = return ()
    | cmd == Constants.svcDisconnect = Com.comError Constants.errDisconnect "Server disconnected\n"
    | cmd == Constants.svcReconnect = reconnect
    | cmd == Constants.svcPrint = parsePrintPacket
    | cmd == Constants.svcCenterPrint = SCR.centerPrint =<< MSG.readString (globals.gNetMessage)
    | cmd == Constants.svcStuffText = parseStuffText
    | cmd == Constants.svcServerData = CBuf.execute >> parseServerData
    | cmd == Constants.svcConfigString = parseConfigString
    | cmd == Constants.svcSound = parseStartSoundPacket
    | cmd == Constants.svcSpawnBaseline = parseBaseline
    | cmd == Constants.svcTempEntity = CLTEnt.parseTEnt
    | cmd == Constants.svcMuzzleFlash = CLFX.parseMuzzleFlash
    | cmd == Constants.svcMuzzleFlash2 = CLFX.parseMuzzleFlash2
    | cmd == Constants.svcDownload = parseDownload
    | cmd == Constants.svcFrame = CLEnts.parseFrame
    | cmd == Constants.svcInventory = CLInv.parseInventory
    | cmd == Constants.svcLayout = parseLayout
    | cmd `elem` [Constants.svcPlayerInfo, Constants.svcPacketEntities, Constants.svcDeltaPacketEntities] =
        Com.comError Constants.errDrop "Out of place frame data"
    | otherwise = Com.comError Constants.errDrop "CL_ParseServerMessage: Illegible server message\n"

checkSaveDemo :: Bool -> Bool -> Quake ()
checkSaveDemo demoRecording demoWaiting
    | demoRecording && not demoWaiting = CL.writeDemoMessage
    | otherwise = return ()

registerSounds :: Quake ()
registerSounds = do
    S.beginRegistration
    CLTEnt.registerTEntSounds
    configStrings <- use (globals.gCl.csConfigStrings)
    precacheSounds configStrings 1 Constants.maxSounds
    S.endRegistration

precacheSounds :: V.Vector B.ByteString -> Int -> Int -> Quake ()
precacheSounds configSrings idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        let cs = configSrings V.! (Constants.csSounds + idx)
        unless (cs == "" || cs == "\0") $ do
            sfxRef <- S.registerSound cs
            globals.gCl.csSoundPrecache.ix idx .= sfxRef
            Sys.sendKeyEvents -- pump message loop
            precacheSounds configSrings (idx + 1) maxIdx

reconnect :: Quake ()
reconnect = do
    Com.printf "Server disconnected, reconnecting\n"
    checkDownload =<< use (globals.gCls.csDownload)
    globals.gCls.csState .= Constants.caConnecting
    globals.gCls.csConnectTime .= (-99999) -- fire immediately
  where
    checkDownload Nothing = return ()
    checkDownload (Just downloadFile) = do
        Lib.fClose downloadFile
        globals.gCls.csDownload .= Nothing

parsePrintPacket :: Quake ()
parsePrintPacket = do
    i <- MSG.readByte (globals.gNetMessage)
    when (i == Constants.printChat) $ do
        S.startLocalSound "misc/talk.wav"
        globals.gCon.cOrMask .= 128
    str <- MSG.readString (globals.gNetMessage)
    Com.printf str
    globals.gCon.cOrMask .= 0

parseStuffText :: Quake ()
parseStuffText = do
    str <- MSG.readString (globals.gNetMessage)
    Com.dprintf (B.concat ["stufftext: ", str, "\n"])
    CBuf.addText str

parseServerData :: Quake ()
parseServerData = do
    CL.clearState
    globals.gCls.csState .= Constants.caConnected
    version <- MSG.readLong (globals.gNetMessage)
    demoHack version =<< use (globals.gServerState)
    serverCount <- MSG.readLong (globals.gNetMessage)
    attractLoop <- MSG.readByte (globals.gNetMessage)
    gameDir <- MSG.readString (globals.gNetMessage)
    playerNum <- MSG.readShort (globals.gNetMessage)
    levelName <- MSG.readString (globals.gNetMessage)
    globals.gCls.csServerProtocol .= version
    globals.gCl.csServerCount .= serverCount
    globals.gCl.csAttractLoop .= (attractLoop /= 0)
    globals.gCl.csGameDir .= gameDir
    globals.gCl.csPlayerNum .= playerNum
    Com.dprintf (B.concat ["gamedir=", gameDir, "\n"])
    Com.dprintf (B.concat ["numplayers=", encode playerNum, "\n"])
    Com.dprintf (B.concat ["levelname=", levelName, "\n"])
    saveGameDir gameDir =<< fmap (^.cvString) fsGameDirVarCVar
    playCinematicOrLoadLevel playerNum levelName
  where
    saveGameDir gameDir gameDirVar
        | B.length gameDir > 0 && (B.null gameDirVar || gameDir == gameDirVar) || (B.null gameDir && B.null gameDirVar) =
            void (CVar.set "game" gameDir)
        | otherwise = return ()
    playCinematicOrLoadLevel playerNum levelName
        | playerNum == -1 = SCR.playCinematic levelName
        | otherwise = do
            Com.printf (B.concat ["Levelname: ", levelName, "\n"])
            globals.gCl.csRefreshPrepped .= False

demoHack :: Int -> Int -> Quake ()
demoHack version state
    | state /= 0 && Constants.protocolVersion == 34 = return ()
    | version /= Constants.protocolVersion =
        Com.comError Constants.errDrop (B.concat ["Server returned version ", encode version, ", not ", encode Constants.protocolVersion])
    | otherwise = return ()

parseConfigString :: Quake ()
parseConfigString = do
    i <- MSG.readShort (globals.gNetMessage)
    when (i < 0 || i >= Constants.maxConfigStrings) $
        Com.comError Constants.errDrop "configstring > MAX_CONFIGSTRINGS"
    str <- MSG.readString (globals.gNetMessage)
    globals.gCl.csConfigStrings.ix i .= str
    refreshPrepped <- use (globals.gCl.csRefreshPrepped)
    oldStr <- preuse (globals.gCl.csConfigStrings.ix i)
    doParseConfigString i str refreshPrepped oldStr

doParseConfigString :: Int -> B.ByteString -> Bool -> Maybe B.ByteString -> Quake ()
doParseConfigString _ _ _ Nothing = Com.fatalError "CLParse.parseConfigString oldStr is Nothing"
doParseConfigString i str refreshPrepped (Just oldStr)
    | i >= Constants.csLights && i < Constants.csLights + Constants.maxLightStyles =
        CLFX.setLightStyle (i - Constants.csLights)
    | i >= Constants.csModels && i < Constants.csModels + Constants.maxModels =
        when refreshPrepped $ do
            renderer <- use (globals.gRenderer)
            maybe rendererError parseModelConfigString renderer
    | i >= Constants.csSounds && i < Constants.csSounds + Constants.maxSounds =
        when refreshPrepped $ do
            soundRef <- S.registerSound str
            globals.gCl.csSoundPrecache.ix (i - Constants.csSounds) .= soundRef
    | i >= Constants.csImages && i < Constants.csImages + Constants.maxImages =
        when refreshPrepped $ do
            renderer <- use (globals.gRenderer)
            maybe rendererError parseImageConfigString renderer
    | i >= Constants.csPlayerSkins && i < Constants.csPlayerSkins + Constants.maxClients =
        when (refreshPrepped && not (oldStr == str)) $
            parseClientInfo (i - Constants.csPlayerSkins)
    | otherwise = return ()
  where
    rendererError = Com.fatalError "CLParse.doParseConfigString renderer is Nothing"
    parseModelConfigString renderer = do
        modelRef <- (renderer^.rRefExport.reRegisterModel) str
        globals.gCl.csModelDraw.ix (i - Constants.csModels) .= modelRef
        setModelClip
    setModelClip
        | B.take 1 str == "*" = do
            cModelRef <- CM.inlineModel str
            globals.gCl.csModelClip.ix (i - Constants.csModels) .= Just cModelRef
        | otherwise =
            globals.gCl.csModelClip.ix (i - Constants.csModels) .= Nothing
    parseImageConfigString renderer = do
        imageRef <- (renderer^.rRefExport.reRegisterPic) str
        globals.gCl.csImagePrecache.ix (i - Constants.csImages) .= imageRef

parseStartSoundPacket :: Quake ()
parseStartSoundPacket = do
    flags <- MSG.readByte (globals.gNetMessage)
    soundNum <- MSG.readByte (globals.gNetMessage)
    volume <- getVolume flags
    attenuation <- getAttenuation flags
    ofs <- getOffset flags
    (channel, ent) <- getChannelAndEntity flags
    pos <- getPosition flags
    sound <- getSound soundNum
    -- TODO: research if we need this:
    -- if (null == Globals.cl.sound_precache[sound_num])
    --      return;
    S.startSound pos (Ref ent) channel sound volume attenuation ofs
  where
    getVolume flags
        | flags .&. Constants.sndVolume /= 0 =
            fmap ((/ 255) . fromIntegral) (MSG.readByte (globals.gNetMessage))
        | otherwise =
            return Constants.defaultSoundPacketVolume
    getAttenuation flags
        | flags .&. Constants.sndAttenuation /= 0 =
            fmap ((/ 64) . fromIntegral) (MSG.readByte (globals.gNetMessage))
        | otherwise =
            return Constants.defaultSoundPacketAttenuation
    getOffset flags
        | flags .&. Constants.sndOffset /= 0 =
            fmap ((/ 1000) . fromIntegral) (MSG.readByte (globals.gNetMessage))
        | otherwise =
            return 0
    getChannelAndEntity flags
        | flags .&. Constants.sndEnt /= 0 = do
            channel <- MSG.readShort (globals.gNetMessage)
            let ent = channel `shiftR` 3
            when (ent > Constants.maxEdicts) $
                Com.comError Constants.errDrop ("CL_ParseStartSoundPacket: ent = " `B.append` encode ent)
            return (channel .&. 7, ent)
        | otherwise =
            return (0, 0)
    getPosition flags
        | flags .&. Constants.sndPos /= 0 = fmap Just (MSG.readPos (globals.gNetMessage))
        | otherwise = return Nothing
    getSound :: Int -> Quake (Maybe (Ref' SfxT))
    getSound soundNum = do
        precache <- use (globals.gCl.csSoundPrecache)
        return (precache V.! soundNum)

parseBaseline :: Quake ()
parseBaseline = do
    (newNum, bits) <- CLEnts.parseEntityBits [0]
    CLEnts.parseDelta (newEntityStateT Nothing) (globals.gClEntities.ix newNum.ceBaseline) newNum (head bits)

parseDownload :: Quake ()
parseDownload = error "CLParse.parseDownload" -- TODO

parseLayout :: Quake ()
parseLayout = do
    layout <- MSG.readString (globals.gNetMessage)
    globals.gCl.csLayout .= layout