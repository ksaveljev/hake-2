{-# LANGUAGE Rank2Types #-}
module Client.CLEnts
    ( addEntities
    , parseDelta
    , parseEntityBits
    , parseFrame
    ) where

import           Control.Lens          (Lens', Traversal', use, ix, (.=), (+=), (-=), (%=), (^.), (&), (.~), (%~))
import           Control.Monad         (when, unless)
import           Data.Bits             (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as UV
import           Linear                (V3(..), V4(..), _x, _y, _z)

import           Client.CEntityT
import           Client.ClientInfoT
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLFX           as CLFX
import qualified Client.CLNewFX        as CLNewFX
import qualified Client.CLParseShared  as CLParse
import qualified Client.CLPred         as CLPred
import qualified Client.CLTEnt         as CLTEnt
import qualified Client.Console        as Console
import           Client.EntityT
import           Client.FrameT
import           Client.RefDefT
import           Client.RefExportT
import qualified Client.VShared        as ClientV
import qualified Constants
import           Game.CVarT
import           Game.EntityStateT
import           Game.PlayerStateT
import           Game.PMoveStateT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import qualified QCommon.FS            as FS
import qualified QCommon.MSG           as MSG
import           QCommon.SizeBufT
import           QuakeRef
import           QuakeState
import           Render.ImageT
import           Render.Renderer
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

bfgLightRamp :: UV.Vector Int
bfgLightRamp = UV.fromList [ 300, 400, 600, 300, 150, 75 ]

addEntities :: Quake ()
addEntities = do
    cl <- use (globals.gCl)
    cls <- use (globals.gCls)
    showClamp <- fmap (^.cvValue) showClampCVar
    when ((cls^.csState) == Constants.caActive) $ do
        setTimeAndLerpFrac cl showClamp
    checkTimeDemo
    calcViewValues =<< use (globals.gCl)
    addPacketEntities =<< use (globals.gCl.csFrame)
    CLTEnt.addTEnts
    CLFX.addParticles
    CLFX.addDLights
    CLFX.addLightStyles
  where
    setTimeAndLerpFrac cl showClamp
        | (cl^.csTime) > (cl^.csFrame.fServerTime) = do
            when (showClamp /= 0) $
                Com.printf (B.concat ["high clamp ", encode ((cl^.csTime) - (cl^.csFrame.fServerTime)), "\n"])
            globals.gCl.csTime .= (cl^.csFrame.fServerTime)
            globals.gCl.csLerpFrac .= 1
        | (cl^.csTime) < (cl^.csFrame.fServerTime) - 100 = do
            when (showClamp /= 0) $
                Com.printf (B.concat ["low clamp ", encode ((cl^.csFrame.fServerTime) - 100 - (cl^.csTime)), "\n"])
            globals.gCl.csTime .= (cl^.csFrame.fServerTime) - 100
            globals.gCl.csLerpFrac .= 0
        | otherwise =
            globals.gCl.csLerpFrac .= 1 - fromIntegral ((cl^.csFrame.fServerTime) - (cl^.csTime)) * 0.01
    checkTimeDemo = do
        timeDemo <- fmap (^.cvValue) timeDemoCVar
        when (timeDemo /= 0) $ do
            globals.gCl.csLerpFrac .= 1

parseFrame :: Quake ()
parseFrame = do
    serverFrame <- MSG.readLong (globals.gNetMessage)
    deltaFrame <- MSG.readLong (globals.gNetMessage)
    globals.gCl.csFrame .= (newFrameT & fServerFrame .~ serverFrame
                                      & fDeltaFrame .~ deltaFrame
                                      & fServerTime .~ serverFrame * 100)
    makeOldDemosWork
    showNetInfo serverFrame deltaFrame
    old <- getOldFrame deltaFrame
    clampTime (serverFrame * 100) =<< use (globals.gCl.csTime)
    readAreaBits
    readPlayerInfo
    parsePlayerState old (globals.gCl.csFrame)
    readPacketEntities
    parsePacketEntities old (globals.gCl.csFrame)
    frame <- saveFrame serverFrame
    when (frame^.fValid) (endConnectionProcess frame)

makeOldDemosWork :: Quake ()
makeOldDemosWork = do
    -- BIG HACK to let old demos continue to work
    serverProtocol <- use (globals.gCls.csServerProtocol)
    when (serverProtocol /= 26) $ do
        surpressCount <- MSG.readByte (globals.gNetMessage)
        globals.gCl.csSurpressCount .= surpressCount

showNetInfo :: Int -> Int -> Quake ()
showNetInfo serverFrame deltaFrame = do
    showNet <- fmap (^.cvValue) clShowNetCVar
    when (showNet == 3) $
        Com.printf (B.concat ["   frame:", encode serverFrame, "  delta:", encode deltaFrame, "\n"])

getOldFrame :: Int -> Quake (Maybe FrameT)
getOldFrame deltaFrame
    | deltaFrame <= 0 = do
        globals.gCl.csFrame.fValid .= True
        globals.gCls.csDemoWaiting .= False
        return Nothing
    | otherwise = do
        old <- readRef (Ref (deltaFrame .&. Constants.updateMask))
        unless (old^.fValid) $ -- should never happen
            Com.printf "Delta from invalid frame (not supposed to happen!).\n"
        parseEntities <- use (globals.gCl.csParseEntities)
        verifyOldFrame old deltaFrame parseEntities
        return (Just old)

verifyOldFrame :: FrameT -> Int -> Int -> Quake ()
verifyOldFrame old deltaFrame parseEntities
    | (old^.fServerFrame) /= deltaFrame = -- The frame is too old, so we can't reconstruct it properly.
        Com.printf "Delta frame too old.\n"
    | parseEntities - (old^.fParseEntities) > Constants.maxParseEntities - 128 =
        Com.printf "Delta parse_entities too old.\n"
    | otherwise =
        globals.gCl.csFrame.fValid .= True -- valid delta parse

clampTime :: Int -> Int -> Quake ()
clampTime serverTime time
    | time > serverTime       = globals.gCl.csTime .= serverTime
    | time < serverTime - 100 = globals.gCl.csTime .= serverTime - 100
    | otherwise               = return ()

readAreaBits :: Quake ()
readAreaBits = do
    len <- MSG.readByte (globals.gNetMessage)
    MSG.readData (globals.gNetMessage) (globals.gCl.csFrame.fAreaBits) len

readPlayerInfo :: Quake ()
readPlayerInfo = do
    cmd <- MSG.readByte (globals.gNetMessage)
    CLParse.showNet (CLParse.svcStrings V.! cmd) =<< clShowNetCVar
    when (cmd /= Constants.svcPlayerInfo) $
        Com.comError Constants.errDrop "CL_ParseFrame: not playerinfo"

readPacketEntities :: Quake ()
readPacketEntities = do
    cmd <- MSG.readByte (globals.gNetMessage)
    CLParse.showNet (CLParse.svcStrings V.! cmd) =<< clShowNetCVar
    when (cmd /= Constants.svcPacketEntities) $
        Com.comError Constants.errDrop "CL_ParseFrame: not packetentities"

saveFrame :: Int -> Quake FrameT
saveFrame serverFrame = do
    -- save the frame off in the backup array for later delta comparisons
    frame <- use (globals.gCl.csFrame)
    writeRef (Ref (serverFrame .&. Constants.updateMask)) frame
    return frame

endConnectionProcess :: FrameT -> Quake ()
endConnectionProcess frame = do
    clientStatic <- use (globals.gCls)
    clientState <- use (globals.gCl)
    when ((clientStatic^.csState) /= Constants.caActive) $ do
        globals.gCls.csState .= Constants.caActive
        globals.gCl.csForceRefDef .= True
        globals.gCl.csPredictedOrigin .= fmap ((* 0.125) . fromIntegral) (frame^.fPlayerState.psPMoveState.pmsOrigin)
        globals.gCl.csPredictedAngles .= (frame^.fPlayerState.psViewAngles)
        when ((clientStatic^.csDisableServerCount) /= (clientState^.csServerCount) && (clientState^.csRefreshPrepped)) $ do
            -- instead of using SCR we write it out directly to avoid recursive module dependency
            -- SCR.endLoadingPlaque -- get rid of loading plaque
            globals.gCls.csDisableScreen .= 0
            Console.clearNotify
    globals.gCl.csSoundPrepped .= True -- can start mixing ambient sounds
    fireEntityEvents frame
    CLPred.checkPredictionError

parseEntityBits :: [Int] -> Quake (Int, [Int])
parseEntityBits bits =
    readA >>= readB >>= readC >>= readD >>= readNumber
  where
    readA = MSG.readByte (globals.gNetMessage)
    readB a | a .&. Constants.uMoreBits1 /= 0 = do
                b <- MSG.readByte (globals.gNetMessage)
                return (a .|. (b `shiftL` 8))
            | otherwise = return a
    readC b | b .&. Constants.uMoreBits2 /= 0 = do
                c <- MSG.readByte (globals.gNetMessage)
                return (b .|. (c `shiftL` 16))
            | otherwise = return b
    readD c | c .&. Constants.uMoreBits3 /= 0 = do
                d <- MSG.readByte (globals.gNetMessage)
                return (c .|. (d `shiftL` 24))
            | otherwise = return c
    readNumber d | d .&. Constants.uNumber16 /= 0 = do
                     num <- MSG.readShort (globals.gNetMessage)
                     return (num, d : tail bits)
                 | otherwise = do
                     num <- MSG.readByte (globals.gNetMessage)
                     return (num, d : tail bits)

parseDelta :: EntityStateT -> Traversal' QuakeState EntityStateT -> Int -> Int -> Quake ()
parseDelta from to number bits = do
    modelIndex <- getModelIndex
    modelIndex2 <- getModelIndex2
    modelIndex3 <- getModelIndex3
    modelIndex4 <- getModelIndex4
    frame <- getFrame
    skinNum <- getSkinNum
    effects <- getEffects
    renderFx <- getRenderFx
    originX <- getOriginX
    originY <- getOriginY
    originZ <- getOriginZ
    anglesX <- getAnglesX
    anglesY <- getAnglesY
    anglesZ <- getAnglesZ
    oldOrigin <- getOldOrigin
    sound <- getSound
    event <- getEvent
    solid <- getSolid
    to .= from { _esNumber      = number
               , _esModelIndex  = modelIndex
               , _esModelIndex2 = modelIndex2
               , _esModelIndex3 = modelIndex3
               , _esModelIndex4 = modelIndex4
               , _esFrame       = frame
               , _esSkinNum     = skinNum
               , _esEffects     = effects
               , _esRenderFx    = renderFx
               , _esOrigin      = V3 originX originY originZ
               , _esAngles      = V3 anglesX anglesY anglesZ
               , _esOldOrigin   = oldOrigin
               , _esSound       = sound
               , _esEvent       = event
               , _esSolid       = solid
               }
  where
    getModelIndex
        | bits .&. Constants.uModel /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex)
    getModelIndex2
        | bits .&. Constants.uModel2 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex2)
    getModelIndex3
        | bits .&. Constants.uModel3 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex3)
    getModelIndex4
        | bits .&. Constants.uModel4 /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esModelIndex4)
    getFrame
        | bits .&. Constants.uFrame8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uFrame16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esFrame)
    getSkinNum
        | bits .&. Constants.uSkin8 /= 0 && bits .&. Constants.uSkin16 /= 0 = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uSkin8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uSkin16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esSkinNum)
    getEffects
        | bits .&. (Constants.uEffects8 .|. Constants.uEffects16) == (Constants.uEffects8 .|. Constants.uEffects16) = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uEffects8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uEffects16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esEffects)
    getRenderFx
        | bits .&. (Constants.uRenderFx8 .|. Constants.uRenderFx16) == (Constants.uRenderFx8 .|. Constants.uRenderFx16) = MSG.readLong (globals.gNetMessage)
        | bits .&. Constants.uRenderFx8 /= 0 = MSG.readByte (globals.gNetMessage)
        | bits .&. Constants.uRenderFx16 /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esRenderFx)
    getOriginX
        | bits .&. Constants.uOrigin1 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._x)
    getOriginY
        | bits .&. Constants.uOrigin2 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._y)
    getOriginZ
        | bits .&. Constants.uOrigin3 /= 0 = MSG.readCoord (globals.gNetMessage)
        | otherwise = return (from^.esOrigin._z)
    getAnglesX
        | bits .&. Constants.uAngle1 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._x)
    getAnglesY
        | bits .&. Constants.uAngle2 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._y)
    getAnglesZ
        | bits .&. Constants.uAngle3 /= 0 = MSG.readAngle (globals.gNetMessage)
        | otherwise = return (from^.esAngles._z)
    getOldOrigin
        | bits .&. Constants.uOldOrigin /= 0 = MSG.readPos (globals.gNetMessage)
        | otherwise = return (from^.esOrigin)
    getSound
        | bits .&. Constants.uSound /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (from^.esSound)
    getEvent
        | bits .&. Constants.uEvent /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return 0
    getSolid
        | bits .&. Constants.uSolid /= 0 = MSG.readShort (globals.gNetMessage)
        | otherwise = return (from^.esSolid)

parsePlayerState :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePlayerState oldFrame newFrameLens = do
    attractLoop <- use (globals.gCl.csAttractLoop)
    flags <- MSG.readShort (globals.gNetMessage)
    -- parse the pmove_state_t
    pmType <- parsePMType flags
    origin <- parseOrigin flags
    velocity <- parseVelocity flags
    pmTime <- parsePMTime flags
    pmFlags <- parsePMFlags flags
    gravity <- parseGravity flags
    deltaAngles <- parseDeltaAngles flags
    -- parse the rest of the player_state_t
    viewOffset <- parseViewOffset flags
    viewAngles <- parseViewAngles flags
    kickAngles <- parseKickAngles flags
    gunIndex <- parseGunIndex flags
    (gunFrame, gunOffset, gunAngles) <- parseGunInfo flags
    blend <- parseBlend flags
    fov <- parseFOV flags
    rdFlags <- parseRDFlags flags
    -- parse stats
    statbits <- MSG.readLong (globals.gNetMessage)
    updates <- readStats statbits 0 Constants.maxStats []
    newFrameLens.fPlayerState .= PlayerStateT
        { _psPMoveState = PMoveStateT { _pmsPMType      = if attractLoop then Constants.pmFreeze else pmType
                                      , _pmsOrigin      = origin
                                      , _pmsVelocity    = velocity
                                      , _pmsPMFlags     = pmFlags
                                      , _pmsPMTime      = pmTime
                                      , _pmsGravity     = gravity
                                      , _pmsDeltaAngles = deltaAngles
                                      }
        , _psViewAngles = viewAngles
        , _psViewOffset = viewOffset
        , _psKickAngles = kickAngles
        , _psGunAngles  = gunAngles
        , _psGunOffset  = gunOffset
        , _psGunIndex   = gunIndex
        , _psGunFrame   = gunFrame
        , _psBlend      = blend
        , _psFOV        = fov
        , _psRDFlags    = rdFlags
        , _psStats      = (state^.psStats) UV.// updates
        }
  where
    state = maybe newPlayerStateT (^.fPlayerState) oldFrame
    parsePMType flags
        | flags .&. Constants.psMType /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (state^.psPMoveState.pmsPMType)
    parseOrigin flags
        | flags .&. Constants.psMOrigin /= 0 = do
            x <- MSG.readShort (globals.gNetMessage)
            y <- MSG.readShort (globals.gNetMessage)
            z <- MSG.readShort (globals.gNetMessage)
            return (fmap fromIntegral (V3 x y z))
        | otherwise =
            return (state^.psPMoveState.pmsOrigin)
    parseVelocity flags
        | flags .&. Constants.psMVelocity /= 0 = do
            x <- MSG.readShort (globals.gNetMessage)
            y <- MSG.readShort (globals.gNetMessage)
            z <- MSG.readShort (globals.gNetMessage)
            return (fmap fromIntegral (V3 x y z))
        | otherwise =
            return (state^.psPMoveState.pmsVelocity)
    parsePMTime flags
        | flags .&. Constants.psMTime /= 0 = fmap fromIntegral (MSG.readByte (globals.gNetMessage))
        | otherwise = return (state^.psPMoveState.pmsPMTime)
    parsePMFlags flags
        | flags .&. Constants.psMFlags /= 0 = fmap fromIntegral (MSG.readByte (globals.gNetMessage))
        | otherwise = return (state^.psPMoveState.pmsPMFlags)
    parseGravity flags
        | flags .&. Constants.psMGravity /= 0 = fmap fromIntegral (MSG.readShort (globals.gNetMessage))
        | otherwise = return (state^.psPMoveState.pmsGravity)
    parseDeltaAngles flags
        | flags .&. Constants.psMDeltaAngles /= 0 = do
            x <- MSG.readShort (globals.gNetMessage)
            y <- MSG.readShort (globals.gNetMessage)
            z <- MSG.readShort (globals.gNetMessage)
            return (fmap fromIntegral (V3 x y z))
        | otherwise =
            return (state^.psPMoveState.pmsDeltaAngles)
    parseViewOffset flags
        | flags .&. Constants.psViewOffset /= 0 = do
            x <- MSG.readChar (globals.gNetMessage)
            y <- MSG.readChar (globals.gNetMessage)
            z <- MSG.readChar (globals.gNetMessage)
            return (fmap ((* 0.25) . fromIntegral) (V3 x y z))
        | otherwise =
            return (state^.psViewOffset)
    parseViewAngles flags
        | flags .&. Constants.psViewAngles /= 0 = do
            x <- MSG.readAngle16 (globals.gNetMessage)
            y <- MSG.readAngle16 (globals.gNetMessage)
            z <- MSG.readAngle16 (globals.gNetMessage)
            return (V3 x y z)
        | otherwise =
            return (state^.psViewAngles)
    parseKickAngles flags
        | flags .&. Constants.psKickAngles /= 0 = do
            x <- MSG.readChar (globals.gNetMessage)
            y <- MSG.readChar (globals.gNetMessage)
            z <- MSG.readChar (globals.gNetMessage)
            return (fmap ((* 0.25) . fromIntegral) (V3 x y z))
        | otherwise =
            return (state^.psKickAngles)
    parseGunIndex flags
        | flags .&. Constants.psWeaponIndex /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (state^.psGunIndex)
    parseGunInfo flags
        | flags .&. Constants.psWeaponFrame /= 0 = do
            gunFrame <- MSG.readByte (globals.gNetMessage)
            x <- MSG.readChar (globals.gNetMessage)
            y <- MSG.readChar (globals.gNetMessage)
            z <- MSG.readChar (globals.gNetMessage)
            x' <- MSG.readChar (globals.gNetMessage)
            y' <- MSG.readChar (globals.gNetMessage)
            z' <- MSG.readChar (globals.gNetMessage)
            return (gunFrame, fmap ((* 0.25) . fromIntegral) (V3 x y z), fmap ((* 0.25) . fromIntegral) (V3 x' y' z'))
        | otherwise =
            return (state^.psGunFrame, state^.psGunOffset, state^.psGunAngles)
    parseBlend flags
        | flags .&. Constants.psBlend /= 0 = do
            x <- MSG.readByte (globals.gNetMessage)
            y <- MSG.readByte (globals.gNetMessage)
            z <- MSG.readByte (globals.gNetMessage)
            w <- MSG.readByte (globals.gNetMessage)
            return (fmap ((/ 255) . fromIntegral) (V4 x y z w))
        | otherwise =
            return (state^.psBlend)
    parseFOV flags
        | flags .&. Constants.psFov /= 0 = fmap fromIntegral (MSG.readByte (globals.gNetMessage))
        | otherwise = return (state^.psFOV)
    parseRDFlags flags
        | flags .&. Constants.psRdFlags /= 0 = MSG.readByte (globals.gNetMessage)
        | otherwise = return (state^.psRDFlags)
    readStats statbits idx maxIdx acc
        | idx >= maxIdx = return acc
        | statbits .&. (1 `shiftL` idx) /= 0 = do
            v <- MSG.readShort (globals.gNetMessage)
            readStats statbits (idx + 1) maxIdx ((idx, fromIntegral v) : acc)
        | otherwise =
            readStats statbits (idx + 1) maxIdx acc

parsePacketEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Quake ()
parsePacketEntities oldFrame newFrameLens = do
    setParseEntities
    (oldNum, oldState) <- getOldState
    doParsePacketEntities oldFrame newFrameLens oldNum oldState 0 0
  where
    setParseEntities = do
        parseEntities <- use (globals.gCl.csParseEntities)
        newFrameLens.fParseEntities .= parseEntities
        newFrameLens.fNumEntities .= 0
    getOldState = do
        parseEntities <- use (globals.gClParseEntities)
        case oldFrame of
            Nothing -> return (99999, Nothing)
            Just frame -> let idx = (frame^.fParseEntities) .&. (Constants.maxParseEntities - 1)
                              oldState = parseEntities V.! idx
                          in return (oldState^.esNumber, Just oldState)

doParsePacketEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Int -> Maybe EntityStateT -> Int -> Int -> Quake ()
doParsePacketEntities oldFrame newFrameLens oldNum oldState oldIndex bits = do
    (newNum, iw) <- parseEntityBits [bits]
    when (newNum >= Constants.maxEdicts) $
        Com.comError Constants.errDrop (B.concat ["CL_ParsePacketEntities: bad number:", encode newNum])
    checkEndOfMessage
    showNet <- fmap (^.cvValue) clShowNetCVar
    parse showNet newNum (head iw)
  where
    checkEndOfMessage = do
        netMsg <- use (globals.gNetMessage)
        when ((netMsg^.sbReadCount) > (netMsg^.sbCurSize)) $
            Com.comError Constants.errDrop "CL_ParsePacketEntities: end of message"
    parse showNet newNum bits'
        | newNum == 0 =
            copyRemainingEntities oldFrame newFrameLens showNet oldNum oldState oldIndex
        | otherwise = do
            (oldIndex', oldNum', oldState') <- deltaEntityPackets oldFrame newFrameLens showNet oldNum newNum oldState oldIndex
            (oldIndex'', oldNum'', oldState'') <- proceedParsing oldIndex' oldNum' oldState' newNum showNet bits'
            doParsePacketEntities oldFrame newFrameLens oldNum'' oldState'' oldIndex'' bits'
    proceedParsing oldIndex' oldNum' oldState' newNum showNet bits'
        | bits' .&. Constants.uRemove /= 0 = do
            when (showNet == 3) $
                Com.printf (B.concat ["   remove: ", encode newNum, "\n"])
            when (oldNum' /= newNum) $
                Com.printf "U_REMOVE: oldnum != newnum\n"
            parseFromPreviousState oldFrame oldIndex' oldState'
        | oldNum' == newNum = do
            when (showNet == 3) $
                Com.printf (B.concat ["   delta: ", encode newNum, "\n"])
            deltaEntity newFrameLens newNum oldState' bits'
            parseFromPreviousState oldFrame oldIndex' oldState'
        | oldNum' > newNum = do
            when (showNet == 3) $
                Com.printf (B.concat ["   baseline: ", encode newNum, "\n"])
            entity <- readRef (Ref newNum)
            deltaEntity newFrameLens newNum (Just (entity^.ceBaseline)) bits'
            return (oldIndex', oldNum', oldState')
        | otherwise =
            error "CLEnts.doParsePacketEntities#proceedParsing shouldn't happen"
    parseFromPreviousState :: Maybe FrameT -> Int -> Maybe EntityStateT -> Quake (Int, Int, Maybe EntityStateT)
    parseFromPreviousState Nothing _ _ = error "CLEnts.doParsePacketEntities oldFrame is Nothing"
    parseFromPreviousState (Just oldFrame') oldIndex' oldState'
        | oldIndex' + 1 >= oldFrame'^.fNumEntities =
            return (oldIndex' + 1, 99999, oldState')
        | otherwise = do
            parseEntities <- use (globals.gClParseEntities)
            let oldState'' = parseEntities V.! (((oldFrame'^.fParseEntities) + oldIndex' + 1) .&. (Constants.maxParseEntities - 1))
            return (oldIndex' + 1, oldState''^.esNumber, Just oldState'')

deltaEntityPackets :: Maybe FrameT -> Lens' QuakeState FrameT -> Float -> Int -> Int -> Maybe EntityStateT -> Int -> Quake (Int, Int, Maybe EntityStateT)
deltaEntityPackets oldFrame newFrameLens showNet oldNum newNum oldState oldIndex
    | oldNum >= newNum = return (oldIndex, oldNum, oldState)
    | otherwise = do
        when (showNet == 3) $
            Com.printf (B.concat ["   unchanged: ", encode oldNum, "\n"])
        deltaEntity newFrameLens oldNum oldState 0
        proceedDelta oldFrame
  where
    proceedDelta Nothing = error "CLEnts.deltaEntityPackets oldFrame is Nothing"
    proceedDelta (Just oldFrame')
        | oldIndex + 1 >= oldFrame'^.fNumEntities =
            deltaEntityPackets oldFrame newFrameLens showNet 99999 newNum oldState (oldIndex + 1)
        | otherwise = do
            parseEntities <- use (globals.gClParseEntities)
            let oldState' = parseEntities V.! (((oldFrame'^.fParseEntities) + (oldIndex + 1)) .&. (Constants.maxParseEntities - 1))
            deltaEntityPackets oldFrame newFrameLens showNet (oldState'^.esNumber) newNum (Just oldState') (oldIndex + 1)

copyRemainingEntities :: Maybe FrameT -> Lens' QuakeState FrameT -> Float -> Int -> Maybe EntityStateT -> Int -> Quake ()
copyRemainingEntities oldFrame newFrameLens showNet oldNum oldState oldIndex
    | oldNum == 99999 = return ()
    | otherwise = do -- one or more entities from the old packet are unchanged
        when (showNet == 3) $
            Com.printf (B.concat ["   unchanged: ", encode oldNum, "\n"])
        deltaEntity newFrameLens oldNum oldState 0
        proceedCopy oldFrame
  where
    proceedCopy Nothing = error "CLEnts.copyRemainingEntities oldFrame is Nothing"
    proceedCopy (Just oldFrame')
        | oldIndex + 1 >= oldFrame'^.fNumEntities =
            copyRemainingEntities oldFrame newFrameLens showNet 99999 oldState (oldIndex + 1)
        | otherwise = do
            parseEntities <- use (globals.gClParseEntities)
            let oldState' = parseEntities V.! (((oldFrame'^.fParseEntities) + oldIndex + 1) .&. (Constants.maxParseEntities - 1))
            copyRemainingEntities oldFrame newFrameLens showNet (oldState'^.esNumber) (Just oldState') (oldIndex + 1)

fireEntityEvents :: FrameT -> Quake ()
fireEntityEvents frame = do
    parseEntities <- use (globals.gClParseEntities)
    mapM_ (fireEntityEvent frame parseEntities) [0..(frame^.fNumEntities)-1]

fireEntityEvent :: FrameT -> V.Vector EntityStateT -> Int -> Quake ()
fireEntityEvent frame parseEntities idx = do
    when ((s1^.esEvent) /= 0) $
        CLFX.entityEvent s1
    -- EF_TELEPORTER acts like an event, but is not cleared each frame
    when ((s1^.esEffects) .&. Constants.efTeleporter /= 0) $
        CLFX.teleporterParticles s1
  where
    num = ((frame^.fParseEntities) + idx) .&. (Constants.maxParseEntities - 1)
    s1 = parseEntities V.! num

deltaEntity :: Traversal' QuakeState FrameT -> Int -> Maybe EntityStateT -> Int -> Quake ()
deltaEntity _ _ Nothing _ = error "CLEnts.deltaEntity old entity is Nothing"
deltaEntity frameLens newNum (Just old) bits = do
    idx <- getEntityIndex
    globals.gCl.csParseEntities += 1
    frameLens.fNumEntities += 1
    parseDelta old (globals.gClParseEntities.ix idx) newNum bits
    state <- getParseEntity idx
    resetServerFrame state =<< readRef entityRef
    ent <- readRef entityRef
    serverFrame <- use (globals.gCl.csFrame.fServerFrame)
    doDeltaEntity state ent serverFrame
    modifyRef entityRef (\v -> v & ceServerFrame .~ serverFrame
                                 & ceCurrent .~ state) -- Copy !
  where
    entityRef = Ref newNum
    getEntityIndex = do
        parseEntities <- use (globals.gCl.csParseEntities)
        return (parseEntities .&. (Constants.maxParseEntities - 1))
    getParseEntity :: Int -> Quake EntityStateT -- TODO: reuse it in this module where entities are being read ?
    getParseEntity idx = do
        parseEntities <- use (globals.gClParseEntities)
        return (parseEntities V.! idx)
    resetServerFrame state ent
        | ((state^.esModelIndex) /= (ent^.ceCurrent.esModelIndex) ||
          (state^.esModelIndex2) /= (ent^.ceCurrent.esModelIndex2) ||
          (state^.esModelIndex3) /= (ent^.ceCurrent.esModelIndex3) ||
          (state^.esModelIndex4) /= (ent^.ceCurrent.esModelIndex4) ||
          abs((state^.esOrigin._x) - (ent^.ceCurrent.esOrigin._x)) > 512 ||
          abs((state^.esOrigin._y) - (ent^.ceCurrent.esOrigin._y)) > 512 ||
          abs((state^.esOrigin._z) - (ent^.ceCurrent.esOrigin._z)) > 512 ||
          (state^.esEvent) == Constants.evPlayerTeleport ||
          (state^.esEvent) == Constants.evOtherTeleport) =
            modifyRef entityRef (\v -> v & ceServerFrame .~ -99)
        | otherwise = return ()
    doDeltaEntity state ent serverFrame
        | (ent^.ceServerFrame) /= serverFrame - 1 = do
            -- wasn't in last update, so initialize some things
            modifyRef entityRef (\v -> v & ceTrailCount .~ 1024 -- for diminishing rocket / grenade trails
                                         & cePrev .~ state) -- duplicate the current state so lerping doesn't hurt anything
            updateOrigin state
        | otherwise =
            -- shuffle the last state to previous Copy !
            modifyRef entityRef (\v -> v & cePrev .~ (ent^.ceCurrent))
    updateOrigin state
        | (state^.esEvent) == Constants.evOtherTeleport =
            modifyRef entityRef (\v -> v & cePrev.esOrigin .~ (state^.esOrigin)
                                         & ceLerpOrigin .~ (state^.esOrigin))
        | otherwise =
            modifyRef entityRef (\v -> v & cePrev.esOrigin .~ (state^.esOldOrigin)
                                         & ceLerpOrigin .~ (state^.esOldOrigin))

calcViewValues :: ClientStateT -> Quake ()
calcViewValues cl = do
    calculateOrigin =<< fmap (^.cvValue) clPredictCVar
    addLocalAngleMovement
    setAngleVectors =<< use (globals.gCl.csRefDef)
    -- interpolate field of view
    globals.gCl.csRefDef.rdFovX .= (ops'^.psFOV) + lerp * ((ps^.psFOV) - (ops'^.psFOV))
    -- don't interpolate blend color
    globals.gCl.csRefDef.rdBlend .= (ps^.psBlend)
    -- add the weapon
    addViewWeapon ps ops'
  where
    -- find the previous frame to interpolate from
    ps = cl^.csFrame.fPlayerState
    i = ((cl^.csFrame.fServerFrame) - 1) .&. Constants.updateMask
    oldFrame = (cl^.csFrames) V.! i
    oldFrame'
        | (oldFrame^.fServerFrame) /= (cl^.csFrame.fServerFrame) - 1 || not (oldFrame^.fValid) = cl^.csFrame -- previous frame was dropped or invalid
        | otherwise = oldFrame
    ops = oldFrame'^.fPlayerState
    -- see if the player entity was teleported this frame
    ops' | abs ((ops^.psPMoveState.pmsOrigin._x) - (ps^.psPMoveState.pmsOrigin._x)) > 256 * 8 ||
           abs ((ops^.psPMoveState.pmsOrigin._y) - (ps^.psPMoveState.pmsOrigin._y)) > 256 * 8 ||
           abs ((ops^.psPMoveState.pmsOrigin._z) - (ps^.psPMoveState.pmsOrigin._z)) > 256 * 8 = ps -- don't interpolate
         | otherwise = ops
    lerp = cl^.csLerpFrac
    calculateOrigin :: Float -> Quake ()
    calculateOrigin predict
        | predict /= 0 && (cl^.csFrame.fPlayerState.psPMoveState.pmsPMFlags) .&. Constants.pmfNoPrediction == 0 = do -- use predicted values
            globals.gCl.csRefDef.rdViewOrg .= (cl^.csPredictedOrigin)
                                            + (ops'^.psViewOffset)
                                            + (fmap (* (cl^.csLerpFrac)) ((ps^.psViewOffset) - (ops'^.psViewOffset)))
                                            - (fmap (* (1 - lerp)) (cl^.csPredictionError))
            -- smooth out stair climbing
            realTime <- use (globals.gCls.csRealTime)
            let delta = (realTime - (cl^.csPredictedStepTime))
            when (delta < 100) $
                globals.gCl.csRefDef.rdViewOrg._z -= (cl^.csPredictedStep) * fromIntegral (100 - delta) * 0.01
        | otherwise = do -- just use interpolated values
            let v = (fmap ((* 0.125) . fromIntegral) (ps^.psPMoveState.pmsOrigin))
                  + (ps^.psViewOffset)
                  - ((fmap ((* 0.125) . fromIntegral) (ops'^.psPMoveState.pmsOrigin)) + (ops'^.psViewOffset))
            globals.gCl.csRefDef.rdViewOrg .= (fmap ((* 0.125) . fromIntegral) (ops'^.psPMoveState.pmsOrigin))
                                            + (ops'^.psViewOffset)
                                            + (fmap (* lerp) v)
    addLocalAngleMovement
        | (cl^.csFrame.fPlayerState.psPMoveState.pmsPMType) < Constants.pmDead =
            globals.gCl.csRefDef.rdViewAngles .= (cl^.csPredictedAngles) + Math3D.lerpAngles (ops'^.psKickAngles) (ps^.psKickAngles) lerp -- use predicted values
        | otherwise =
            globals.gCl.csRefDef.rdViewAngles .= Math3D.lerpAngles (ops'^.psViewAngles) (ps^.psViewAngles) lerp + Math3D.lerpAngles (ops'^.psKickAngles) (ps^.psKickAngles) lerp -- just use interpolated values

setAngleVectors :: RefDefT -> Quake ()
setAngleVectors rd = do
    globals.gCl %= (\v -> v & csVForward .~ f
                            & csVRight .~ r
                            & csVUp .~ u)
  where
    (f, r, u) = Math3D.angleVectors (rd^.rdViewAngles) True True True

addViewWeapon :: PlayerStateT -> PlayerStateT -> Quake ()
addViewWeapon ps ops = do
    clGun <- fmap (^.cvValue) clGunCVar
    -- allow the gun to be completely removed (don't draw gun if in wide angle view)
    unless (clGun == 0 || (ps^.psFOV) > 90) $ do
        gunModel <- getGunModel
        maybe (return ()) (\gunModelRef -> setUpGun gunModelRef ps ops =<< use (globals.gCl)) gunModel
  where
    getGunModel = do
        gunModel <- use (globals.gGunModel)
        maybe getGunByIndex (\_ -> return gunModel) gunModel
    getGunByIndex = do
        modelDraw <- use (globals.gCl.csModelDraw)
        return (modelDraw V.! (ps^.psGunIndex))

setUpGun :: Ref ModelT -> PlayerStateT -> PlayerStateT -> ClientStateT -> Quake ()
setUpGun gunModelRef ps ops cl = do
    (frame, oldFrame) <- fmap getGunFrame (use (globals.gGunFrame))
    ClientV.addEntity (newEntityT & eModel .~ Just gunModelRef
                                  & eAngles .~ angles
                                  & eOrigin .~ origin
                                  & eFrame .~ frame
                                  & eOldOrigin .~ origin
                                  & eOldFrame .~ oldFrame
                                  & eBackLerp .~ 1 - (cl^.csLerpFrac)
                                  & enFlags .~ Constants.rfMinLight .|. Constants.rfDepthHack .|. Constants.rfWeaponModel)
  where
    origin = (cl^.csRefDef.rdViewOrg)
           + (ops^.psGunOffset)
           + (fmap (* (cl^.csLerpFrac)) ((ps^.psGunOffset) - (ops^.psGunOffset)))
    angles = (cl^.csRefDef.rdViewAngles)
           + (Math3D.lerpAngles (ops^.psGunAngles) (ps^.psGunAngles) (cl^.csLerpFrac))
    getGunFrame gunFrame
        | gunFrame /= 0 = (gunFrame, gunFrame)
        | (ps^.psGunFrame) == 0 = (0, 0)
        | otherwise = (ps^.psGunFrame, ops^.psGunFrame)

addPacketEntities :: FrameT -> Quake ()
addPacketEntities frame = do
    cl <- use (globals.gCl)
    let autoRotate = Math3D.angleMod (fromIntegral (cl^.csTime) / 10)
        autoAnim = 2 * (cl^.csTime) `div` 1000
    doAddPacketEntities frame autoRotate autoAnim newEntityT 0 (frame^.fNumEntities)

doAddPacketEntities :: FrameT -> Float -> Int -> EntityT -> Int -> Int -> Quake ()
doAddPacketEntities frame autoRotate autoAnim ent pNum maxPNum
    | pNum >= maxPNum = return ()
    | otherwise = do
        cl <- use (globals.gCl)
        s1 <- getParseEntity
        cent <- readRef (Ref (s1^.esNumber))
        let entFrame = setFrame autoAnim s1 (cl^.csTime)
            (effects, renderfx) = calcEffectsAndRenderFx s1
            entOldFrame = cent^.cePrev.esFrame
            entBackLerp = 1 - (cl^.csLerpFrac)
            (entOrigin, entOldOrigin) = calcOrigin cent renderfx (cl^.csLerpFrac)
        (entAlpha, entSkinNum, entSkin, entModel) <- tweakBeamsColor cl s1 ent renderfx
            -- only used for black hole model right now, FIXME: do better
        let entAlpha' | renderfx == Constants.rfTranslucent = 0.7
                      | otherwise = entAlpha
            -- render effects (fullbright, translucent, etc)
            entFlags | effects .&. Constants.efColorShell /= 0 = 0 -- renderfx go on color shell entity
                     | otherwise = renderfx
        entAngles <- calcAngles cl s1 entOrigin cent autoRotate effects
        let ent' = ent & eFrame     .~ entFrame
                       & eOldFrame  .~ entOldFrame
                       & eBackLerp  .~ entBackLerp
                       & eOrigin    .~ entOrigin
                       & eOldOrigin .~ entOldOrigin
                       & eAlpha     .~ entAlpha'
                       & eSkinNum   .~ entSkinNum
                       & eModel     .~ entModel
                       & eSkin      .~ entSkin
                       & enFlags    .~ entFlags
                       & eAngles    .~ entAngles
        proceedAddPacketEntities cl s1 effects entOrigin entFlags entAlpha' renderfx ent'
  where
    getParseEntity = do
        parseEntities <- use (globals.gClParseEntities)
        return (parseEntities V.! (((frame^.fParseEntities) + pNum) .&. (Constants.maxParseEntities - 1)))
    proceedAddPacketEntities cl s1 effects entOrigin entFlags entAlpha' renderfx ent'
        | (s1^.esNumber) == (cl^.csPlayerNum) + 1 = do
            addOriginEffects effects entOrigin
            doAddPacketEntities frame autoRotate autoAnim (ent' & enFlags %~ (.|. Constants.rfViewerModel)) (pNum + 1) maxPNum
        | (s1^.esModelIndex) == 0 =
            doAddPacketEntities frame autoRotate autoAnim ent' (pNum + 1) maxPNum
        | otherwise = do
            let (entFlags', entAlpha'') = updateFlagsAndAlpha entFlags entAlpha' effects
                ent'' = ent' & eAlpha     .~ entAlpha''
                             & enFlags    .~ entFlags'
            -- add to refresh list
            ClientV.addEntity ent''
            -- color shells generate a separate entity for the main model
            checkColorShells ent'' effects renderfx
                >>= checkModelIndex2 s1
                >>= checkModelIndex3 s1
                >>= checkModelIndex4 s1
                >>= checkPowerScreen effects
                >>= addAutomaticParticleTrails effects s1
                >>= copyOrigin s1
                >>= \v -> doAddPacketEntities frame autoRotate autoAnim v (pNum + 1) maxPNum

setFrame :: Int -> EntityStateT -> Int -> Int
setFrame autoAnim s1 time
    | effects .&. Constants.efAnim01 /= 0      = autoAnim .&. 1
    | effects .&. Constants.efAnim23 /= 0      = 2 + (autoAnim .&. 1)
    | effects .&. Constants.efAnimAll /= 0     = autoAnim
    | effects .&. Constants.efAnimAllFast /= 0 = time `div` 100
    | otherwise                                = s1^.esFrame
  where
    effects = s1^.esEffects

calcEffectsAndRenderFx :: EntityStateT -> (Int, Int)
calcEffectsAndRenderFx s1 = result
  where
    effects = s1^.esEffects
    renderfx = s1^.esRenderFx
    (effects', renderfx')
        | effects .&. Constants.efPent /= 0 =
            ((effects .&. (complement Constants.efPent)) .|. Constants.efColorShell, renderfx .|. Constants.rfShellRed)
        | otherwise = (effects, renderfx)
    (effects'', renderfx'')
        | effects' .&. Constants.efQuad /= 0 =
            ((effects' .&. (complement Constants.efQuad)) .|. Constants.efColorShell, renderfx' .|. Constants.rfShellBlue)
        | otherwise = (effects', renderfx')
    (effects''', renderfx''')
        | effects'' .&. Constants.efDouble /= 0 =
            ((effects'' .&. (complement Constants.efDouble)) .|. Constants.efColorShell, renderfx'' .|. Constants.rfShellDouble)
        | otherwise = (effects'', renderfx'')
    result
        | effects''' .&. Constants.efHalfDamage /= 0 =
            ((effects''' .&. (complement Constants.efHalfDamage)) .|. Constants.efColorShell, renderfx''' .|. Constants.rfShellHalfDam)
        | otherwise = (effects''', renderfx''')

calcOrigin :: CEntityT -> Int -> Float -> (V3 Float, V3 Float)
calcOrigin cent renderfx lerpFrac
    | renderfx .&. (Constants.rfFrameLerp .|. Constants.rfBeam) /= 0 =
        -- step origin discretely, because the frames
        -- do the animation properly
        (cent^.ceCurrent.esOrigin, cent^.ceCurrent.esOldOrigin)
    | otherwise = -- interpolate origin
        let v = (cent^.cePrev.esOrigin)
              + (fmap (* lerpFrac) ((cent^.ceCurrent.esOrigin) - (cent^.cePrev.esOrigin)))
        in (v, v)

tweakBeamsColor :: ClientStateT -> EntityStateT -> EntityT -> Int -> Quake (Float, Int, Maybe (Ref ImageT), Maybe (Ref ModelT))
tweakBeamsColor cl s1 ent renderfx
    | renderfx .&. Constants.rfBeam /= 0 = do -- the four beam colors are encoded in 32 bits of skinnum (hack)
        r <- fmap (`mod` 4) Lib.rand
        return (0.3, ((s1^.esSkinNum) `shiftR` (fromIntegral r * 8)) .&. 0xFF, ent^.eSkin, Nothing)
    | (s1^.esModelIndex) == 255 = do -- use custom player skin
        let skinNum = 0
            ci = (cl^.csClientInfo) V.! ((s1^.esSkinNum) .&. 0xFF)
            (skin, model)
                | isNothing (ci^.ciSkin) || isNothing (ci^.ciModel) = (cl^.csBaseClientInfo.ciSkin, cl^.csBaseClientInfo.ciModel)
                | otherwise = (ci^.ciSkin, ci^.ciModel)
        (skin', model') <- checkDisguise skin model
        return (ent^.eAlpha, skinNum, skin', model')
    | otherwise =
        return (ent^.eAlpha, s1^.esSkinNum, Nothing, (cl^.csModelDraw) V.! (s1^.esModelIndex))
  where
    checkDisguise skin model
        | renderfx .&. Constants.rfUseDisguise /= 0 = do
            renderer <- use (globals.gRenderer)
            doCheckDisguise skin model renderer
        | otherwise =
            return (skin, model)
    doCheckDisguise Nothing model _ = do
        Com.fatalError "CLEnts.tweakBeamsColor skin is Nothing"
        return (Nothing, model)
    doCheckDisguise skin model Nothing = do
        Com.fatalError "CLEnts.tweakBeamsColor renderer is Nothing"
        return (skin, model)
    doCheckDisguise skin@(Just skinRef) model (Just renderer) = do
        image <- readRef skinRef
        registerDisguise skin model image renderer

registerDisguise :: Maybe (Ref ImageT) -> Maybe (Ref ModelT) -> ImageT -> Renderer -> Quake (Maybe (Ref ImageT), Maybe (Ref ModelT))
registerDisguise skin model image renderer
    | "players/male" `BC.isPrefixOf` (image^.iName) = do
        s <- registerSkin "players/male/disguise.pcx"
        m <- registerModel "players/male/tris.md2"
        return (s, m)
    | "players/female" `BC.isPrefixOf` (image^.iName) = do
        s <- registerSkin "players/female/disguise.pcx"
        m <- registerModel "players/female/tris.md2"
        return (s, m)
    | "players/cyborg" `BC.isPrefixOf` (image^.iName) = do
        s <- registerSkin "players/cyborg/disguise.pcx"
        m <- registerModel "players/cyborg/tris.md2"
        return (s, m)
    | otherwise =
        return (skin, model)
  where
      registerSkin = renderer^.rRefExport.reRegisterSkin
      registerModel = renderer^.rRefExport.reRegisterModel

calcAngles :: ClientStateT -> EntityStateT -> V3 Float -> CEntityT -> Float -> Int -> Quake (V3 Float)
calcAngles cl s1 entOrigin cent autoRotate effects
    | effects .&. Constants.efRotate /= 0 = -- some bonus items
        return (V3 0 autoRotate 0)
      -- RAFAEL
    | effects .&. Constants.efSpinningLights /= 0 = do
        let result = V3 0 (Math3D.angleMod (fromIntegral (cl^.csTime) / 2) + (s1^.esAngles._y)) 180
            (forward, _, _) = Math3D.angleVectors result True False False
            start = entOrigin + fmap (* 64) forward
        ClientV.addLight start 100 1 0 0
        return result
      -- interpolate angles
    | otherwise =
        return (Math3D.lerpAngles (cent^.cePrev.esAngles) (cent^.ceCurrent.esAngles) (cl^.csLerpFrac))

updateFlagsAndAlpha :: Int -> Float -> Int -> (Int, Float)
updateFlagsAndAlpha entFlags entAlpha effects = result
  where
    (f, a)
        | effects .&. Constants.efBFG /= 0 = (entFlags .|. Constants.rfTranslucent, 0.3)
        | otherwise                        = (entFlags, entAlpha)
    (f', a')
        | effects .&. Constants.efPlasma /= 0 = (f .|. Constants.rfTranslucent, 0.6)
        | otherwise                           = (f, a)
    result
        | effects .&. Constants.efSphereTrans /= 0 = checkTrail
        | otherwise                                = (f', a')
    checkTrail
        | effects .&. Constants.efTrackerTrail /= 0 = (f' .|. Constants.rfTranslucent, 0.6)
        | otherwise                                 = (f' .|. Constants.rfTranslucent, 0.3)

addOriginEffects :: Int -> V3 Float -> Quake ()
addOriginEffects effects entOrigin
    | effects .&. Constants.efFlag1 /= 0 =
        ClientV.addLight entOrigin 225 1.0 0.1 0.1
    | effects .&. Constants.efFlag2 /= 0 =
        ClientV.addLight entOrigin 225 0.1 0.1 1.0
    | effects .&. Constants.efTagTrail /= 0 =
        ClientV.addLight entOrigin 225 1.0 1.0 0.0
    | effects .&. Constants.efTrackerTrail /= 0 =
        ClientV.addLight entOrigin 225 (-1) (-1) (-1)
    | otherwise =
        return ()

checkColorShells :: EntityT -> Int -> Int -> Quake EntityT
checkColorShells ent effects renderfx = do
    when (effects .&. Constants.efColorShell /= 0) $ do
        {-
        - PMM - at this point, all of the shells have been handled if
        - we're in the rogue pack, set up the custom mixing, otherwise
        - just keep going if(Developer_searchpath(2) == 2) { all of the
        - solo colors are fine. we need to catch any of the
        - combinations that look bad (double & half) and turn them into
        - the appropriate color, and make double/quad something special
        -}
        renderfx' <- checkHalfDamageShell
        renderfx'' <- checkDoubleDamageShell renderfx'
        ClientV.addEntity (ent & enFlags .~ renderfx'' .|. Constants.rfTranslucent
                               & eAlpha .~ 0.3)
    return (ent & eSkin    .~ Nothing -- never use a custom skin on others
                & eSkinNum .~ 0
                & enFlags  .~ 0
                & eAlpha   .~ 0)
  where
    checkHalfDamageShell
        | renderfx .&. Constants.rfShellHalfDam /= 0 = do
            v <- FS.developerSearchPath 2
            -- ditch the half damage shell if any of red, blue, or double are on
            return (ditchHalfDamageShell v)
        | otherwise =
            return renderfx
    ditchHalfDamageShell v
        | v == 2 && renderfx .&. (Constants.rfShellRed .|. Constants.rfShellBlue .|. Constants.rfShellDouble) /= 0 =
            renderfx .&. (complement Constants.rfShellHalfDam)
        | otherwise = renderfx
    checkDoubleDamageShell renderfx'
        | renderfx' .&. Constants.rfShellDouble /= 0 = do
            v <- FS.developerSearchPath 2
            return (adjustShells renderfx' v)
        | otherwise =
            return renderfx'
    adjustShells renderfx' v
        | v == 2 =
            -- lose the yellow shell if we have a red, blue, or green shell
            let r | renderfx' .&. (Constants.rfShellRed .|. Constants.rfShellBlue .|. Constants.rfShellGreen) /= 0 =
                      renderfx' .&. (complement Constants.rfShellDouble)
                  | otherwise =
                      renderfx'
                -- if we have a red shell, turn it to purple by adding blue
                r' | r .&. Constants.rfShellRed /= 0 =
                       r .|. Constants.rfShellBlue
                   -- if we have a blue shell (and not a red shell), turn it to cyan by adding green
                   | r .&. Constants.rfShellBlue /= 0 =
                       -- go to green if it's on already, otherwise do cyan (flash green)
                       if r .&. Constants.rfShellGreen /= 0
                           then r .&. (complement Constants.rfShellBlue)
                           else r .|. Constants.rfShellGreen
                   | otherwise =
                       r
            in r'
        | otherwise =
            renderfx'

checkModelIndex2 :: EntityStateT -> EntityT -> Quake EntityT
checkModelIndex2 s1 ent
    | (s1^.esModelIndex2) /= 0 = do
        model <- getWeaponModel s1
        -- PMM - check for the defender sphere shell .. make it translucent
        -- replaces the previous version which used the high bit on
        -- modelindex2 to determine transparency
        configString <- getConfigString
        ClientV.addEntity (buildEnt model configString)
        -- PGM - make sure these get reset
        return (ent & eModel  .~ model
                    & eAlpha  .~ 0
                    & enFlags .~ 0)
    | otherwise =
        return ent
  where
    getConfigString = do
        configStrings <- use (globals.gCl.csConfigStrings)
        return (configStrings V.! (Constants.csModels + (s1^.esModelIndex2)))
    buildEnt model configString
        | BC.map toLower configString == "models/items/shell/tris.md2" =
            ent & eModel .~ model
                & eAlpha .~ 0.32
                & enFlags .~ Constants.rfTranslucent
        | otherwise =
            ent & eModel .~ model

getWeaponModel :: EntityStateT -> Quake (Maybe (Ref ModelT))
getWeaponModel s1
    | (s1^.esModelIndex2) == 255 = do -- custom weapon
        ci <- getClientInfo
        clVwep <- fmap (^.cvValue) clVwepCVar
        let i = (s1^.esSkinNum) `shiftR` 8 -- 0 is default weapon model
            i' | clVwep == 0 || i > Constants.maxClientWeaponModels - 1 = 0
               | otherwise = i
            model = (ci^.ciWeaponModel) V.! i'
        doGetWeaponModel ci i' model
    | otherwise = do
        modelDraw <- use (globals.gCl.csModelDraw)
        return (modelDraw V.! (s1^.esModelIndex2))
  where
    getClientInfo = do
        clientInfo <- use (globals.gCl.csClientInfo)
        return (clientInfo V.! ((s1^.esSkinNum) .&. 0xFF))

doGetWeaponModel :: ClientInfoT -> Int -> Maybe (Ref ModelT) -> Quake (Maybe (Ref ModelT))
doGetWeaponModel ci i' Nothing =
    maybe getBaseWeapon (\_ -> return m) m
  where
    m | i' /= 0 = (ci^.ciWeaponModel) V.! 0
      | otherwise = Nothing
    getBaseWeapon = do
        baseClientInfo <- use (globals.gCl.csBaseClientInfo)
        return ((baseClientInfo^.ciWeaponModel) V.! 0)
doGetWeaponModel _ _ model = return model

checkModelIndex3 :: EntityStateT -> EntityT -> Quake EntityT
checkModelIndex3 s1 ent
    | (s1^.esModelIndex3) /= 0 = do
        model <- getModel
        let ent' = ent & eModel .~ model
        ClientV.addEntity ent'
        return ent'
    | otherwise =
        return ent
  where
    getModel = do
        modelDraw <- use (globals.gCl.csModelDraw)
        return (modelDraw V.! (s1^.esModelIndex3))

checkModelIndex4 :: EntityStateT -> EntityT -> Quake EntityT
checkModelIndex4 s1 ent
    | (s1^.esModelIndex4) /= 0 = do
        model <- getModel
        let ent' = ent & eModel .~ model
        ClientV.addEntity ent'
        return ent'
    | otherwise =
        return ent
  where
    getModel = do
        modelDraw <- use (globals.gCl.csModelDraw)
        return (modelDraw V.! (s1^.esModelIndex4))

checkPowerScreen :: Int -> EntityT -> Quake EntityT
checkPowerScreen effects ent
    | effects .&. Constants.efPowerScreen /= 0 = do
        model <- use (clTEntGlobals.clteModPowerScreen)
        let ent' = ent & eModel    .~ model
                       & eOldFrame .~ 0
                       & eFrame    .~ 0
                       & enFlags   .~ (ent^.enFlags) .|. Constants.rfTranslucent .|. Constants.rfShellGreen
                       & eAlpha    .~ 0.3
        ClientV.addEntity ent'
        return ent'
    | otherwise =
        return ent

-- TODO: refactor this, old implementation
addAutomaticParticleTrails :: Int -> EntityStateT -> EntityT -> Quake EntityT
addAutomaticParticleTrails effects s1 ent
    | effects .&. (complement Constants.efRotate) /= 0 = do
        cent <- readRef (Ref (s1^.esNumber))
        doAddAutomaticParticleTrails cent
    | otherwise =
        return ent
  where
    doAddAutomaticParticleTrails cent
        | effects .&. Constants.efRocket /= 0 = do
            CLFX.rocketTrail (cent^.ceLerpOrigin) (ent^.eOrigin) (s1^.esNumber)
            ClientV.addLight (ent^.eOrigin) 200 1 1 0
            return ent
        | effects .&. Constants.efBlaster /= 0 = do
            if effects .&. Constants.efTracker /= 0 -- lame... problematic?
              then do
                CLNewFX.blasterTrail2 (cent^.ceLerpOrigin) (ent^.eOrigin)
                ClientV.addLight (ent^.eOrigin) 200 0 1 0
              else do
                CLFX.blasterTrail (cent^.ceLerpOrigin) (ent^.eOrigin)
                ClientV.addLight (ent^.eOrigin) 200 1 1 0
            return ent
        | effects .&. Constants.efHyperblaster /= 0 = do
            if effects .&. Constants.efTracker /= 0 -- PGM overloaded for blaster2
              then ClientV.addLight (ent^.eOrigin) 200 0 1 0
              else ClientV.addLight (ent^.eOrigin) 200 1 1 0
            return ent
        | effects .&. Constants.efGib /= 0 = do
            CLFX.diminishingTrail (cent^.ceLerpOrigin) (ent^.eOrigin) (s1^.esNumber) effects
            return ent
        | effects .&. Constants.efGrenade /= 0 = do
            CLFX.diminishingTrail (cent^.ceLerpOrigin) (ent^.eOrigin) (s1^.esNumber) effects
            return ent
        | effects .&. Constants.efFlies /= 0 = do
            CLFX.flyEffect (s1^.esNumber) (ent^.eOrigin)
            return ent
        | effects .&. Constants.efBFG /= 0 = do
            i <- if effects .&. Constants.efAnimAllFast /= 0
                   then do
                     CLFX.bfgParticles ent
                     return 200
                   else
                     return (bfgLightRamp UV.! (s1^.esFrame))
            ClientV.addLight (ent^.eOrigin) (fromIntegral i) 0 1 0
            return ent
        | effects .&. Constants.efTrap /= 0 = do
            let origin@(V3 a b c) = ent^.eOrigin
                ent' = ent { _eOrigin = V3 a b (c + 32) }
            CLFX.trapParticles ent'
            r <- Lib.rand
            let i = fromIntegral (r `mod` 100) + 100
            ClientV.addLight (ent'^.eOrigin) i 1 0.8 0.1
            return ent'
        | effects .&. Constants.efFlag1 /= 0 = do
            CLFX.flagTrail (cent^.ceLerpOrigin) (ent^.eOrigin) 242
            ClientV.addLight (ent^.eOrigin) 225 1 0.1 0.1
            return ent
        | effects .&. Constants.efFlag2 /= 0 = do
            CLFX.flagTrail (cent^.ceLerpOrigin) (ent^.eOrigin) 115
            ClientV.addLight (ent^.eOrigin) 225 0.1 0.1 1
            return ent
        | effects .&. Constants.efTagTrail /= 0 = do
            CLNewFX.tagTrail (cent^.ceLerpOrigin) (ent^.eOrigin) 220
            ClientV.addLight (ent^.eOrigin) 225 1.0 1.0 0.0
            return ent
        | effects .&. Constants.efTrackerTrail /= 0 =
            if effects .&. Constants.efTracker /= 0
              then do
                time <- use (globals.gCl.csTime)
                let intensity = 50 + (500 * (sin (fromIntegral time / 500.0) + 1.0))
                -- FIXME: check out this effect in rendition
                -- TODO: there is an extra check in jake2 that we skipped
                ClientV.addLight (ent^.eOrigin) intensity (-1.0) (-1.0) (-1.0)
                return ent
              else do
                CLNewFX.trackerShell (cent^.ceLerpOrigin)
                ClientV.addLight (ent^.eOrigin) 155 (-1.0) (-1.0) (-1.0)
                return ent
        | effects .&. Constants.efTracker /= 0 = do
            CLNewFX.trackerTrail (cent^.ceLerpOrigin) (ent^.eOrigin) 0
            -- FIXME: check out this effect in rendition
            -- TODO: there is an extra check in jake2 that we skipped
            ClientV.addLight (ent^.eOrigin) 200 (-1) (-1) (-1)
            return ent
        | effects .&. Constants.efGreenGib /= 0 = do
            CLFX.diminishingTrail (cent^.ceLerpOrigin) (ent^.eOrigin) (s1^.esNumber) effects
            return ent
        | effects .&. Constants.efIonRipper /= 0 = do
            CLFX.ionRipperTrail (cent^.ceLerpOrigin) (ent^.eOrigin)
            ClientV.addLight (ent^.eOrigin) 100 1 0.5 0.5
            return ent
        | effects .&. Constants.efBlueHyperblaster /= 0 = do
            ClientV.addLight (ent^.eOrigin) 200 0 0 1
            return ent
        | effects .&. Constants.efPlasma /= 0 = do
            when (effects .&. Constants.efAnimAllFast /= 0) $
              CLFX.blasterTrail (cent^.ceLerpOrigin) (ent^.eOrigin)
            ClientV.addLight (ent^.eOrigin) 130 1 0.5 0.5
            return ent
        | otherwise =
            return ent

copyOrigin :: EntityStateT -> EntityT -> Quake EntityT
copyOrigin s1 ent = do
    modifyRef (Ref (s1^.esNumber)) (\v -> v & ceLerpOrigin .~ (ent^.eOrigin))
    return ent
