module Game.GameBase
    ( clipVelocity
    , findByClass
    , findByTarget
    , getGameApi
    , gFind
    , runFrame
    , setMoveDir
    , shutdownGame
    , touchTriggers
    ) where

import           Control.Lens           (use, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad          (when, unless, void, (>=>))
import           Data.Bits              ((.&.), (.|.))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import           Data.Char              (toLower)
import           Data.Maybe             (isJust)
import qualified Data.Vector            as V
import           Linear                 (V3(..), dot, _z)

import qualified Client.M               as M
import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI            as GameAI
import           Game.GClientT
import           Game.LevelLocalsT
import qualified Game.PlayerClient      as PlayerClient
import qualified Game.PlayerView        as PlayerView
import qualified QCommon.Com            as Com
import qualified QCommon.CVar           as CVar
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import qualified Server.SV              as SV
import qualified Server.SVWorld         as SVWorld
import           Types
import           Util.Binary            (encode)
import qualified Util.Math3D            as Math3D

vecUp :: V3 Float
vecUp = V3 0 (-1) 0

moveDirUp :: V3 Float
moveDirUp = V3 0 0 1

vecDown :: V3 Float
vecDown = V3 0 (-2) 0

moveDirDown :: V3 Float
moveDirDown = V3 0 0 (-1)

stopEpsilon :: Float
stopEpsilon = 0.1

findByClass :: EdictT -> B.ByteString -> Bool
findByClass e s = BC.map toLower (e^.eClassName) == BC.map toLower s

findByTarget :: EdictT -> B.ByteString -> Bool
findByTarget e s = doFindByTarget (e^.eTargetName)
  where
    doFindByTarget Nothing = False
    doFindByTarget (Just targetName) = (BC.map toLower targetName) == (BC.map toLower s)

getGameApi :: GameImportT -> Quake ()
getGameApi imp = gameBaseGlobals.gbGameImport .= (imp & giPointContents .~ SVWorld.pointContents)

gFind :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref EdictT))
gFind = error "GameBase.gFind" -- TODO

runFrame :: Quake ()
runFrame = do
    updateLevelGlobals
    GameAI.aiSetSightClient
    checkIntermission =<< use (gameBaseGlobals.gbLevel)
  where
    checkIntermission level
        | level^.llExitIntermission = exitLevel
        | otherwise = do
            numEdicts <- use (gameBaseGlobals.gbNumEdicts)
            mapM_ (readEdict >=> treatObject) [0..numEdicts-1]
            checkDMRules
            checkNeedPassword
            clientEndServerFrames

updateLevelGlobals :: Quake ()
updateLevelGlobals = do
    level <- use (gameBaseGlobals.gbLevel)
    gameBaseGlobals.gbLevel.llFrameNum += 1
    gameBaseGlobals.gbLevel.llTime .= (fromIntegral (level^.llFrameNum) + 1) * Constants.frameTime

exitLevel :: Quake ()
exitLevel = do
    changeMap <- use (gameBaseGlobals.gbLevel.llChangeMap)
    addCommandString <- use (gameBaseGlobals.gbGameImport.giAddCommandString)
    addCommandString (B.concat ["gamemap \"", changeMap, "\"\n"])
    gameBaseGlobals.gbLevel %= (\v -> v & llChangeMap .~ B.empty -- TODO: we sure? jake2 has NULL here
                                        & llExitIntermission .~ False
                                        & llIntermissionTime .~ 0)
    clientEndServerFrames
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    mapM_ (readEdict >=> updateEdictHealth) [1..maxClients]
  where
    updateEdictHealth (edictRef, edict)
        | (edict^.eInUse) = maybe clientError (updateClientHealth edictRef edict) (edict^.eClient)
        | otherwise = return ()
    clientError = Com.fatalError "GameBase.exitLevel edict^.eClient is Nothing"
    updateClientHealth edictRef edict clientRef = do
        client <- readRef clientRef
        when ((edict^.eHealth) > (client^.gcPers.cpMaxHealth)) $
            modifyRef edictRef (\v -> v & eHealth .~ (client^.gcPers.cpMaxHealth))

treatObject :: (Ref EdictT, EdictT) -> Quake ()
treatObject (edictRef, edict)
    | not (edict^.eInUse) = return ()
    | otherwise = do
        gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef
        modifyRef edictRef (\v -> v & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))
        checkGroundEntity edictRef edict (edict^.eGroundEntity)
        treatClientOrEntity edictRef =<< fmap (truncate . (^.cvValue)) maxClientsCVar

checkGroundEntity :: Ref EdictT -> EdictT -> Maybe (Ref EdictT) -> Quake ()
checkGroundEntity _ _ Nothing = return ()
checkGroundEntity edictRef edict (Just groundRef) = do
    ground <- readRef groundRef
    when ((ground^.eLinkCount) /= (edict^.eGroundEntityLinkCount)) $ do
        modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
        when ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 && (edict^.eFlags) .&. Constants.svfMonster /= 0) $
            M.checkGround edictRef

treatClientOrEntity :: Ref EdictT -> Int -> Quake ()
treatClientOrEntity edictRef@(Ref idx) maxClients
    | idx > 0 && idx <= maxClients = PlayerClient.clientBeginServerFrame edictRef
    | otherwise = runEntity edictRef

checkDMRules :: Quake ()
checkDMRules = do
    intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
    deathmatch <- deathmatchCVar
    dmRules intermissionTime deathmatch
  where
    dmRules intermissionTime deathmatch
        | intermissionTime /= 0 || (deathmatch^.cvValue) == 0 = return ()
        | otherwise = do
            timeLimit <- fmap (^.cvValue) timeLimitCVar
            fragLimit <- fmap (truncate . (^.cvValue)) fragLimitCVar
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            bprintf <- use (gameBaseGlobals.gbGameImport.giBprintf)
            applyDMRules timeLimit fragLimit levelTime bprintf

applyDMRules :: Float -> Int -> Float -> (Int -> B.ByteString -> Quake ()) -> Quake ()
applyDMRules timeLimit fragLimit levelTime bprintf
    | timeLimit /= 0 && levelTime >= timeLimit * 60 = do
        bprintf Constants.printHigh "Timelimit hit.\n"
        endDMLevel
    | fragLimit /= 0 = do
        maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
        shouldEnd <- or <$> mapM (readEdict >=> checkScore fragLimit) [1..maxClients]
        when shouldEnd $ do
            bprintf Constants.printHigh "Fraglimit hit.\n"
            endDMLevel
    | otherwise = return ()

checkScore :: Int -> (Ref EdictT, EdictT) -> Quake Bool
checkScore fragLimit (Ref idx, edict)
    | edict^.eInUse = do
        client <- readRef (Ref (idx - 1))
        return ((client^.gcResp.crScore) >= fragLimit)
    | otherwise = return False

checkNeedPassword :: Quake ()
checkNeedPassword = do
    password <- infoPasswordCVar
    spectatorPassword <- spectatorPasswordCVar
    checkPassword password spectatorPassword

checkPassword :: CVarT -> CVarT -> Quake ()
checkPassword password spectatorPassword
    | (password^.cvModified) || (spectatorPassword^.cvModified) = do
        CVar.update (password & cvModified .~ False)
        CVar.update (spectatorPassword & cvModified .~ False)
        cVarSet <- use (gameBaseGlobals.gbGameImport.giCVarSet)
        void (cVarSet "needpass" (encode need))
    | otherwise = return ()
  where
    needPassword = BC.map toLower (password^.cvString) /= "none"
    needSpectatorPassword = BC.map toLower (spectatorPassword^.cvString) /= "none"
    need | needPassword && needSpectatorPassword = 3 :: Int
         | needSpectatorPassword = 2
         | needPassword = 1
         | otherwise = 0

clientEndServerFrames :: Quake ()
clientEndServerFrames = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    mapM_ (readEdict >=> checkEdict) [1..maxClients]
  where
    checkEdict (edictRef, edict)
        | (edict^.eInUse) && isJust (edict^.eClient) =
            PlayerView.clientEndServerFrame edictRef
        | otherwise = return ()

runEntity :: Ref EdictT -> Quake ()
runEntity edictRef = do
    edict <- readRef edictRef
    maybe (return ()) doPreThink (edict^.ePrethink)
    makeNextMove edictRef (edict^.eMoveType)
  where
    doPreThink e = void (entThink e edictRef)

makeNextMove :: Ref EdictT -> Int -> Quake ()
makeNextMove edictRef moveType
    | any (== moveType) [Constants.moveTypePush, Constants.moveTypeStop] = SV.physicsPusher edictRef
    | moveType == Constants.moveTypeNone = SV.physicsNone edictRef
    | moveType == Constants.moveTypeNoClip = SV.physicsNoClip edictRef
    | moveType == Constants.moveTypeStep = SV.physicsStep edictRef
    | any (== moveType) [Constants.moveTypeToss, Constants.moveTypeBounce, Constants.moveTypeFly, Constants.moveTypeFlyMissile] = SV.physicsToss edictRef
    | otherwise = do
        err <- use (gameBaseGlobals.gbGameImport.giError)
        err ("SV_Physics: bad movetype " `B.append` encode moveType)

endDMLevel :: Quake ()
endDMLevel = error "GameBase.endDMLevel" -- TODO

setMoveDir :: Ref EdictT -> EdictT -> Quake ()
setMoveDir edictRef edict =
    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                & eMoveDir .~ moveDir)
  where
    angles = edict^.eEntityState.esAngles
    moveDir | angles == vecUp = moveDirUp
            | angles == vecDown = moveDirDown
            | otherwise = let (forward, _, _) = Math3D.angleVectors angles True False False
                          in forward

shutdownGame :: Quake ()
shutdownGame = do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    (gameImport^.giDprintf) "==== ShutdownGame ====\n"

readEdict :: Int -> Quake (Ref EdictT, EdictT) -- TODO: duplicate with something in SVInit
readEdict idx = do
    edict <- readRef (Ref idx)
    return (Ref idx, edict)

touchTriggers :: Ref EdictT -> Quake ()
touchTriggers edictRef = do
    edict <- readRef edictRef
    -- dead things don't activate triggers!
    unless ((isJust (edict^.eClient) || (edict^.eSvFlags) .&. Constants.svfMonster /= 0) && (edict^.eHealth <= 0)) $ do
      boxEdicts <- use (gameBaseGlobals.gbGameImport.giBoxEdicts)
      num <- boxEdicts (edict^.eAbsMin) (edict^.eAbsMax) (gameBaseGlobals.gbTouch) Constants.maxEdicts Constants.areaTriggers
      -- be careful, it is possible to have an entity in this
      -- list removed before we got to it (killtriggered)
      touchEdicts edictRef 0 num

touchEdicts :: Ref EdictT -> Int -> Int -> Quake ()
touchEdicts edictRef idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        edicts <- use (gameBaseGlobals.gbTouch)
        let hitRef = edicts V.! idx
        hit <- readRef hitRef
        doTouchEdicts hitRef hit (hit^.eTouch)
  where
    doTouchEdicts _ _ Nothing =
        touchEdicts edictRef (idx + 1) maxIdx
    doTouchEdicts hitRef hit (Just hitTouch)
        | not (hit^.eInUse) =
            touchEdicts edictRef (idx + 1) maxIdx
        | otherwise = do
            dummyPlane <- use (gameBaseGlobals.gbDummyPlane)
            entTouch hitTouch hitRef edictRef dummyPlane Nothing

clipVelocity :: V3 Float -> V3 Float -> Float -> (Int, V3 Float)
clipVelocity v3in normal overbounce =
    let isBlocked | normal^._z > 0  = 1 -- floor
                  | normal^._z == 0 = 2 -- step
                  | otherwise       = 0
        backoff = (dot v3in normal) * overbounce
        change = fmap (* backoff) normal
        out = v3in - change
        v3out = fmap (\v -> if v > (-stopEpsilon) && v < stopEpsilon then 0 else v) out
    in (isBlocked, v3out)
