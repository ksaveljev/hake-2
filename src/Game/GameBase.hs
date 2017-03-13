module Game.GameBase
    ( findByClass
    , getGameApi
    , gFind
    , runFrame
    , setMoveDir
    , shutdownGame
    ) where

import           Control.Lens (use, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad (when, void, (>=>))
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Maybe (isJust)
import           Linear (V3(..))

import qualified Client.M as M
import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI as GameAI
import           Game.GClientT
import           Game.LevelLocalsT
import qualified Game.PlayerClient as PlayerClient
import qualified Game.PlayerView as PlayerView
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import qualified Server.SV as SV
import qualified Server.SVWorld as SVWorld
import           Types
import qualified Util.Math3D as Math3D
import           Util.Binary (encode)

vecUp :: V3 Float
vecUp = V3 0 (-1) 0

moveDirUp :: V3 Float
moveDirUp = V3 0 0 1

vecDown :: V3 Float
vecDown = V3 0 (-2) 0

moveDirDown :: V3 Float
moveDirDown = V3 0 0 (-1)

runFrame :: Quake ()
runFrame =
  do updateLevelGlobals
     GameAI.aiSetSightClient
     checkIntermission =<< use (gameBaseGlobals.gbLevel)
  where checkIntermission level
          | level^.llExitIntermission = exitLevel
          | otherwise =
              do numEdicts <- use (gameBaseGlobals.gbNumEdicts)
                 mapM_ (readEdict >=> treatObject) [0..numEdicts-1]
                 checkDMRules
                 checkNeedPassword
                 clientEndServerFrames

readEdict :: Int -> Quake (Ref' EdictT, EdictT) -- TODO: duplicate with something in SVInit
readEdict idx =
  do edict <- readRef (Ref Constants.noParent idx)
     return (Ref Constants.noParent idx, edict)

updateLevelGlobals :: Quake ()
updateLevelGlobals =
  do level <- use (gameBaseGlobals.gbLevel)
     gameBaseGlobals.gbLevel.llFrameNum += 1
     gameBaseGlobals.gbLevel.llTime .= (fromIntegral (level^.llFrameNum) + 1) * Constants.frameTime

treatObject :: (Ref' EdictT, EdictT) -> Quake ()
treatObject (edictRef, edict)
  | not (edict^.eInUse) = return ()
  | otherwise =
      do gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef
         modifyRef edictRef (\v -> v & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))
         checkGroundEntity edictRef edict (edict^.eGroundEntity)
         treatClientOrEntity edictRef =<< fmap (truncate . (^.cvValue)) maxClientsCVar

checkGroundEntity :: Ref' EdictT -> EdictT -> Maybe (Ref' EdictT) -> Quake ()
checkGroundEntity _ _ Nothing = return ()
checkGroundEntity edictRef edict (Just groundRef) =
  do ground <- readRef groundRef
     when ((ground^.eLinkCount) /= (edict^.eGroundEntityLinkCount)) $
       do modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
          when ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 && (edict^.eFlags) .&. Constants.svfMonster /= 0) $
            M.checkGround edictRef

treatClientOrEntity :: Ref' EdictT -> Int -> Quake ()
treatClientOrEntity edictRef@(Ref _ idx) maxClients
  | idx > 0 && idx <= maxClients = PlayerClient.clientBeginServerFrame edictRef
  | otherwise = runEntity edictRef

setMoveDir :: Ref' EdictT -> EdictT -> Quake ()
setMoveDir edictRef edict =
  modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                              & eMoveDir .~ moveDir)
  where angles = edict^.eEntityState.esAngles
        moveDir | angles == vecUp = moveDirUp
                | angles == vecDown = moveDirDown
                | otherwise = let (forward, _, _) = Math3D.angleVectors angles True False False
                              in forward

shutdownGame :: Quake ()
shutdownGame =
  do gameImport <- use (gameBaseGlobals.gbGameImport)
     (gameImport^.giDprintf) "==== ShutdownGame ====\n"

getGameApi :: GameImportT -> Quake ()
getGameApi imp =
  gameBaseGlobals.gbGameImport .= (imp & giPointContents .~ SVWorld.pointContents)

exitLevel :: Quake ()
exitLevel =
  do changeMap <- use (gameBaseGlobals.gbLevel.llChangeMap)
     addCommandString <- use (gameBaseGlobals.gbGameImport.giAddCommandString)
     addCommandString (B.concat ["gamemap \"", changeMap, "\"\n"])
     gameBaseGlobals.gbLevel %= (\v -> v & llChangeMap .~ B.empty -- TODO: we sure? jake2 has NULL here
                                         & llExitIntermission .~ False
                                         & llIntermissionTime .~ 0)
     clientEndServerFrames
     maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     mapM_ (readEdict >=> updateEdictHealth) [1..maxClients]
  where updateEdictHealth (edictRef, edict)
          | (edict^.eInUse) = maybe clientError (updateClientHealth edictRef edict) (edict^.eClient)
          | otherwise = return ()
        clientError = Com.fatalError "GameBase.exitLevel edict^.eClient is Nothing"
        updateClientHealth edictRef edict clientRef =
          do client <- readRef clientRef
             when ((edict^.eHealth) > (client^.gcPers.cpMaxHealth)) $
               modifyRef edictRef (\v -> v & eHealth .~ (client^.gcPers.cpMaxHealth))

checkDMRules :: Quake ()
checkDMRules =
  do intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
     deathmatch <- deathmatchCVar
     dmRules intermissionTime deathmatch
  where dmRules intermissionTime deathmatch
          | intermissionTime /= 0 || (deathmatch^.cvValue) == 0 = return ()
          | otherwise =
              do timeLimit <- fmap (^.cvValue) timeLimitCVar
                 fragLimit <- fmap (truncate . (^.cvValue)) fragLimitCVar
                 levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                 bprintf <- use (gameBaseGlobals.gbGameImport.giBprintf)
                 applyDMRules timeLimit fragLimit levelTime bprintf

applyDMRules :: Float -> Int -> Float -> (Int -> B.ByteString -> Quake ()) -> Quake ()
applyDMRules timeLimit fragLimit levelTime bprintf
  | timeLimit /= 0 && levelTime >= timeLimit * 60 =
      do bprintf Constants.printHigh "Timelimit hit.\n"
         endDMLevel
  | fragLimit /= 0 =
      do maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
         shouldEnd <- or <$> mapM (readEdict >=> checkScore fragLimit) [1..maxClients]
         when shouldEnd $
           do bprintf Constants.printHigh "Fraglimit hit.\n"
              endDMLevel
  | otherwise = return ()

checkScore :: Int -> (Ref' EdictT, EdictT) -> Quake Bool
checkScore fragLimit (Ref _ idx, edict)
  | edict^.eInUse =
      do client <- readRef (Ref Constants.noParent (idx - 1))
         return ((client^.gcResp.crScore) >= fragLimit)
  | otherwise = return False

checkNeedPassword :: Quake ()
checkNeedPassword =
  do password <- infoPasswordCVar
     spectatorPassword <- spectatorPasswordCVar
     checkPassword password spectatorPassword

checkPassword :: CVarT -> CVarT -> Quake ()
checkPassword password spectatorPassword
  | (password^.cvModified) || (spectatorPassword^.cvModified) =
      do CVar.update (password & cvModified .~ False)
         CVar.update (spectatorPassword & cvModified .~ False)
         cVarSet <- use (gameBaseGlobals.gbGameImport.giCVarSet)
         void (cVarSet "needpass" (encode need))
  | otherwise = return ()
  where needPassword = BC.map toLower (password^.cvString) /= "none"
        needSpectatorPassword = BC.map toLower (spectatorPassword^.cvString) /= "none"
        need | needPassword && needSpectatorPassword = 3 :: Int
             | needSpectatorPassword = 2
             | needPassword = 1
             | otherwise = 0

clientEndServerFrames :: Quake ()
clientEndServerFrames =
  do maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     mapM_ (readEdict >=> checkEdict) [1..maxClients]
  where checkEdict (edictRef, edict)
          | (edict^.eInUse) && isJust (edict^.eClient) =
              PlayerView.clientEndServerFrame edictRef
          | otherwise = return ()

runEntity :: Ref' EdictT -> Quake ()
runEntity edictRef =
  do edict <- readRef edictRef
     maybe (return ()) doPreThink (edict^.ePrethink)
     makeNextMove edictRef (edict^.eMoveType)
  where doPreThink e = void (entThink e edictRef)

makeNextMove :: Ref' EdictT -> Int -> Quake ()
makeNextMove edictRef moveType
  | any (== moveType) [Constants.moveTypePush, Constants.moveTypeStop] = SV.physicsPusher edictRef
  | moveType == Constants.moveTypeNone = SV.physicsNone edictRef
  | moveType == Constants.moveTypeNoClip = SV.physicsNoClip edictRef
  | moveType == Constants.moveTypeStep = SV.physicsStep edictRef
  | any (== moveType) [Constants.moveTypeToss, Constants.moveTypeBounce, Constants.moveTypeFly, Constants.moveTypeFlyMissile] = SV.physicsToss edictRef
  | otherwise =
      do err <- use (gameBaseGlobals.gbGameImport.giError)
         err ("SV_Physics: bad movetype " `B.append` encode moveType)

endDMLevel :: Quake ()
endDMLevel = error "GameBase.endDMLevel" -- TODO

gFind :: Maybe (Ref' EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref' EdictT))
gFind = error "GameBase.gFind" -- TODO

findByClass :: EdictT -> B.ByteString -> Bool
findByClass e s = BC.map toLower (e^.eClassName) == BC.map toLower s