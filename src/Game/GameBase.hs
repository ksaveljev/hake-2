module Game.GameBase
  ( getGameApi
  , runFrame
  , setMoveDir
  , shutdownGame
  ) where

import qualified Client.M as M
import qualified Constants
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameAI as GameAI
import           Game.GClientT
import           Game.LevelLocalsT
import qualified Game.PlayerClient as PlayerClient
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import qualified Server.SVWorld as SVWorld
import           Types
import qualified Util.Math3D as Math3D

import           Control.Lens (use, (^.), (.=), (+=), (&), (.~))
import           Control.Monad (when)
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import           Linear (V3(..))

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
                 mapM_ (fmap treatObject . readEdict) [0..numEdicts-1]
                 checkDMRules
                 checkNeedPass
                 clientEndServerFrames
        readEdict idx =
          do edict <- readRef (Ref idx)
             return (Ref idx, edict)

updateLevelGlobals :: Quake ()
updateLevelGlobals =
  do level <- use (gameBaseGlobals.gbLevel)
     gameBaseGlobals.gbLevel.llFrameNum += 1
     gameBaseGlobals.gbLevel.llTime .= (fromIntegral (level^.llFrameNum) + 1) * Constants.frameTime

treatObject :: (Ref EdictT, EdictT) -> Quake ()
treatObject (edictRef, edict)
  | not (edict^.eInUse) = return ()
  | otherwise =
      do gameBaseGlobals.gbLevel.llCurrentEntity .= Just edictRef
         modifyRef edictRef (\v -> v & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))
         checkGroundEntity edictRef edict (edict^.eGroundEntity)
         treatClientOrEntity edictRef =<< fmap (truncate . (^.cvValue)) maxClientsCVar

checkGroundEntity :: Ref EdictT -> EdictT -> Maybe (Ref EdictT) -> Quake ()
checkGroundEntity _ _ Nothing = return ()
checkGroundEntity edictRef edict (Just groundRef) =
  do ground <- readRef groundRef
     when ((ground^.eLinkCount) /= (edict^.eGroundEntityLinkCount)) $
       do modifyRef edictRef (\v -> v & eGroundEntity .~ Nothing)
          when ((edict^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 && (edict^.eFlags) .&. Constants.svfMonster /= 0) $
            M.checkGround edictRef

treatClientOrEntity :: Ref EdictT -> Int -> Quake ()
treatClientOrEntity edictRef@(Ref idx) maxClients
  | idx > 0 && idx <= maxClients = PlayerClient.clientBeginServerFrame edictRef
  | otherwise = runEntity edictRef

setMoveDir :: Ref EdictT -> EdictT -> Quake ()
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
exitLevel = error "GameBase.exitLevel" -- TODO

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
         shouldEnd <- or <$> mapM (\idx -> readEdict (Ref idx) >>= checkScore fragLimit) [1..maxClients]
         when shouldEnd $
           do bprintf Constants.printHigh "Fraglimit hit.\n"
              endDMLevel
  | otherwise = return ()
  where readEdict edictRef =
          do edict <- readRef edictRef
             return (edictRef, edict)

checkScore :: Int -> (Ref EdictT, EdictT) -> Quake Bool
checkScore fragLimit (Ref idx, edict)
  | edict^.eInUse =
      do client <- readRef (Ref (idx - 1))
         return ((client^.gcResp.crScore) >= fragLimit)
  | otherwise = return False

checkNeedPass :: Quake ()
checkNeedPass = error "GameBase.checkNeedPass" -- TODO

clientEndServerFrames :: Quake ()
clientEndServerFrames = error "GameBase.clientEndServerFrames" -- TODO

runEntity :: Ref EdictT -> Quake ()
runEntity = error "GameBase.runEntity" -- TODO

endDMLevel :: Quake ()
endDMLevel = error "GameBase.endDMLevel" -- TODO