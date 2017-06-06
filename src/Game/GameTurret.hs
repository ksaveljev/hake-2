module Game.GameTurret
    ( spTurretBase
    , spTurretBreach
    , spTurretDriver
    ) where

import           Control.Lens      (use, (%=), (^.), (&), (.~), (%~))
import           Control.Monad     (when, void)
import qualified Data.ByteString   as B
import           Data.Maybe        (fromMaybe)
import           Linear            (_x, _y)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase     as GameBase
import qualified Game.GameCombat   as GameCombat
import qualified Game.GameUtil     as GameUtil
import           Game.LevelLocalsT
import           Game.SpawnTempT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib          as Lib

spTurretBase :: Ref EdictT -> Quake ()
spTurretBase selfRef = do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                               & eMoveType .~ Constants.moveTypePush)
    gameImport <- use (gameBaseGlobals.gbGameImport)
    self <- readRef selfRef
    (gameImport^.giSetModel) selfRef (self^.eiModel)
    modifyRef selfRef (\v -> v & eBlocked .~ Just turretBlocked)
    (gameImport^.giLinkEntity) selfRef

turretBlocked :: EntBlocked
turretBlocked = EntBlocked "turret_blocked" $ \selfRef otherRef -> do
    other <- readRef otherRef
    when ((other^.eTakeDamage) /= 0) $ do
        self <- readRef selfRef
        teamMasterRef <- getTeamMasterRef (self^.eTeamMaster)
        teamMaster <- readRef teamMasterRef
        let attackerRef = fromMaybe teamMasterRef (teamMaster^.eOwner)
        v3o <- use (globals.gVec3Origin)
        GameCombat.damage otherRef selfRef attackerRef v3o (other^.eEntityState.esOrigin) v3o (teamMaster^.eDmg) 10 0 Constants.modCrush
  where
    getTeamMasterRef Nothing = do
        Com.fatalError "GameTurret.turretBlocked self^.eTeamMaster is Nothing"
        return (Ref (-1))
    getTeamMasterRef (Just teamMasterRef) = return teamMasterRef

spTurretBreach :: Ref EdictT -> Quake ()
spTurretBreach selfRef = do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                               & eMoveType .~ Constants.moveTypePush)
    gameImport <- use (gameBaseGlobals.gbGameImport)
    self <- readRef selfRef
    (gameImport^.giSetModel) selfRef (self^.eiModel)
    gameBaseGlobals.gbSpawnTemp %= (\v -> v & stMinPitch %~ (\vv -> if vv == 0 then (-30) else vv)
                                            & stMaxPitch %~ (\vv -> if vv == 0 then 30 else vv)
                                            & stMaxYaw %~ (\vv -> if vv == 0 then 360 else vv))
    spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eSpeed %~ (\a -> if a == 0 then 50 else a)
                               & eDmg %~ (\a -> if a == 0 then 10 else a)
                               & ePos1._x .~ (-1) * (spawnTemp^.stMinPitch)
                               & ePos1._y .~ (spawnTemp^.stMinYaw)
                               & ePos2._x .~ (-1) * (spawnTemp^.stMaxPitch)
                               & ePos2._y .~ (spawnTemp^.stMaxYaw)
                               & eIdealYaw .~ (self^.eEntityState.esAngles._y)
                               & eMoveAngles._y .~ (self^.eEntityState.esAngles._y)
                               & eBlocked .~ Just turretBlocked
                               & eThink .~ Just turretBreachFinishInit
                               & eNextThink .~ levelTime + Constants.frameTime)
    (gameImport^.giLinkEntity) selfRef

turretBreachFinishInit :: EntThink
turretBreachFinishInit = EntThink "turret_breach_finish_init" $ \selfRef -> do
    self <- readRef selfRef
    -- get and save info for muzzle location
    case self^.eTarget of
      Nothing -> do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat [self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), " needs a target\n"])
      Just _ -> do
        Just targetRef <- GameBase.pickTarget (self^.eTarget)
        modifyRef selfRef (\v -> v & eTargetEnt .~ Just targetRef)
        target <- readRef targetRef
        let moveOrigin = (target^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
        modifyRef selfRef (\v -> v & eMoveOrigin .~ moveOrigin)
        GameUtil.freeEdict targetRef
    let Just teamMasterRef = self^.eTeamMaster
    modifyRef teamMasterRef (\v -> v & eDmg .~ (self^.eDmg))
    modifyRef selfRef (\v -> v & eThink .~ Just turretBreachThink)
    void (entThink turretBreachThink selfRef)
    return True

turretBreachThink :: EntThink
turretBreachThink = error "GameTurret.turretBreachThink" -- TODO

spTurretDriver :: Ref EdictT -> Quake ()
spTurretDriver = error "GameTurret.spTurretDriver" -- TODO
