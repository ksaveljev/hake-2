{-# LANGUAGE OverloadedStrings #-}
module Game.GameTurret ( spTurretBreach
                       , spTurretBase
                       , spTurretDriver
                       ) where

import Control.Lens (use, (^.), (&), (.~), (%~), (%=), zoom, (+=), (+~))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (fromJust)
import Linear (V3(..), _x, _y, _z, norm)
import qualified Data.ByteString as B

import Game.EntityStateT
import Game.EdictT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.GameCombat as GameCombat
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MInfantry as MInfantry
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

spTurretBreach :: Ref EdictT -> Quake ()
spTurretBreach selfRef = do
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                  & eMoveType .~ Constants.moveTypePush
                                  )
    
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity
    
    self <- readRef selfRef
    
    setModel selfRef (self^.eiModel)
    
    zoom (gameBaseGlobals.gbSpawnTemp) $ do
      stMinPitch %= (\v -> if v == 0 then (-30) else v)
      stMaxPitch %= (\v -> if v == 0 then 30 else v)
      stMaxYaw %= (\v -> if v == 0 then 360 else v)
    
    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    modifyRef selfRef (\v -> v & eSpeed %~ (\a -> if a == 0 then 50 else a)
                                  & eDmg %~ (\a -> if a == 0 then 10 else a)
                                  & ePos1._x .~ (-1) * (spawnTemp^.stMinPitch) -- IMPROVE: use Constants.pitch instead of using _x directly
                                  & ePos1._y .~ (spawnTemp^.stMinYaw) -- IMPROVE: use Constants.yaw instead of using _y directly
                                  & ePos2._x .~ (-1) * (spawnTemp^.stMaxPitch) -- IMPROVE: use Constants.pitch instead of using _x directly
                                  & ePos2._y .~ (spawnTemp^.stMaxYaw) -- IMPROVE: use Constants.yaw instead of using _y directly
                                  & eIdealYaw .~ (self^.eEntityState.esAngles._y) -- IMPROVE: use Constants.yaw instead of using _y directly
                                  & eMoveAngles._y .~ (self^.eEntityState.esAngles._y) -- IMPROVE: use Constants.yaw instead of using _y directly
                                  & eBlocked .~ Just turretBlocked
                                  & eThink .~ Just turretBreachFinishInit
                                  & eNextThink .~ levelTime + Constants.frameTime
                                  )
    
    linkEntity selfRef

{-
- QUAKED turret_base (0 0 0) ? This portion of the turret changes yaw only.
- MUST be teamed with a turret_breach.
-}
spTurretBase :: Ref EdictT -> Quake ()
spTurretBase selfRef = do
  modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                & eMoveType .~ Constants.moveTypePush
                                )
  
  gameImport <- use $ gameBaseGlobals.gbGameImport
  let setModel = gameImport^.giSetModel
      linkEntity = gameImport^.giLinkEntity
  
  self <- readRef selfRef
      
  setModel selfRef (self^.eiModel)
  
  modifyRef selfRef (\v -> v & eBlocked .~ Just turretBlocked)
  linkEntity selfRef

spTurretDriver :: Ref EdictT -> Quake ()
spTurretDriver selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    
    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef
      
      else do
        self <- readRef selfRef
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        gameImport <- use $ gameBaseGlobals.gbGameImport
        spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp
        
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity
            dprintf = gameImport^.giDprintf
        
        modelIdx <- modelIndex (Just "models/monsters/infantry/tris.md2")
      
        modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                      & eSolid .~ Constants.solidBbox
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eMins .~ V3 (-16) (-16) (-24)
                                      & eMaxs .~ V3 16 16 32
                                      & eHealth .~ 100
                                      & eGibHealth .~ 0
                                      & eMass .~ 200
                                      & eViewHeight .~ 24
                                      & eDie .~ Just turretDriverDie
                                      & eMonsterInfo.miStand .~ Just MInfantry.infantryStand
                                      & eFlags %~ (\a -> a .|. Constants.flNoKnockback)
                                      & eSvFlags %~ (\a -> a .|. Constants.svfMonster)
                                      & eEntityState.esRenderFx %~ (\a -> a .|. Constants.rfFrameLerp)
                                      & eTakeDamage .~ Constants.damageAim
                                      & eUse .~ Just GameUtil.monsterUse
                                      & eClipMask .~ Constants.maskMonsterSolid
                                      & eEntityState.esOldOrigin .~ (self^.eEntityState.esOrigin)
                                      & eMonsterInfo.miAIFlags %~ (\a -> a .|. Constants.aiStandGround .|. Constants.aiDucked)
                                      & eThink .~ Just turretDriverLink
                                      & eNextThink .~ levelTime + Constants.frameTime
                                      )
        
        gameBaseGlobals.gbLevel.llTotalMonsters += 1
        
        
        case spawnTemp^.stItem of
          Nothing ->
            return ()
          
          Just itemClassname -> do
            item <- GameItems.findItemByClassname itemClassname
            modifyRef selfRef (\v -> v & eItem .~ item)
            
            case item of
              Just _ -> return ()
              Nothing -> dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` " has bad item: " `B.append` itemClassname `B.append` "\n")
        
        linkEntity selfRef

turretBlocked :: EntBlocked
turretBlocked =
  GenericEntBlocked "turret_blocked" $ \selfRef otherRef -> do
    other <- readRef otherRef
    
    when ((other^.eTakeDamage) /= 0) $ do
      self <- readRef selfRef
      let Just teamMasterRef = self^.eTeamMaster
      teamMaster <- readRef teamMasterRef
      
      let attackerRef = case teamMaster^.eOwner of
                          Nothing -> teamMasterRef
                          Just ownerRef -> ownerRef
      
      v3o <- use $ globals.gVec3Origin
      
      GameCombat.damage otherRef selfRef attackerRef v3o (other^.eEntityState.esOrigin) v3o (teamMaster^.eDmg) 10 0 Constants.modCrush

turretBreachFinishInit :: EntThink
turretBreachFinishInit =
  GenericEntThink "turret_breach_finish_init" $ \selfRef -> do
    self <- readRef selfRef
    
    -- get and save info for muzzle location
    case self^.eTarget of
      Nothing -> do
        dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
        dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` " needs a target\n")
      
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
    void $ think turretBreachThink selfRef
    return True

{-
- QUAKED turret_driver (1 .5 0) (-16 -16 -24) (16 16 32) Must NOT be on the
- team with the rest of the turret parts. Instead it must target the
- turret_breach.
-}
turretDriverDie :: EntDie
turretDriverDie =
  GenericEntDie "turret_driver_die" $ \selfRef inflictorRef attackerRef damage _ -> do
    self <- readRef selfRef
    
    let Just targetEntRef = self^.eTargetEnt
    
    -- level the gun
    modifyRef targetEntRef (\v -> v & eMoveAngles._x .~ 0)
    
    -- remove the driver from the end of them team chain
    targetEnt <- readRef targetEntRef
    removeDriverFromTeamChain selfRef (targetEnt^.eTeamMaster)
    
    modifyRef selfRef (\v -> v & eTeamMaster .~ Nothing
                                  & eFlags %~ (\a -> a .&. complement (Constants.flTeamSlave))
                                  )
    
    modifyRef targetEntRef (\v -> v & eOwner .~ Nothing)
    modifyRef (fromJust $ targetEnt^.eTeamMaster) (\v -> v & eOwner .~ Nothing)
    
    die MInfantry.infantryDie selfRef inflictorRef attackerRef damage (V3 0 0 0) -- infantryDie doesn't use last argument
            
  where removeDriverFromTeamChain :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
        removeDriverFromTeamChain _ Nothing = return () -- RESEARCH: shouldn't ever reach here, maybe throw an error?
        removeDriverFromTeamChain selfRef (Just edictRef) = do
          edict <- readRef edictRef
          
          if (edict^.eTeamChain) == Just selfRef
            then modifyRef edictRef (\v -> v & eTeamChain .~ Nothing)
            else removeDriverFromTeamChain selfRef (edict^.eTeamChain)

turretDriverLink :: EntThink
turretDriverLink =
  GenericEntThink "turret_driver_link" $ \selfRef -> do
    self <- readRef selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    Just targetEntRef <- GameBase.pickTarget (self^.eTarget)
    targetEnt <- readRef targetEntRef
    
    let Just teamMasterRef = targetEnt^.eTeamMaster
    
    modifyRef targetEntRef (\v -> v & eOwner .~ Just selfRef)
    modifyRef teamMasterRef (\v -> v & eOwner .~ Just selfRef)
    
    let vec = ((targetEnt^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)) & _z .~ 0
        vec' = (self^.eEntityState.esOrigin) - (targetEnt^.eEntityState.esOrigin)
        vec'' = Math3D.vectorAngles vec'
        vec''' = anglesNormalize vec''
  
    modifyRef selfRef (\v -> v & eThink .~ Just turretDriverThink
                                  & eNextThink .~ levelTime + Constants.frameTime
                                  & eTargetEnt .~ Just targetEntRef
                                  & eEntityState.esAngles .~ (targetEnt^.eEntityState.esAngles)
                                  & eMoveOrigin._x .~ norm vec
                                  & eMoveOrigin._y .~ (vec'''^._y)
                                  & eMoveOrigin._z .~ (self^.eEntityState.esOrigin._z) - (targetEnt^.eEntityState.esOrigin._z)
                                  )
    
    addDriverToTeamChain selfRef (targetEnt^.eTeamMaster)
    
    modifyRef selfRef (\v -> v & eTeamMaster .~ (targetEnt^.eTeamMaster)
                                  & eFlags %~ (\a -> a .|. Constants.flTeamSlave)
                                  )
    
    return True
  
  where addDriverToTeamChain :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
        addDriverToTeamChain _ Nothing = return () -- RESEARCH: shouldn't ever reach here, maybe throw an error?
        addDriverToTeamChain selfRef (Just edictRef) = do
          edict <- readRef edictRef
          
          case edict^.eTeamChain of
            Nothing -> modifyRef edictRef (\v -> v & eTeamChain .~ Just selfRef)
            Just _ -> addDriverToTeamChain selfRef (edict^.eTeamChain)

turretBreachThink :: EntThink
turretBreachThink =
  GenericEntThink "turret_breach_think" $ \_ -> do
    io (putStrLn "GameTurret.turretBreachThink") >> undefined -- TODO

anglesNormalize :: V3 Float -> V3 Float
anglesNormalize (V3 a b c) = V3 a'' b'' c

  where a' = decrease a
        a'' = increase a'
        b' = decrease b
        b'' = increase b'
        
        decrease :: Float -> Float
        decrease x = if x > 360 then decrease (x - 360) else x
        
        increase :: Float -> Float
        increase x = if x < 0 then increase (x + 360) else x

turretDriverThink :: EntThink
turretDriverThink =
  GenericEntThink "turret_driver_think" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)
    
    updateSelfEnemy selfRef
    
    self <- readRef selfRef
    
    done <- case self^.eEnemy of
              Nothing -> do
                foundTarget <- GameUtil.findTarget selfRef
                if not foundTarget
                  then 
                    return True
                  else do
                    modifyRef selfRef (\v -> v & eMonsterInfo.miTrailTime .~ levelTime
                                                  & eMonsterInfo.miAIFlags %~ (\a -> a .&. (complement Constants.aiLostSight))
                                                  )
                    return False
              
              Just enemyRef -> do
                visible <- GameUtil.visible selfRef enemyRef
                
                if visible
                  then do
                    when ((self^.eMonsterInfo.miAIFlags) .&. Constants.aiLostSight /= 0) $
                      modifyRef selfRef (\v -> v & eMonsterInfo.miTrailTime .~ levelTime
                                                    & eMonsterInfo.miAIFlags %~ (\a -> a .&. (complement Constants.aiLostSight))
                                                    )
                    return False
                  
                  else do
                    modifyRef selfRef (\v -> v & eMonsterInfo.miAIFlags %~ (\a -> a .|. Constants.aiLostSight))
                    return True
        
    if done
      then
        return True
      
      else do
        self' <- readRef selfRef
        -- let the turret know where we want it to aim
        let Just enemyRef = self'^.eEnemy
        enemy <- readRef enemyRef
        let Just targetEntRef = self'^.eTargetEnt
        targetEnt <- readRef targetEntRef
        
        let target = (enemy^.eEntityState.esOrigin) & _z +~ fromIntegral (enemy^.eViewHeight)
            dir = target - (targetEnt^.eEntityState.esOrigin)
            moveAngles = Math3D.vectorAngles dir
        
        modifyRef targetEntRef (\v -> v & eMoveAngles .~ moveAngles)
        
        -- decide if we should shoot
        if levelTime < (self'^.eMonsterInfo.miAttackFinished)
          then
            return True
            
          else do
            skillValue <- liftM (^.cvValue) skillCVar
            let reactionTime = (3 - skillValue) * 1.0
            
            if levelTime - (self'^.eMonsterInfo.miTrailTime) < reactionTime
              then
                return True
              
              else do
                modifyRef selfRef (\v -> v & eMonsterInfo.miAttackFinished .~ levelTime + reactionTime + 1.0)
                -- FIXME how do we really want to pass this along?
                modifyRef targetEntRef (\v -> v & eSpawnFlags %~ (\a -> a .|. 65536))
                return True
                
  where updateSelfEnemy :: Ref EdictT -> Quake ()
        updateSelfEnemy selfRef = do
          self <- readRef selfRef
          case self^.eEnemy of
            Nothing -> return ()
            Just enemyRef -> do
              enemy <- readRef enemyRef
              when (not (enemy^.eInUse) || (enemy^.eHealth) <= 0) $
                modifyRef selfRef (\v -> v & eEnemy .~ Nothing)
