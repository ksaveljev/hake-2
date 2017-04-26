module Game.Monster
    ( monsterFireBlaster
    , monsterFireBullet
    , monsterFireRocket
    , monsterFireShotgun
    , monsterStart
    , monsterStartGo
    , monsterTriggeredStart
    ) where

import           Control.Lens          (use, (^.), (+=), (&), (.~), (%~))
import           Control.Monad         (when, unless, void)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import           Data.Maybe            (isJust, isNothing)
import           Linear                (V3(..), _y)

import qualified Client.M              as M
import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameItems        as GameItems
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MMoveT
import           Game.MonsterInfoT
import           Game.SpawnTempT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

import {-# SOURCE #-} qualified Game.GameBase as GameBase

monsterStart :: Ref EdictT -> Quake Bool
monsterStart edictRef = do
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    doMonsterStart deathmatch
  where
    doMonsterStart deathmatch
        | deathmatch /= 0 = do
            GameUtil.freeEdict edictRef
            return False
        | otherwise = do
            updateSpawnFlags
            updateMonsterCount
            updateSelf
            setItem
            randomizeStartFrame
            return True
    updateSpawnFlags = do
        edict <- readRef edictRef
        when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
            modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. (complement 4))
                                        & eSpawnFlags %~ (.|. 1))
    updateMonsterCount = do
        edict <- readRef edictRef
        when ((edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $
            gameBaseGlobals.gbLevel.llTotalMonsters += 1
    updateSelf = do
        edict <- readRef edictRef
        time <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef edictRef (\v -> v & eNextThink .~ time + Constants.frameTime
                                    & eSvFlags %~ (.|. Constants.svfMonster)
                                    & eEntityState.esRenderFx %~ (.|. Constants.rfFrameLerp)
                                    & eTakeDamage .~ Constants.damageAim
                                    & eAirFinished .~ time + 12
                                    & eUse .~ Just (GameUtil.monsterUse)
                                    & eMaxHealth .~ (edict^.eHealth)
                                    & eClipMask .~ Constants.maskMonsterSolid
                                    & eEntityState.esSkinNum .~ 0
                                    & eDeadFlag .~ Constants.deadNo
                                    & eSvFlags %~ (.&. (complement Constants.svfDeadMonster))
                                    & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))
        when (isNothing (edict^.eMonsterInfo.miCheckAttack)) $
            modifyRef edictRef (\v -> v & eMonsterInfo.miCheckAttack .~ Just GameUtil.mCheckAttack)
    setItem = do
        mItem <- use (gameBaseGlobals.gbSpawnTemp.stItem)
        maybe (return ()) doSetItem mItem
    doSetItem item = do
        when (B.length item > 0) $ do
            foundItem <- GameItems.findItemByClassname item
            modifyRef edictRef (\v -> v & eItem .~ foundItem)
            when (isNothing foundItem) $ do
                edict <- readRef edictRef
                dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
                dprintf $ "monster_start:" `B.append` (edict^.eClassName) `B.append`
                          " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append`
                          " has bad item: " `B.append` item `B.append` "\n" -- IMPROVE
    randomizeStartFrame = do
        edict <- readRef edictRef
        maybe (return ()) doRandomizeStartFrame (edict^.eMonsterInfo.miCurrentMove)
    doRandomizeStartFrame move = do
        r <- Lib.rand
        let frame = (move^.mmFirstFrame) + ((fromIntegral r) `div` ((move^.mmLastFrame) - (move^.mmFirstFrame) + 1))
        modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)

monsterFireBlaster :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBlaster = error "Monster.monsterFireBlaster" -- TODO

monsterFireShotgun :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireShotgun = error "Monster.monsterFireShotgun" -- TODO

monsterFireBullet :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Int -> Int -> Quake ()
monsterFireBullet = error "Monster.monsterFireBullet" -- TODO

monsterStartGo :: Ref EdictT -> Quake ()
monsterStartGo selfRef = do
    self <- readRef selfRef
    unless ((self^.eHealth) <= 0) $ do
        maybe (return ()) checkTarget (self^.eTarget)
        combatTarget <- fmap (^.eCombatTarget) (readRef selfRef)
        maybe (return ()) (validateCombatTarget Nothing) combatTarget
        target <- fmap (^.eTarget) (readRef selfRef)
        pickAction target
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eThink .~ Just monsterThink
                                   & eNextThink .~ levelTime + Constants.frameTime)
  where
    checkTarget targetName = do
        (notCombat, fixup) <- checkTargets targetName False False Nothing
        self <- readRef selfRef
        when (notCombat && isJust (self^.eCombatTarget)) $ do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat [self^.eClassName, " at ", Lib.vtos (self^.eEntityState.esOrigin), " has target with mixed types\n"])
        when fixup $
            modifyRef selfRef (\v -> v & eTarget .~ Nothing)
    checkTargets targetName notCombat fixup targetRef = do
        foundRef <- GameBase.gFind targetRef GameBase.findByTarget targetName
        maybe (return (notCombat, fixup)) (proceedCheckTargets targetName notCombat fixup) foundRef
    proceedCheckTargets targetName notCombat fixup targetRef = do
        target <- readRef targetRef
        if (target^.eClassName) == "point_combat"
            then do
                modifyRef selfRef (\v -> v & eCombatTarget .~ Just targetName)
                checkTargets targetName notCombat True (Just targetRef)
            else
                checkTargets targetName True fixup (Just targetRef)
    validateCombatTarget targetRef combatTarget = do
        foundRef <- GameBase.gFind targetRef GameBase.findByTarget combatTarget
        maybe (return ()) (proceedValidation combatTarget) foundRef
    proceedValidation combatTarget foundRef = do
        foundEdict <- readRef foundRef
        when ((foundEdict^.eClassName) /= "point_combat") $ do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf "IMPLEMENT ME! bad combattarget" -- TODO
        validateCombatTarget (Just foundRef) combatTarget
    pickAction (Just targetName) = do
        pickedTargetRef <- GameBase.pickTarget (Just targetName)
        modifyRef selfRef (\v -> v & eGoalEntity .~ pickedTargetRef
                                   & eMoveTarget .~ pickedTargetRef)
        maybe cantFindTarget (\targetRef -> targetFound =<< readRef targetRef) pickedTargetRef
    pickAction Nothing = do
        modifyRef selfRef (\v -> v & eMonsterInfo.miPauseTime .~ 100000000)
        standF <- fmap (^.eMonsterInfo.miStand) (readRef selfRef)
        monsterAction standF
    cantFindTarget = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf "IMPLEMENT ME! can't find target" -- TODO
        modifyRef selfRef (\v -> v & eTarget .~ Nothing
                                   & eMonsterInfo.miPauseTime .~ 100000000)
        standF <- fmap (^.eMonsterInfo.miStand) (readRef selfRef)
        monsterAction standF
    targetFound target
        | (target^.eClassName) == "path_corner" = do
            self <- readRef selfRef
            let vec = (target^.eEntityState.esOrigin) - (self^.eEntityState.esOrigin)
                yaw = Math3D.vectorYaw vec
            modifyRef selfRef (\v -> v & eEntityState.esAngles._y .~ yaw -- IMPROVE: use Constants.yaw instead of directly _y
                                       & eIdealYaw .~ yaw)
            monsterAction (self^.eMonsterInfo.miWalk)
            modifyRef selfRef (\v -> v & eTarget .~ Nothing)
        | otherwise = do
            modifyRef selfRef (\v -> v & eGoalEntity .~ Nothing
                                       & eMoveTarget .~ Nothing
                                       & eMonsterInfo.miPauseTime .~ 100000000)
            standF <- fmap (^.eMonsterInfo.miStand) (readRef selfRef)
            monsterAction standF
    monsterAction Nothing = Com.fatalError "Monster.monsterStartGo#monsterAction actionF is Nothing"
    monsterAction (Just actionF) = void (entThink actionF selfRef)

monsterTriggeredStart :: EntThink
monsterTriggeredStart = EntThink "monster_triggered_start" $ \selfRef -> do
    self <- readRef selfRef
    when ((self^.eIndex) == 312) $
        Com.printf "monster_triggered_start\n"
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidNot
                               & eMoveType .~ Constants.moveTypeNone
                               & eSvFlags %~ (.|. Constants.svfNoClient)
                               & eNextThink .~ 0
                               & eUse .~ Just monsterTriggeredSpawnUse)
    return True

monsterTriggeredSpawnUse :: EntUse
monsterTriggeredSpawnUse = error "Monster.monsterTriggeredSpawnUse" -- TODO

monsterFireRocket :: Ref EdictT -> V3 Float -> V3 Float -> Int -> Int -> Int -> Quake ()
monsterFireRocket = error "Monster.monsterFireRocket" -- TODO

monsterThink :: EntThink
monsterThink = EntThink "monster_think" $ \selfRef -> do
    M.moveFrame selfRef
    self <- readRef selfRef
    when ((self^.eLinkCount) /= (self^.eMonsterInfo.miLinkCount)) $ do
        modifyRef selfRef (\v -> v & eMonsterInfo.miLinkCount .~ (self^.eLinkCount))
        M.checkGround selfRef
    M.catagorizePosition selfRef
    M.worldEffects selfRef
    M.setEffects selfRef
    return True
