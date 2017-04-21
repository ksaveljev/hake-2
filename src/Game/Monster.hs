module Game.Monster
    ( monsterFireBlaster
    , monsterFireBullet
    , monsterFireShotgun
    , monsterStart
    , monsterStartGo
    , monsterTriggeredStart
    ) where

import           Control.Lens          (use, (^.), (+=), (&), (.~), (%~))
import           Control.Monad         (when)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import           Data.Maybe            (isNothing)
import           Linear                (V3(..))

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
monsterStartGo = error "Monster.monsterSTartGo" -- TODO

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
