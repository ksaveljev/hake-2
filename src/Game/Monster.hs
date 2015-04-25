{-# LANGUAGE OverloadedStrings #-}
module Game.Monster where

import Control.Lens ((^.), preuse, (%=), ix, (+=), use, zoom, (.=))
import Control.Monad (liftM, when)
import Data.Bits ((.&.), (.|.), complement)
import Data.Maybe (isNothing, isJust)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

monsterStart :: EdictReference -> Quake Bool
monsterStart edictRef@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then do
        GameUtil.freeEdict edictRef
        return False
      else do
        updateSpawnFlags
        updateMonsterCount
        updateSelf
        setItem
        randomizeStartFrame
        return True

  where updateSpawnFlags :: Quake ()
        updateSpawnFlags = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          when ((edict^.eSpawnFlags) .&. 4 /= 0 && (edict^.eMonsterInfo.miAIFlags) .&. Constants.aiGoodGuy == 0) $ do
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.&. (complement 4))
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. 1)

        updateMonsterCount :: Quake ()
        updateMonsterCount = do
          Just aiFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miAIFlags
          when (aiFlags .&. Constants.aiGoodGuy == 0) $
            gameBaseGlobals.gbLevel.llTotalMonsters += 1

        updateSelf :: Quake ()
        updateSelf = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          time <- use $ gameBaseGlobals.gbLevel.llTime

          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eEdictAction.eaNextThink .= time + Constants.frameTime
            eSvFlags %= (.|. Constants.svfMonster)
            eEntityState.esRenderFx %= (.|. Constants.rfFrameLerp)
            eEdictStatus.eTakeDamage .= Constants.damageAim
            eEdictPhysics.eAirFinished .= time + 12
            eEdictAction.eaUse .= Just (GameUtil.monsterUse)
            eEdictStatus.eMaxHealth .= (edict^.eEdictStatus.eHealth)
            eClipMask .= Constants.maskMonsterSolid

            eEntityState.esSkinNum .= 0
            eEdictStatus.eDeadFlag .= Constants.deadNo
            eSvFlags %= (.&. (complement Constants.svfDeadMonster))

            eEntityState.esOldOrigin .= (edict^.eEntityState.esOrigin)

          when (isNothing (edict^.eMonsterInfo.miCheckAttack)) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCheckAttack .= Just GameUtil.mCheckAttack

        setItem :: Quake ()
        setItem = do
          item <- use $ gameBaseGlobals.gbSpawnTemp.stItem

          when (B.length item > 0) $ do
            foundItem <- GameItems.findItemByClassname item
            gameBaseGlobals.gbGEdicts.ix edictIdx.eItem .= foundItem

            when (isNothing foundItem) $ do
              Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
              dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
              dprintf $ "monster_start:" `B.append` (edict^.eClassName) `B.append`
                        " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append`
                        " has bad item: " `B.append` item `B.append` "\n"

        randomizeStartFrame :: Quake ()
        randomizeStartFrame = do
          Just currentMove <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMonsterInfo.miCurrentMove
          when (isJust currentMove) $ do
            r <- Lib.rand
            let Just move = currentMove
                frame = (move^.mmFirstFrame) + ((fromIntegral r) `div` ((move^.mmLastFrame) - (move^.mmFirstFrame) + 1))
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= frame