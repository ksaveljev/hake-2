{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBoss3 where

import Control.Lens (preuse, use, ix, (^.), (.=), (+=), zoom)
import Control.Monad (liftM, void)
import Linear (V3(..))

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MBoss32 as MBoss32

useBoss3 :: EntUse
useBoss3 =
  GenericEntUse "Use_Boss3" $ \edictRef@(EdictReference edictIdx) _ _ -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast

    writeByte Constants.svcTempEntity
    writeByte Constants.teBossTPort
    writePosition (edict^.eEntityState.esOrigin)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs
    GameUtil.freeEdict edictRef

thinkBoss3Stand :: EntThink
thinkBoss3Stand =
  GenericEntThink "Think_Boss3Stand" $ \(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if (edict^.eEntityState.esFrame) == MBoss32.frameStand260
      then gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MBoss32.frameStand201
      else gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame += 1

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= levelTime + Constants.frameTime
    return True

{-
- QUAKED monster_boss3_stand (1 .5 0) (-32 -32 0) (32 32 90)
- 
- Just stands and cycles in one place until targeted, then teleports away.
-}
spMonsterBoss3Stand :: EdictReference -> Quake ()
spMonsterBoss3Stand selfRef@(EdictReference selfIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict selfRef
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let modelIndex = gameImport^.giModelIndex
            soundIndex = gameImport^.giSoundIndex
            linkEntity = gameImport^.giLinkEntity

        Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
        modelIdx <- modelIndex (Just "models/monsters/boss3/rider/tris.md2")
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eMoveType                 .= Constants.moveTypeStep
          eSolid                    .= Constants.solidBbox
          eEdictInfo.eiModel        .= Just "models/monsters/boss3/rider/tris.md2"
          eEntityState.esModelIndex .= modelIdx
          eEntityState.esFrame      .= MBoss32.frameStand201
          eEdictMinMax.eMins        .= V3 (-32) (-32) 0
          eEdictMinMax.eMaxs        .= V3 32 32 90
          eEdictAction.eaUse        .= Just useBoss3
          eEdictAction.eaThink      .= Just thinkBoss3Stand
          eEdictAction.eaNextThink  .= levelTime + Constants.frameTime

        void $ soundIndex (Just "misc/bigtele.wav")

        linkEntity selfRef
