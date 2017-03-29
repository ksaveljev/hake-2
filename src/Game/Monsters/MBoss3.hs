{-# LANGUAGE OverloadedStrings #-}
module Game.Monsters.MBoss3 where

import Control.Lens (preuse, use, ix, (^.), (.=), (+=), zoom, (&), (.~), (%~), (+~))
import Control.Monad (liftM, void)
import Linear (V3(..))

import Types
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MBoss32 as MBoss32

useBoss3 :: EntUse
useBoss3 =
  GenericEntUse "Use_Boss3" $ \edictRef _ _ -> do
    edict <- readEdictT edictRef
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
  GenericEntThink "Think_Boss3Stand" $ \edictRef -> do
    edict <- readEdictT edictRef

    if (edict^.eEntityState.esFrame) == MBoss32.frameStand260
      then modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ MBoss32.frameStand201)
      else modifyEdictT edictRef (\v -> v & eEntityState.esFrame +~ 1)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)
    return True

{-
- QUAKED monster_boss3_stand (1 .5 0) (-32 -32 0) (32 32 90)
- 
- Just stands and cycles in one place until targeted, then teleports away.
-}
spMonsterBoss3Stand :: EdictReference -> Quake ()
spMonsterBoss3Stand selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let modelIndex = gameImport^.giModelIndex
            soundIndex = gameImport^.giSoundIndex
            linkEntity = gameImport^.giLinkEntity

        self <- readEdictT selfRef
        modelIdx <- modelIndex (Just "models/monsters/boss3/rider/tris.md2")
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eMoveType                 .~ Constants.moveTypeStep
                                      & eSolid                    .~ Constants.solidBbox
                                      & eiModel                   .~ Just "models/monsters/boss3/rider/tris.md2"
                                      & eEntityState.esModelIndex .~ modelIdx
                                      & eEntityState.esFrame      .~ MBoss32.frameStand201
                                      & eMins                     .~ V3 (-32) (-32) 0
                                      & eMaxs                     .~ V3 32 32 90
                                      & eUse                      .~ Just useBoss3
                                      & eThink                    .~ Just thinkBoss3Stand
                                      & eNextThink                .~ levelTime + Constants.frameTime)

        void $ soundIndex (Just "misc/bigtele.wav")

        linkEntity selfRef
