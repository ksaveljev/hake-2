{-# LANGUAGE TemplateHaskell #-}
module Game.GameImportT ( GameImportT(..)
                        , module Game.GameImportT
                        ) where

import Control.Lens (makeLenses)

import Internal
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.PMove as PMove
import qualified Server.SVGame as SVGame
import qualified Server.SVInit as SVInit
import qualified Server.SVSend as SVSend
import qualified Server.SVWorld as SVWorld

makeLenses ''GameImportT

newGameImportT :: GameImportT
newGameImportT =
  GameImportT { _giBprintf            = SVSend.broadcastPrintf
              , _giDprintf            = SVGame.dprintf
              , _giCprintf            = SVGame.cprintf
              , _giCenterPrintf       = SVGame.centerPrintf
              , _giSound              = SVGame.startSound
              , _giPositionedSound    = SVSend.startSound
              , _giConfigString       = SVGame.configString
              , _giError              = Com.comError Constants.errFatal
              , _giError2             = SVGame.pfError2
              , _giModelIndex         = SVInit.modelIndex
              , _giSoundIndex         = SVInit.soundIndex
              , _giImageIndex         = SVInit.imageIndex
              , _giSetModel           = SVGame.setModel
              , _giTrace              = SVWorld.trace
              --, pmove_t.PointContentsAdapter -- TODO: ???
              , _giInPHS              = SVGame.inPHS
              , _giSetAreaPortalState = CM.setAreaPortalState
              , _giAreasConnected     = CM.areasConnected
              , _giLinkEntity         = SVWorld.linkEdict
              , _giUnlinkEntity       = SVWorld.unlinkEdict
              , _giBoxEdicts          = SVWorld.areaEdicts
              , _giPMove              = PMove.pMove
              , _giMulticast          = SVSend.multicast
              , _giUnicast            = SVGame.unicast
              , _giWriteByte          = SVGame.writeByte
              , _giWriteShort         = SVGame.writeShort
              , _giWriteString        = SVGame.writeString
              , _giWritePosition      = SVGame.writePos
              , _giWriteDir           = SVGame.writeDir
              , _giCVar               = CVar.get
              , _giCVarSet            = CVar.set
              , _giCVarForceSet       = CVar.forceSet
              , _giArgc               = Cmd.argc
              , _giArgv               = Cmd.argv
              , _giArgs               = Cmd.args
              , _giAddCommandString   = CBuf.addText
              }
