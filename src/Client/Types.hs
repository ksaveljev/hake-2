module Client.Types where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import Game.Types

data CEntity = CEntity { cEntityBaseline    :: EntityState
                       , cEntityCurrent     :: EntityState
                       , cEntityPrev        :: EntityState
                       , cEntityServerFrame :: Int
                       , cEntityTrailCount  :: Int
                       , cEntityLerpOrigin  :: V3 Float
                       , cEntityFlyStopTime :: Int
                       }

data CLSustain = CLSustain { clSustainId            :: Int
                           , clSustainType          :: Int
                           , clSustainEndTime       :: Int
                           , clSustainNextThink     :: Int
                           , clSustainThinkInterval :: Int
                           , clSustainOrg           :: V3 Float
                           , clSustainDir           :: V3 Float
                           , clSustainColor         :: Int
                           , clSustainCount         :: Int
                           , clSustainMagnitude     :: Int
                           , clSustainThink         :: IO () -- TODO: ???
                           }

data ClientState = ClientState { clientStateTimeOutCount           :: Int
                               , clientStateTimeDemoFrames         :: Int
                               , clientStateTimeDemoStart          :: Int
                               , clientStateRefreshPrepped         :: Bool
                               , clientStateSoundPrepped           :: Bool
                               , clientStateForceRefDef            :: Bool
                               , clientStateParseEntities          :: Int
                               , clientStateCmd                    :: UserCmd
                               , clientStateCmds                   :: UV.Vector UserCmd
                               , clientStateCmdTime                :: UV.Vector Int
                               , clientStatePredictedOrigins       :: UV.Vector (V3 Int16)
                               , clientStatePredictedStep          :: Float
                               , clientStatePredictedStepTime      :: Int
                               , clientStatePredictedOrigin        :: V3 Float
                               , clientStatePredictedAngles        :: V3 Float
                               , clientStatePredictionError        :: V3 Float
                               , clientStateFrame                  :: Frame
                               , clientStateSurpressCount          :: Int
                               , clientStateFrames                 :: UV.Vector Frame
                               , clientStateViewAngles             :: V3 Float
                               , clientStateTime                   :: Int
                               , clientStateLerpFrac               :: Float
                               , clientStateRefDef                 :: RefDef
                               , clientStateVForward               :: V3 Float
                               , clientStateVRight                 :: V3 Float
                               , clientStateVUp                    :: V3 Float
                               , clientStateLayout                 :: B.ByteString
                               , clientStateInventory              :: UV.Vector Int
                               , clientStateCinematicFile          :: IO () -- TODO: ???
                               , clientStateCinematicTime          :: Int
                               , clientStateCinematicFrame         :: Int
                               , clientStateCinematicPalette       :: UV.Vector Int8
                               , clientStateCinematicPaletteActive :: Bool
                               , clientStateAttractLoop            :: Bool
                               , clientStateServerCount            :: Int
                               , clientStateGameDir                :: B.ByteString
                               , clientStatePlayerNum              :: Int
                               , clientStateConfigStrings          :: UV.Vector B.ByteString
                               , clientStateModelDraw              :: UV.Vector Model
                               , clientStateModelClip              :: CModel
                               , clientStateSoundPrecache          :: UV.Vector SFX
                               , clientStateImagePrecache          :: UV.Vector Image
                               , clientStateClientInfo             :: UV.Vector ClientInfo
                               , clientStateBaseClientInfo         :: ClientInfo
                               }

data ClientStatic = ClientStatic { clientStaticState              :: Int
                                 , clientStaticKeyDest            :: Int
                                 , clientStaticFrameCount         :: Int
                                 , clientStaticRealTime           :: Int
                                 , clientStaticFrameTime          :: Float
                                 , clientStaticDisableScreen      :: Float
                                 , clientStaticDisableServerCount :: Int
                                 , clientStaticServerName         :: B.ByteString
                                 , clientStaticConnectTime        :: Float
                                 , clientStaticQuakePort          :: Int
                                 , clientStaticNetChan            :: NetChan
                                 , clientStaticServerProtocol     :: Int
                                 , clientStaticChallenge          :: Int
                                 , clientStaticDownload           :: FilePath -- TODO: ???
                                 , clientStaticDownloadTempName   :: B.ByteString
                                 , clientStaticDownloadName       :: B.ByteString
                                 , clientStaticDownloadNumber     :: Int
                                 , clientStaticDownloadType       :: Int
                                 , clientStaticDownloadPercent    :: Int
                                 , clientStaticDemoRecording      :: Bool
                                 , clientStaticDemoWaiting        :: Bool
                                 , clientStaticDemoFile           :: FilePath -- TODO: ???
                                 }
