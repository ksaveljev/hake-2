{-# LANGUAGE TemplateHaskell #-}
module Client.ClientStateT where

import Data.Int (Int16)
import Linear.V3 (V3)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import Client.RefDefT
import Client.FrameT
import Game.UserCmdT
import Render.ImageT

data ClientStateT =
  ClientStateT { _csTimeOutCount           :: Int
               , _csTimeDemoFrames         :: Int
               , _csTimeDemoStart          :: Int
               , _csRefreshPrepped         :: Bool
               , _csSoundPrepped           :: Bool
               , _csForceRefDef            :: Bool
               , _csParseEntities          :: Int
               , _csCmd                    :: UserCmdT
               , _csCmds                   :: V.Vector UserCmdT
               , _csCmdTime                :: UV.Vector Int
               , _csPredictedOrigins       :: UV.Vector (V3 Int16)
               , _csPredictedStep          :: Float
               , _csPredictedStepTime      :: Int
               , _csPredictedOrigin        :: V3 Float
               , _csPredictedAngles        :: V3 Float
               , _csPredictionError        :: V3 Float
               , _csFrame                  :: FrameT
               , _csSurpressCount          :: Int
               , _csFrames                 :: V.Vector FrameT
               , _csViewAngles             :: V3 Float
               , _csTime                   :: Int
               , _csLerpFrac               :: Float
               , _csRefDef                 :: RefDefT
               , _csVForward               :: V3 Float
               , _csVRight                 :: V3 Float
               , _csVUp                    :: V3 Float
               , _csLayout                 :: B.ByteString
               , _csInventory              :: UV.Vector Int
               , _csCinematicFile          :: Quake () -- TODO: ???
               , _csCinematicTime          :: Int
               , _csCinematicFrame         :: Int
               , _csCinematicPalette       :: B.ByteString
               , _csCinematicPaletteActive :: Bool
               , _csAttractLoop            :: Bool
               , _csServerCount            :: Int
               , _csGameDir                :: B.ByteString
               , _csPlayerNum              :: Int
               , _csConfigStrings          :: V.Vector B.ByteString
               , _csModelDraw              :: V.Vector ModelT
               , _csModelClip              :: CModelT
               , _csSoundPrecache          :: V.Vector SfxT
               , _csImagePrecache          :: V.Vector ImageT
               , _csClientInfo             :: V.Vector ClientInfoT
               , _csBaseClientInfo         :: ClientInfoT
               }
