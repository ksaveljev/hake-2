module Game.GameImportT ( GameImportT(..)
                        , newGameImportT
                        , giBprintf
                        , giDprintf
                        , giCprintf
                        , giCenterPrintf
                        , giSound
                        , giPositionedSound
                        , giConfigString
                        , giError
                        , giError2
                        , giModelIndex
                        , giSoundIndex
                        , giImageIndex
                        , giSetModel
                        , giTrace
                        --, pmove_t.PointContentsAdapter -- TODO: ???
                        , giInPHS
                        , giSetAreaPortalState
                        , giAreasConnected
                        , giLinkEntity
                        , giUnlinkEntity
                        , giBoxEdicts
                        , giPMove
                        , giMulticast
                        , giUnicast
                        , giWriteByte
                        , giWriteShort
                        , giWriteString
                        , giWritePosition
                        , giWriteDir
                        , giCVar
                        , giCVarSet
                        , giCVarForceSet
                        , giArgc
                        , giArgv
                        , giArgs
                        , giAddCommandString
                        ) where

import Linear (V3)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Internal
import Game.CVarT

newGameImportT       :: GameImportT
giBprintf            :: Functor f => ((Int -> B.ByteString -> Quake ()) -> f (Int -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giDprintf            :: Functor f => ((B.ByteString -> Quake ()) -> f (B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giCprintf            :: Functor f => ((EdictT -> Int -> B.ByteString -> Quake ()) -> f (EdictT -> Int -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giCenterPrintf       :: Functor f => ((EdictT -> B.ByteString -> Quake ()) -> f (EdictT -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giSound              :: Functor f => ((EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()) -> f (EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ())) -> GameImportT -> f GameImportT
giPositionedSound    :: Functor f => ((V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ()) -> f (V3 Float -> EdictT -> Int -> Int -> Float -> Float -> Float -> Quake ())) -> GameImportT -> f GameImportT
giConfigString       :: Functor f => ((Int -> B.ByteString -> Quake ()) -> f (Int -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giError              :: Functor f => ((B.ByteString -> Quake ()) -> f (B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giError2             :: Functor f => ((Int -> B.ByteString -> Quake ()) -> f (Int -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giModelIndex         :: Functor f => ((B.ByteString -> Quake Int) -> f (B.ByteString -> Quake Int)) -> GameImportT -> f GameImportT
giSoundIndex         :: Functor f => ((B.ByteString -> Quake Int) -> f (B.ByteString -> Quake Int)) -> GameImportT -> f GameImportT
giImageIndex         :: Functor f => ((B.ByteString -> Quake Int) -> f (B.ByteString -> Quake Int)) -> GameImportT -> f GameImportT
giSetModel           :: Functor f => ((EdictT -> B.ByteString -> Quake ()) -> f (EdictT -> B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giTrace              :: Functor f => ((V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT) -> f (V3 Float -> V3 Float -> V3 Float -> V3 Float -> EdictT -> Int -> Quake TraceT)) -> GameImportT -> f GameImportT
--, pmove_t.PointContentsAdapter -- TODO: ???
giInPHS              :: Functor f => ((V3 Float -> V3 Float -> Quake Bool) -> f (V3 Float -> V3 Float -> Quake Bool)) -> GameImportT -> f GameImportT
giSetAreaPortalState :: Functor f => ((Int -> Bool -> Quake ()) -> f (Int -> Bool -> Quake ())) -> GameImportT -> f GameImportT
giAreasConnected     :: Functor f => ((Int -> Int -> Quake Bool) -> f (Int -> Int -> Quake Bool)) -> GameImportT -> f GameImportT
giLinkEntity         :: Functor f => ((EdictT -> Quake ()) -> f (EdictT -> Quake ())) -> GameImportT -> f GameImportT
giUnlinkEntity       :: Functor f => ((EdictT -> Quake ()) -> f (EdictT -> Quake ())) -> GameImportT -> f GameImportT
giBoxEdicts          :: Functor f => ((V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int) -> f (V3 Float -> V3 Float -> V.Vector EdictT -> Int -> Int -> Quake Int)) -> GameImportT -> f GameImportT
giPMove              :: Functor f => ((PMoveT -> Quake ()) -> f (PMoveT -> Quake ())) -> GameImportT -> f GameImportT
giMulticast          :: Functor f => ((V3 Float -> Int -> Quake ()) -> f (V3 Float -> Int -> Quake ())) -> GameImportT -> f GameImportT
giUnicast            :: Functor f => ((EdictT -> Bool -> Quake ()) -> f (EdictT -> Bool -> Quake ())) -> GameImportT -> f GameImportT
giWriteByte          :: Functor f => ((Int -> Quake ()) -> f (Int -> Quake ())) -> GameImportT -> f GameImportT
giWriteShort         :: Functor f => ((Int -> Quake ()) -> f (Int -> Quake ())) -> GameImportT -> f GameImportT
giWriteString        :: Functor f => ((B.ByteString -> Quake ()) -> f (B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
giWritePosition      :: Functor f => ((V3 Float -> Quake ()) -> f (V3 Float -> Quake ())) -> GameImportT -> f GameImportT
giWriteDir           :: Functor f => ((V3 Float -> Quake ()) -> f (V3 Float -> Quake ())) -> GameImportT -> f GameImportT
giCVar               :: Functor f => ((B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)) -> f (B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT))) -> GameImportT -> f GameImportT
giCVarSet            :: Functor f => ((B.ByteString -> B.ByteString -> Quake CVarT) -> f (B.ByteString -> B.ByteString -> Quake CVarT)) -> GameImportT -> f GameImportT
giCVarForceSet       :: Functor f => ((B.ByteString -> B.ByteString -> Quake CVarT) -> f (B.ByteString -> B.ByteString -> Quake CVarT)) -> GameImportT -> f GameImportT
giArgc               :: Functor f => (Quake Int -> f (Quake Int)) -> GameImportT -> f GameImportT
giArgv               :: Functor f => ((Int -> Quake B.ByteString) -> f (Int -> Quake B.ByteString)) -> GameImportT -> f GameImportT
giArgs               :: Functor f => (Quake B.ByteString -> f (Quake B.ByteString)) -> GameImportT -> f GameImportT
giAddCommandString   :: Functor f => ((B.ByteString -> Quake ()) -> f (B.ByteString -> Quake ())) -> GameImportT -> f GameImportT
