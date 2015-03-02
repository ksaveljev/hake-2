module QCommon.Types where

import Data.Word (Word32)
import Data.Int (Int8)
import Linear.V4 (V4)
import Control.Monad.State (StateT)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import QuakeState

type XCommand = StateT QuakeState IO ()

data CmdFunction = CmdFunction { cmdFunctionNext     :: CmdFunction
                               , cmdFunctionName     :: B.ByteString
                               , cmdFunctionFunction :: XCommand
                               }

data Lump = Lump { lumpFileofs :: Int
                 , lumpFilelen :: Int
                 }

data Miptex = Miptex { miptexName      :: B.ByteString
                     , miptexWidth     :: Int
                     , miptexHeight    :: Int
                     , miptexOffsets   :: UV.Vector Int
                     , miptexAnimFrame :: B.ByteString
                     , miptexFlags     :: Int
                     , miptexContents  :: Int
                     , miptexValue     :: Int
                     }

data NetAdr = NetAdr { netAdrType :: Int
                     , netAdrPort :: Int
                     , netAdrIP   :: Word32 -- TODO: how to represent?
                     }

data NetChan = NetChan { netChanFatalError                   :: Bool
                       , netChanSock                         :: Int
                       , netChanDropped                      :: Int
                       , netChanLastReceived                 :: Int
                       , netChanLastSent                     :: Int
                       , netChanRemoteAddress                :: NetAdr
                       , netChanQPort                        :: Int
                       , netChanIncomingSequence             :: Int
                       , netChanIncomingAcknowledged         :: Int
                       , netChanIncomingReliableAcknowledged :: Int
                       , netChanIncomingReliableSequence     :: Int
                       , netChanOutgoingSequence             :: Int
                       , netChanLastReliableSequence         :: Int
                       , netChanMessage                      :: SizeBuf
                       , netChanMessageBuf                   :: UV.Vector Int8 -- TODO: Word8?
                       , netChanReliableLength               :: Int
                       , netChanReliableBuf                  :: UV.Vector Int8 -- TODO: Word8?
                       }

data SizeBuf = SizeBuf { sizeBufAllowOverflow :: Bool
                       , sizeBufOverflowed    :: Bool
                       , sizeBufData          :: UV.Vector Int8
                       , sizeBufMaxSize       :: Int
                       , sizeBufCurSize       :: Int
                       , sizeBufReadCount     :: Int
                       }

data TexInfo = TexInfo { texInfoSize        :: Int
                       , texInfoVecs        :: (V4 Float, V4 Float)
                       , texInfoFlags       :: Int
                       , texInfoValue       :: Int
                       , texInfoTexture     :: B.ByteString
                       , texInfoNextTexInfo :: Int
                       }
