module QCommon.FSConstants where

import Data.Char (ord)
import Data.Bits (shiftL)

maxRead :: Int
maxRead = 0x10000

idPakHeader :: Int
idPakHeader = (ord 'K' `shiftL` 24) + (ord 'C' `shiftL` 16) + (ord 'A' `shiftL` 8) + ord 'P'

maxFilesInPack :: Int
maxFilesInPack = 4096
