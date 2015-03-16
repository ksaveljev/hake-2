{-# LANGUAGE OverloadedStrings #-}
module QCommon.Com where

import Data.Char (chr)
import Data.Word (Word8)
import Control.Monad (when, void, unless)
import Control.Lens ((.=), (%=), use)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Constants
import qualified QCommon.Com as Com
import qualified Sys.Sys as Sys

-- checks the number of command line arguments and
-- copies all arguments with valid length into comArgv
initArgv :: [String] -> Quake ()
initArgv args = do
    let len = length args

    when (len > Constants.maxNumArgvs) $ comError Constants.errFatal "argc > MAX_NUM_ARGVS"

    comGlobals.cgComArgc .= len
    comGlobals.cgComArgv .= V.fromList (fmap (BC.pack . stripLongArg) args)

  where stripLongArg s = if length s > Constants.maxTokenChars then "" else s

comError :: Int -> B.ByteString -> Quake ()
comError code fmt = do
    recursive <- use $ comGlobals.cgRecursive

    when recursive $ do
      msg <- use $ comGlobals.cgMsg
      Sys.sysError ("recursive error after: " `B.append` msg)

    comGlobals.cgRecursive .= True
    comGlobals.cgMsg .= fmt -- TODO: we do not have arguments for sprintf here like jake does

    void $ errReact code

    Sys.sysError fmt

  where errReact c | c == Constants.errDisconnect = undefined -- TODO
        errReact c | c == Constants.errDrop = undefined -- TODO
        errReact _ = undefined -- TODO

printf :: B.ByteString -> Quake ()
printf = io.print -- TODO

dprintf :: B.ByteString -> Quake ()
dprintf = io.print -- TODO

argc :: Quake Int
argc = use $ comGlobals.cgComArgc

argv :: Int -> Quake B.ByteString
argv idx = do
    c <- use $ comGlobals.cgComArgc

    if idx < 0 || idx >= c
      then return ""
      else do
        args <- use $ comGlobals.cgComArgv
        return $ args V.! idx

clearArgv :: Int -> Quake ()
clearArgv idx = do
    c <- use $ comGlobals.cgComArgc

    unless (idx < 0 || idx >= c) $ comGlobals.cgComArgv %= (V.// [(idx, "")])

errorF :: XCommandT
errorF = do
    v1 <- argv 1
    comError Constants.errFatal v1

parse :: B.ByteString -> Int -> Int -> Quake (B.ByteString, Int)
parse txt len idx = do
    let skipWhitesIdx = skipWhites txt idx

    if skipWhitesIdx >= len
      then return ("", skipWhitesIdx)
      else
        if BC.take 2 (B.drop skipWhitesIdx txt) == "//" -- skip // comments
          then do
            let skipToEolIdx = skipToEOL txt skipWhitesIdx
            parse txt len skipToEolIdx
          else
            if txt `BC.index` skipWhitesIdx == '\"'
              then do -- handle quoted strings specially
                let droppedStr = B.drop (skipWhitesIdx + 1) txt
                    str = BC.takeWhile (\c -> c == '\"' || c == chr 0) droppedStr
                    newIdx = skipWhitesIdx + 1 + B.length str
                    finalIdx = if newIdx >= len
                                 then newIdx
                                 else newIdx + 1 -- we have reached '\"' or NUL so we need to skip it
                return (B.take Constants.maxTokenChars str, finalIdx)
              else do -- parse a regular word
                let droppedStr = B.drop skipWhitesIdx txt
                    str = BC.takeWhile (\c -> c > chr 32) droppedStr
                    newIdx = skipWhitesIdx + B.length str

                if B.length str >= Constants.maxTokenChars
                  then do
                    Com.printf $ "Token exceeded " `B.append` BC.pack (show Constants.maxTokenChars) `B.append` " chars, discarded.\n"
                    return ("", newIdx)
                  else return (str, newIdx)

  where skipWhites :: B.ByteString -> Int -> Int
        skipWhites str startIdx =
          let droppedStr = B.drop startIdx str
          in startIdx + B.length (BC.takeWhile (\c -> c <= ' ' && c /= chr 0) droppedStr)

        skipToEOL :: B.ByteString -> Int -> Int
        skipToEOL str startIdx =
          let droppedStr = B.drop startIdx str
          in startIdx + B.length (BC.takeWhile (\c -> c /= '\n' && c /= chr 0) droppedStr)

-- CRC table
chktbl :: UV.Vector Word8
chktbl = UV.fromList [ 0x84, 0x47, 0x51, 0xc1, 0x93, 0x22, 0x21, 0x24
                     , 0x2f, 0x66, 0x60, 0x4d, 0xb0, 0x7c, 0xda, 0x88
                     , 0x54, 0x15, 0x2b, 0xc6, 0x6c, 0x89, 0xc5, 0x9d
                     , 0x48, 0xee, 0xe6, 0x8a, 0xb5, 0xf4, 0xcb, 0xfb
                     , 0xf1, 0x0c, 0x2e, 0xa0, 0xd7, 0xc9, 0x1f, 0xd6
                     , 0x06, 0x9a, 0x09, 0x41, 0x54, 0x67, 0x46, 0xc7
                     , 0x74, 0xe3, 0xc8, 0xb6, 0x5d, 0xa6, 0x36, 0xc4
                     , 0xab, 0x2c, 0x7e, 0x85, 0xa8, 0xa4, 0xa6, 0x4d
                     , 0x96, 0x19, 0x19, 0x9a, 0xcc, 0xd8, 0xac, 0x39
                     , 0x5e, 0x3c, 0xf2, 0xf5, 0x5a, 0x72, 0xe5, 0xa9
                     , 0xd1, 0xb3, 0x23, 0x82, 0x6f, 0x29, 0xcb, 0xd1
                     , 0xcc, 0x71, 0xfb, 0xea, 0x92, 0xeb, 0x1c, 0xca
                     , 0x4c, 0x70, 0xfe, 0x4d, 0xc9, 0x67, 0x43, 0x47
                     , 0x94, 0xb9, 0x47, 0xbc, 0x3f, 0x01, 0xab, 0x7b
                     , 0xa6, 0xe2, 0x76, 0xef, 0x5a, 0x7a, 0x29, 0x0b
                     , 0x51, 0x54, 0x67, 0xd8, 0x1c, 0x14, 0x3e, 0x29
                     , 0xec, 0xe9, 0x2d, 0x48, 0x67, 0xff, 0xed, 0x54
                     , 0x4f, 0x48, 0xc0, 0xaa, 0x61, 0xf7, 0x78, 0x12
                     , 0x03, 0x7a, 0x9e, 0x8b, 0xcf, 0x83, 0x7b, 0xae
                     , 0xca, 0x7b, 0xd9, 0xe9, 0x53, 0x2a, 0xeb, 0xd2
                     , 0xd8, 0xcd, 0xa3, 0x10, 0x25, 0x78, 0x5a, 0xb5
                     , 0x23, 0x06, 0x93, 0xb7, 0x84, 0xd2, 0xbd, 0x96
                     , 0x75, 0xa5, 0x5e, 0xcf, 0x4e, 0xe9, 0x50, 0xa1
                     , 0xe6, 0x9d, 0xb1, 0xe3, 0x85, 0x66, 0x28, 0x4e
                     , 0x43, 0xdc, 0x6e, 0xbb, 0x33, 0x9e, 0xf3, 0x0d
                     , 0x00, 0xc1, 0xcf, 0x67, 0x34, 0x06, 0x7c, 0x71
                     , 0xe3, 0x63, 0xb7, 0xb7, 0xdf, 0x92, 0xc4, 0xc2
                     , 0x25, 0x5c, 0xff, 0xc3, 0x6e, 0xfc, 0xaa, 0x1e
                     , 0x2a, 0x48, 0x11, 0x1c, 0x36, 0x68, 0x78, 0x86
                     , 0x79, 0x30, 0xc3, 0xd6, 0xde, 0xbc, 0x3a, 0x2a
                     , 0x6d, 0x1e, 0x46, 0xdd, 0xe0, 0x80, 0x1e, 0x44
                     , 0x3b, 0x6f, 0xaf, 0x31, 0xda, 0xa2, 0xbd, 0x77
                     , 0x06, 0x56, 0xc0, 0xb7, 0x92, 0x4b, 0x37, 0xc0
                     , 0xfc, 0xc2, 0xd5, 0xfb, 0xa8, 0xda, 0xf5, 0x57
                     , 0xa8, 0x18, 0xc0, 0xdf, 0xe7, 0xaa, 0x2a, 0xe0
                     , 0x7c, 0x6f, 0x77, 0xb1, 0x26, 0xba, 0xf9, 0x2e
                     , 0x1d, 0x16, 0xcb, 0xb8, 0xa2, 0x44, 0xd5, 0x2f
                     , 0x1a, 0x79, 0x74, 0x87, 0x4b, 0x00, 0xc9, 0x4a
                     , 0x3a, 0x65, 0x8f, 0xe6, 0x5d, 0xe5, 0x0a, 0x77
                     , 0xd8, 0x1a, 0x14, 0x41, 0x75, 0xb1, 0xe2, 0x50
                     , 0x2c, 0x93, 0x38, 0x2b, 0x6d, 0xf3, 0xf6, 0xdb
                     , 0x1f, 0xcd, 0xff, 0x14, 0x70, 0xe7, 0x16, 0xe8
                     , 0x3d, 0xf0, 0xe3, 0xbc, 0x5e, 0xb6, 0x3f, 0xcc
                     , 0x81, 0x24, 0x67, 0xf3, 0x97, 0x3b, 0xfe, 0x3a
                     , 0x96, 0x85, 0xdf, 0xe4, 0x6e, 0x3c, 0x85, 0x05
                     , 0x0e, 0xa3, 0x2b, 0x07, 0xc8, 0xbf, 0xe5, 0x13
                     , 0x82, 0x62, 0x08, 0x61, 0x69, 0x4b, 0x47, 0x62
                     , 0x73, 0x44, 0x64, 0x8e, 0xe2, 0x91, 0xa6, 0x9a
                     , 0xb7, 0xe9, 0x04, 0xb6, 0x54, 0x0c, 0xc5, 0xa9
                     , 0x47, 0xa6, 0xc9, 0x08, 0xfe, 0x4e, 0xa6, 0xcc
                     , 0x8a, 0x5b, 0x90, 0x6f, 0x2b, 0x3f, 0xb6, 0x0a
                     , 0x96, 0xc0, 0x78, 0x58, 0x3c, 0x76, 0x6d, 0x94
                     , 0x1a, 0xe4, 0x4e, 0xb8, 0x38, 0xbb, 0xf5, 0xeb
                     , 0x29, 0xd8, 0xb0, 0xf3, 0x15, 0x1e, 0x99, 0x96
                     , 0x3c, 0x5d, 0x63, 0xd5, 0xb1, 0xad, 0x52, 0xb8
                     , 0x55, 0x70, 0x75, 0x3e, 0x1a, 0xd5, 0xda, 0xf6
                     , 0x7a, 0x48, 0x7d, 0x44, 0x41, 0xf9, 0x11, 0xce
                     , 0xd7, 0xca, 0xa5, 0x3d, 0x7a, 0x79, 0x7e, 0x7d
                     , 0x25, 0x1b, 0x77, 0xbc, 0xf7, 0xc7, 0x0f, 0x84
                     , 0x95, 0x10, 0x92, 0x67, 0x15, 0x11, 0x5a, 0x5e
                     , 0x41, 0x66, 0x0f, 0x38, 0x03, 0xb2, 0xf1, 0x5d
                     , 0xf8, 0xab, 0xc0, 0x02, 0x76, 0x84, 0x28, 0xf4
                     , 0x9d, 0x56, 0x46, 0x60, 0x20, 0xdb, 0x68, 0xa7
                     , 0xbb, 0xee, 0xac, 0x15, 0x01, 0x2f, 0x20, 0x09
                     , 0xdb, 0xc0, 0x16, 0xa1, 0x89, 0xf9, 0x94, 0x59
                     , 0x00, 0xc1, 0x76, 0xbf, 0xc1, 0x4d, 0x5d, 0x2d
                     , 0xa9, 0x85, 0x2c, 0xd6, 0xd3, 0x14, 0xcc, 0x02
                     , 0xc3, 0xc2, 0xfa, 0x6b, 0xb7, 0xa6, 0xef, 0xdd
                     , 0x12, 0x26, 0xa4, 0x63, 0xe3, 0x62, 0xbd, 0x56
                     , 0x8a, 0x52, 0x2b, 0xb9, 0xdf, 0x09, 0xbc, 0x0e
                     , 0x97, 0xa9, 0xb0, 0x82, 0x46, 0x08, 0xd5, 0x1a
                     , 0x8e, 0x1b, 0xa7, 0x90, 0x98, 0xb9, 0xbb, 0x3c
                     , 0x17, 0x9a, 0xf2, 0x82, 0xba, 0x64, 0x0a, 0x7f
                     , 0xca, 0x5a, 0x8c, 0x7c, 0xd3, 0x79, 0x09, 0x5b
                     , 0x26, 0xbb, 0xbd, 0x25, 0xdf, 0x3d, 0x6f, 0x9a
                     , 0x8f, 0xee, 0x21, 0x66, 0xb0, 0x8d, 0x84, 0x4c
                     , 0x91, 0x45, 0xd4, 0x77, 0x4f, 0xb3, 0x8c, 0xbc
                     , 0xa8, 0x99, 0xaa, 0x19, 0x53, 0x7c, 0x02, 0x87
                     , 0xbb, 0x0b, 0x7c, 0x1a, 0x2d, 0xdf, 0x48, 0x44
                     , 0x06, 0xd6, 0x7d, 0x0c, 0x2d, 0x35, 0x76, 0xae
                     , 0xc4, 0x5f, 0x71, 0x85, 0x97, 0xc4, 0x3d, 0xef
                     , 0x52, 0xbe, 0x00, 0xe4, 0xcd, 0x49, 0xd1, 0xd1
                     , 0x1c, 0x3c, 0xd0, 0x1c, 0x42, 0xaf, 0xd4, 0xbd
                     , 0x58, 0x34, 0x07, 0x32, 0xee, 0xb9, 0xb5, 0xea
                     , 0xff, 0xd7, 0x8c, 0x0d, 0x2e, 0x2f, 0xaf, 0x87
                     , 0xbb, 0xe6, 0x52, 0x71, 0x22, 0xf5, 0x25, 0x17
                     , 0xa1, 0x82, 0x04, 0xc2, 0x4a, 0xbd, 0x57, 0xc6
                     , 0xab, 0xc8, 0x35, 0x0c, 0x3c, 0xd9, 0xc2, 0x43
                     , 0xdb, 0x27, 0x92, 0xcf, 0xb8, 0x25, 0x60, 0xfa
                     , 0x21, 0x3b, 0x04, 0x52, 0xc8, 0x96, 0xba, 0x74
                     , 0xe3, 0x67, 0x3e, 0x8e, 0x8d, 0x61, 0x90, 0x92
                     , 0x59, 0xb6, 0x1a, 0x1c, 0x5e, 0x21, 0xc1, 0x65
                     , 0xe5, 0xa6, 0x34, 0x05, 0x6f, 0xc5, 0x60, 0xb1
                     , 0x83, 0xc1, 0xd5, 0xd5, 0xed, 0xd9, 0xc7, 0x11
                     , 0x7b, 0x49, 0x7a, 0xf9, 0xf9, 0x84, 0x47, 0x9b
                     , 0xe2, 0xa5, 0x82, 0xe0, 0xc2, 0x88, 0xd0, 0xb2
                     , 0x58, 0x88, 0x7f, 0x45, 0x09, 0x67, 0x74, 0x61
                     , 0xbf, 0xe6, 0x40, 0xe2, 0x9d, 0xc2, 0x47, 0x05
                     , 0x89, 0xed, 0xcb, 0xbb, 0xb7, 0x27, 0xe7, 0xdc
                     , 0x7a, 0xfd, 0xbf, 0xa8, 0xd0, 0xaa, 0x10, 0x39
                     , 0x3c, 0x20, 0xf0, 0xd3, 0x6e, 0xb1, 0x72, 0xf8
                     , 0xe6, 0x0f, 0xef, 0x37, 0xe5, 0x09, 0x33, 0x5a
                     , 0x83, 0x43, 0x80, 0x4f, 0x65, 0x2f, 0x7c, 0x8c
                     , 0x6a, 0xa0, 0x82, 0x0c, 0xd4, 0xd4, 0xfa, 0x81
                     , 0x60, 0x3d, 0xdf, 0x06, 0xf1, 0x5f, 0x08, 0x0d
                     , 0x6d, 0x43, 0xf2, 0xe3, 0x11, 0x7d, 0x80, 0x32
                     , 0xc5, 0xfb, 0xc5, 0xd9, 0x27, 0xec, 0xc6, 0x4e
                     , 0x65, 0x27, 0x76, 0x87, 0xa6, 0xee, 0xee, 0xd7
                     , 0x8b, 0xd1, 0xa0, 0x5c, 0xb0, 0x42, 0x13, 0x0e
                     , 0x95, 0x4a, 0xf2, 0x06, 0xc6, 0x43, 0x33, 0xf4
                     , 0xc7, 0xf8, 0xe7, 0x1f, 0xdd, 0xe4, 0x46, 0x4a
                     , 0x70, 0x39, 0x6c, 0xd0, 0xed, 0xca, 0xbe, 0x60
                     , 0x3b, 0xd1, 0x7b, 0x57, 0x48, 0xe5, 0x3a, 0x79
                     , 0xc1, 0x69, 0x33, 0x53, 0x1b, 0x80, 0xb8, 0x91
                     , 0x7d, 0xb4, 0xf6, 0x17, 0x1a, 0x1d, 0x5a, 0x32
                     , 0xd6, 0xcc, 0x71, 0x29, 0x3f, 0x28, 0xbb, 0xf3
                     , 0x5e, 0x71, 0xb8, 0x43, 0xaf, 0xf8, 0xb9, 0x64
                     , 0xef, 0xc4, 0xa5, 0x6c, 0x08, 0x53, 0xc7, 0x00
                     , 0x10, 0x39, 0x4f, 0xdd, 0xe4, 0xb6, 0x19, 0x27
                     , 0xfb, 0xb8, 0xf5, 0x32, 0x73, 0xe5, 0xcb, 0x32
                     ]
