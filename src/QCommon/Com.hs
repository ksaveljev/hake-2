module QCommon.Com
    ( argv
    , blockSequenceCRCByte
    , clearArgv
    , comError
    , dprintf
    , fatalError
    , initializeArgv
    , parse
    , printf
    , quit
    ) where

import           Control.Lens          (use, (.=), (^.), (%=))
import           Control.Monad         (when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (chr)
import           Data.Maybe            (fromMaybe)
import qualified Data.Vector           as V
import           Data.Word             (Word8)

import qualified Client.Console        as Console
import qualified Constants
import           Game.CVarT
import qualified QCommon.CVar          as CVar
import           QuakeState
import qualified Sys.Sys               as Sys
import           Types
import           Util.Binary           (encode)
import qualified Util.Lib              as Lib

import {-# SOURCE #-} qualified Client.CL     as CL
import {-# SOURCE #-} qualified Server.SVMain as SVMain

argv :: Int -> Quake B.ByteString
argv idx = do
    comArgv <- use (comGlobals.cgComArgv)
    return (fromMaybe B.empty (comArgv V.!? idx))

clearArgv :: Int -> Quake ()
clearArgv idx =
    comGlobals.cgComArgv %= (\v -> if idx < 0 || idx >= V.length v then v else v V.// [(idx, B.empty)])

initializeArgv :: [String] -> Quake ()
initializeArgv args = do
    when (len > Constants.maxNumArgvs) $
        fatalError "argc > MAX_NUM_ARGVS"
    comGlobals.cgComArgc .= len
    comGlobals.cgComArgv .= V.fromList (fmap stripLongArg args)
  where
    len = length args 
    stripLongArg s
        | length s > Constants.maxTokenChars = B.empty
        | otherwise = BC.pack s 

printf :: B.ByteString -> Quake ()
printf msg = do
    checkRdTarget =<< use (comGlobals.cgRdTarget)
    Console.printConsole msg
    Sys.consoleOutput msg
    checkLogFile =<< CVar.findVar "logfile"
  where
    checkRdTarget rdTarget
        | rdTarget /= 0 = error "Com.printf#checkRdTarget" -- TODO
        | otherwise = return ()
    checkLogFile Nothing = return ()
    checkLogFile (Just logFile)
        | (logFile^.cvValue) /= 0 = error "Com.printf#checkLogFile" -- TODO
        | otherwise = return ()

dprintf :: B.ByteString -> Quake ()
dprintf str = io (B.putStr str) -- TODO: use printf $ str -- memory is going crazy here, need optimizations

fatalError :: B.ByteString -> Quake ()
fatalError = error "Com.fatalError" -- TODO

comError :: Int -> B.ByteString -> Quake ()
comError _ msg = error ("Com.comError: " ++ BC.unpack msg) -- TODO

parse :: B.ByteString -> Int -> Int -> Quake (Maybe B.ByteString, Int)
parse text len idx
    | skipWhitesIdx >= len = return (Nothing, skipWhitesIdx)
    | isComment = skipComment
    | isQuotedString = return (handleQuotedString text len skipWhitesIdx)
    | otherwise = parseRegularWord text skipWhitesIdx
  where
    skipWhitesIdx = skipWhites text idx
    isComment = B.take 2 (B.drop skipWhitesIdx text) == "//"
    skipComment = parse text len (skipToEOL text skipWhitesIdx)
    isQuotedString = text `BC.index` skipWhitesIdx == '\"'

handleQuotedString :: B.ByteString -> Int -> Int -> (Maybe B.ByteString, Int)
handleQuotedString text len skipWhitesIdx =
    let droppedStr = B.drop (skipWhitesIdx + 1) text
        str = BC.takeWhile notQuote droppedStr
        notQuote c = c /= '\"' && c /= chr 0
        newIdx = skipWhitesIdx + 1 + B.length str
        finalIdx | newIdx >= len = newIdx
                 | otherwise = newIdx + 1 -- we have reached '\"' or NUL so we need to skip it
    in (Just (B.take Constants.maxTokenChars str), finalIdx)

parseRegularWord :: B.ByteString -> Int -> Quake (Maybe B.ByteString, Int)
parseRegularWord text skipWhitesIdx
    | lengthExceeded = lengthExceededError
    | otherwise = return (Just str, newIdx)
  where
    droppedStr = B.drop skipWhitesIdx text
    str = BC.takeWhile (> chr 32) droppedStr
    newIdx = skipWhitesIdx + B.length str
    lengthExceeded = B.length str >= Constants.maxTokenChars
    lengthExceededError = do
        printf (B.concat["Token exceeded ", encode Constants.maxTokenChars, " chars, discarded.\n"])
        return (Just B.empty, newIdx)

skipWhites :: B.ByteString -> Int -> Int
skipWhites str startIdx =
    let droppedStr = B.drop startIdx str
        isWhite c = c <= ' ' && c /= chr 0
    in startIdx + B.length (BC.takeWhile isWhite droppedStr)
  
skipToEOL :: B.ByteString -> Int -> Int
skipToEOL str startIdx =
    let droppedStr = B.drop startIdx str
        notEOL c = c /= '\n' && c /= chr 0
    in startIdx + B.length (BC.takeWhile notEOL droppedStr)

blockSequenceCRCByte :: B.ByteString -> Int -> Int -> Int -> Quake Word8
blockSequenceCRCByte _ _ _ _ =
    io (putStrLn "Com.blockSequenceCRCByte IMPLEMENT ME!") >> return 0

quit :: Quake ()
quit = do
    SVMain.shutdown "Server quit\n" False
    CL.shutdown
    closeLogFile =<< use (globals.gLogFile)
    globals.gLogFile .= Nothing
    Sys.quit
  where
    closeLogFile Nothing = return ()
    closeLogFile (Just fileHandle) = Lib.fClose fileHandle
