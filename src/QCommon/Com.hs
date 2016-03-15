module QCommon.Com
  ( argv
  , clearArgv
  , comError
  , dprintf
  , fatalError
  , initializeArgv
  , parse
  , printf
  ) where

import qualified Constants
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (.=), (%=))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (chr)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

initializeArgv :: [String] -> Quake ()
initializeArgv args =
  do when (len > Constants.maxNumArgvs) $
       fatalError "argc > MAX_NUM_ARGVS"
     comGlobals.cgComArgc .= len
     comGlobals.cgComArgv .= V.fromList (fmap stripLongArg args)
  where len = length args 
        stripLongArg s
          | length s > Constants.maxTokenChars = B.empty
          | otherwise = BC.pack s 

printf :: B.ByteString -> Quake ()
printf str =
  do request (io (B.putStr str)) -- TODO
     request (io (B.putStr "IMPLEMENT Com.printf!!\n"))

dprintf :: B.ByteString -> Quake ()
dprintf str = request (io (B.putStr str)) -- TODO: use printf $ str -- memory is going crazy here, need optimizations

comError :: Int -> B.ByteString -> Quake ()
comError _ msg = error ("Com.comError: " ++ BC.unpack msg) -- TODO

fatalError :: B.ByteString -> Quake ()
fatalError = comError Constants.errFatal

argv :: Int -> Quake B.ByteString
argv idx = do
  comArgv <- use (comGlobals.cgComArgv)
  return (fromMaybe B.empty (comArgv V.!? idx))

clearArgv :: Int -> Quake ()
clearArgv idx =
  comGlobals.cgComArgv %= (\v -> if idx < 0 || idx >= V.length v then v else v V.// [(idx, B.empty)])

parse :: B.ByteString -> Int -> Int -> Quake (Maybe B.ByteString, Int)
parse text len idx
  | skipWhitesIdx >= len = return (Nothing, skipWhitesIdx)
  | isComment = skipComment
  | isQuotedString = return (handleQuotedString text len skipWhitesIdx)
  | otherwise = parseRegularWord text skipWhitesIdx
  where skipWhitesIdx = skipWhites text idx
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
  where droppedStr = B.drop skipWhitesIdx text
        str = BC.takeWhile (> chr 32) droppedStr
        newIdx = skipWhitesIdx + B.length str
        lengthExceeded = B.length str >= Constants.maxTokenChars
        lengthExceededError =
          do printf (B.concat["Token exceeded ", encode Constants.maxTokenChars, " chars, discarded.\n"])
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
