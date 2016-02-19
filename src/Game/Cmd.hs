module Game.Cmd
  ( addCommand
  , argc
  , argv
  , executeString
  , initialize
  ) where

import qualified Constants
import qualified QCommon.CBufShared as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVarShared as CVar
import qualified QCommon.FSShared as FS
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ
import           QuakeState
import           Types

import           Control.Lens (Lens', use, (^.), (%=), (.=), (+=), (&), (.~))
import           Control.Monad (when, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Char (toUpper, chr)
import           Data.Foldable (find)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Serialize (encode)
import           Data.Sequence ((<|))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

aliasLoopCount :: Int
aliasLoopCount = 16

initialCommands :: [(B.ByteString,XCommandT)]
initialCommands =
  [("exec",execF),("echo",echoF),("cmdlist",listF),("alias",aliasF),("wait",waitF)] 
  
initialize :: Quake ()
initialize = mapM_ (\(name,cmd) -> addCommand name (Just cmd)) initialCommands

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()
addCommand name cmd =
  do variable <- CVar.variableString name
     exists <- commandExists name
     if | not (B.null variable) -> -- fail if the command is a variable name
            Com.printf (B.concat ["Cmd.addCommand: ", name, " already defined as a var\n"])
        | exists -> -- fail if the command already exists
            Com.printf (B.concat ["Cmd.addCommand: ", name, " already defined\n"])
        | otherwise -> -- add new command otherwise
            cmdGlobals.cgCmdFunctions %= (CmdFunctionT name cmd <|)

commandExists :: B.ByteString -> Quake Bool
commandExists name =
  do allCommands <- use $ cmdGlobals.cgCmdFunctions
     return (any (\cmd -> cmd^.cfName == name) allCommands)

execF :: XCommandT
execF = XCommandT "Cmd.execF" $
  do c <- argc
     if c /= 2
       then Com.printf "exec <filename> : execute a script file\n"
       else loadAndExec
  where loadAndExec =
          do fileName <- argv 1
             script <- FS.loadFile fileName
             maybe (failedToExec fileName) (execScript fileName) script
        failedToExec fileName =
             Com.printf (B.concat ["couldn't exec ", fileName, "\n"])
        execScript fileName contents =
          do Com.printf (B.concat ["execing ", fileName, "\n"])
             CBuf.insertText contents

echoF :: XCommandT
echoF = XCommandT "Cmd.echoF" $
  do v <- use (cmdGlobals.cgCmdArgv)
     mapM_ (\arg -> Com.printf (arg `B.append` " ")) (V.drop 1 v)
     Com.printf "'\n"

listF :: XCommandT
listF = XCommandT "Cmd.listF" $
  do allCommands <- use (cmdGlobals.cgCmdFunctions)
     mapM_ (\cmd -> Com.printf ((cmd^.cfName) `B.append` "\n")) allCommands
     Com.printf (encode (Seq.length allCommands) `B.append` " commands \n")
  

aliasF :: XCommandT
aliasF = XCommandT "Cmd.aliasF" $
  do c <- argc
     checkArgs c
  where checkArgs 1 = listAliases
        checkArgs c =
          do arg <- argv 1
             aliasCmd arg c

listAliases :: Quake ()
listAliases =
  do Com.printf "Current alias commands:\n"
     aliases <- use (globals.gCmdAlias)
     mapM_ printAlias aliases

printAlias :: CmdAliasT -> Quake ()
printAlias alias = Com.printf aliasInfo
  where aliasInfo = B.concat [alias^.caName, " : ", alias^.caValue]

aliasCmd :: B.ByteString -> Int -> Quake ()
aliasCmd arg c
  | tooLongAlias = tooLongError
  | otherwise =
      do existingAliasIdx <- findExistingAlias
         cmd <- fmap (`B.append` "\n") (restOfCommandLine 2 c mempty)
         maybe (newAlias arg cmd) (updateExistingAlias arg cmd) existingAliasIdx
  where tooLongAlias = B.length arg > Constants.maxAliasName
        tooLongError = Com.printf "Alias name is too long\n"
        findExistingAlias =
          do aliases <- use (globals.gCmdAlias)
             return (aliasIndex aliases)
        aliasIndex = Seq.findIndexL (sameAliasNameAs name)
        name = BC.map toUpper arg

newAlias :: B.ByteString -> B.ByteString -> Quake ()
newAlias name cmd = globals.gCmdAlias %= (newlyCreatedAlias <|)
  where newlyCreatedAlias = newCmdAliasT & caName .~ name
                                         & caValue .~ cmd

updateExistingAlias :: B.ByteString -> B.ByteString -> Int -> Quake ()
updateExistingAlias name cmd idx =
  globals.gCmdAlias %= Seq.adjust updateF idx
  where updateF x = x & caName .~ name
                      & caValue .~ cmd

restOfCommandLine :: Int -> Int -> BB.Builder -> Quake B.ByteString
restOfCommandLine idx count accum
  | idx >= count = return (BL.toStrict (BB.toLazyByteString accum))
  | otherwise =
      do arg <- argv idx
         restOfCommandLine (idx + 1) count (updateAccum arg)
  where updateAccum arg | idx == count - 1 = accum <> BB.byteString arg
                        | otherwise = accum <> BB.byteString arg <> space
        space = BB.byteString " "

waitF :: XCommandT
waitF = XCommandT "Cmd.waitF" (globals.gCmdWait .= True)

argc :: Quake Int
argc = use (cmdGlobals.cgCmdArgc)

args :: Quake B.ByteString
args = use (cmdGlobals.cgCmdArgs)

argv :: Int -> Quake B.ByteString
argv idx =
  do cmdArgv <- use (cmdGlobals.cgCmdArgv)
     return (fromMaybe "" (cmdArgv V.!? idx))

executeString :: B.ByteString -> Quake ()
executeString text =
  do tokenizeString text True
     tokensCount <- argc
     when (tokensCount > 0) (proceedStringExecution text)

proceedStringExecution :: B.ByteString -> Quake ()
proceedStringExecution text =
  do functions <- use (cmdGlobals.cgCmdFunctions)
     name <- fmap (BC.map toUpper) (argv 0)
     maybe (functionNotFound name) (handleExistingFunction text) (findFunction name functions)
  where findFunction name = find (sameFunctionNameAs name)

functionNotFound :: B.ByteString -> Quake ()
functionNotFound name =
  do aliases <- use (globals.gCmdAlias)
     maybe aliasNotFound handleExistingAlias (findAlias aliases)
  where findAlias = find (sameAliasNameAs name)

handleExistingFunction :: B.ByteString -> CmdFunctionT -> Quake ()
handleExistingFunction text function =
  maybe (executeString ("cmd" `B.append` text)) (^.xcCmd) (function^.cfFunction)

aliasNotFound :: Quake ()
aliasNotFound =
  do cmd <- CVar.command -- check cvars
     unless cmd forwardToServer -- send it as a server command if we are connected

handleExistingAlias :: CmdAliasT -> Quake ()
handleExistingAlias alias =
  do globals.gAliasCount += 1
     count <- use (globals.gAliasCount)
     if count + 1 >= aliasLoopCount
       then Com.printf "ALIAS_LOOP_COUNT\n"
       else CBuf.insertText (alias^.caValue)

sameNameAs :: B.ByteString -> B.ByteString -> Bool
sameNameAs expected given =
  let current = BC.map toUpper given
  in expected == current

sameFunctionNameAs :: B.ByteString -> CmdFunctionT -> Bool
sameFunctionNameAs expected fn = sameNameAs expected (fn^.cfName)

sameAliasNameAs :: B.ByteString -> CmdAliasT -> Bool
sameAliasNameAs expected alias = sameNameAs expected aliasName
  where aliasName = BC.map toUpper (alias^.caName)

tokenizeString :: B.ByteString -> Bool -> Quake ()
tokenizeString text macroExpand =
  do clearArgs
     expandedText <- if macroExpand
                       then macroExpandString text (B.length text)
                       else return (Just text)
     maybe (return ()) (`tokenize` 0) expandedText
  where clearArgs =
          do cmdGlobals.cgCmdArgc .= 0
             cmdGlobals.cgCmdArgs .= ""

tokenize :: B.ByteString -> Int -> Quake ()
tokenize text idx =
  unless (newIdx == textLen || text `BC.index` newIdx == '\n') $
    do cmdArgc <- use (cmdGlobals.cgCmdArgc)
       when (cmdArgc == 1) setArgsAfterFirst
       (var, updatedIdx) <- Com.parse text textLen newIdx
       maybe (return ()) (processVar text cmdArgc updatedIdx) var
  where newIdx = skipWhitesToEOL text idx
        textLen = B.length text
        skipWhitesToEOL str startIdx =
          startIdx + B.length (BC.takeWhile isWhite (B.drop startIdx str))
        isWhite c = c <= ' ' && c /= '\n' && c /= chr 0
        setArgsAfterFirst = cmdGlobals.cgCmdArgs .= trimStr (B.drop newIdx text)
        trimStr = B.reverse . BC.dropWhile (<= ' ') . B.reverse
        
processVar :: B.ByteString -> Int -> Int -> B.ByteString -> Quake ()
processVar text cmdArgc updatedIdx var =
  do when (cmdArgc < Constants.maxStringTokens) (updateArgv cmdArgc var)
     tokenize text updatedIdx
             
updateArgv :: Int -> B.ByteString -> Quake ()
updateArgv cmdArgc var =
  do cmdGlobals.cgCmdArgv %= (V.// [(cmdArgc, var)])
     cmdGlobals.cgCmdArgc += 1

macroExpandString :: B.ByteString -> Int -> Quake (Maybe B.ByteString)
macroExpandString text len
  | lengthExceeded = lineExceededError
  | otherwise = expand text False len 0 0
  where lengthExceeded = len >= Constants.maxStringChars
        lineExceededError =
          do Com.printf (B.concat ["Line exceeded ", encode Constants.maxStringChars, " chars, discarded.\n"])
             return Nothing

expand :: B.ByteString -> Bool -> Int -> Int -> Int -> Quake (Maybe B.ByteString)
expand text inQuote newLen count idx
  | idx == newLen && inQuote = unmatchedQuotesError
  | idx == newLen = return (Just text)
  | shouldContinue = expand text newInQuote newLen count (idx + 1)
  | otherwise =
      do (var, newIdx) <- Com.parse text newLen (idx + 1)
         maybe (expand text newInQuote newLen count newIdx)
               (processParsedVar text newInQuote newLen count idx newIdx)
               var
  where unmatchedQuotesError =
          do Com.printf "Line has unmatched quote, discarded.\n"
             return Nothing
        shouldContinue = newInQuote || text `BC.index` idx /= '$'
        newInQuote | text `BC.index` idx == '"' = not inQuote
                   | otherwise = inQuote

processParsedVar :: B.ByteString -> Bool -> Int -> Int -> Int -> Int -> B.ByteString -> Quake (Maybe B.ByteString)
processParsedVar text newInQuote newLen count idx newIdx var =
  do token <- CVar.variableString var
     if | lineExceeded token -> lineExceededError
        | loopDetected -> loopDetectedError
        | otherwise ->
            do let newText = B.concat [B.take idx text, token, B.drop newIdx text]
               expand newText newInQuote (B.length newText) (count + 1) idx
  where lineExceeded token = newLen + B.length token >= Constants.maxStringChars
        lineExceededError =
          do Com.printf (B.concat ["Expanded line exceeded ", encode Constants.maxStringChars, " chars, discarded.\n"])
             return Nothing
        loopDetected = count + 1 == 100
        loopDetectedError =
          do Com.printf "Macro expansion loop, discarded.\n"
             return Nothing

forwardToServer :: Quake ()
forwardToServer =
  do cmd <- argv 0
     cls <- use (globals.gCls)
     forward cmd cls (BC.head cmd)
  where forward cmd cls c
          | (cls^.csState) <= Constants.caConnected || c == '-' || c == '+' =
              Com.printf (B.concat ["Unknown command \"", cmd, "\"\n"])
          | otherwise = writeMessage cmd

writeMessage :: B.ByteString -> Quake ()
writeMessage cmd =
  do MSG.writeByteI message stringCmd
     SZ.printSB message cmd
     c <- argc
     when (c > 1) addArgs
  where stringCmd = fromIntegral Constants.clcStringCmd
        message :: Lens' QuakeState SizeBufT
        message = globals.gCls.csNetChan.ncMessage
        addArgs =
          do cmdArgs <- args
             SZ.printSB message " "
             SZ.printSB message cmdArgs