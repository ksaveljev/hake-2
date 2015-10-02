{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.Cmd where

import Data.Char (chr, toUpper)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Sequence ((<|))
import Data.Traversable (traverse)
import Control.Lens ((^.), (%=), (.=), use)
import Control.Monad (liftM, when, unless, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

import Quake
import QuakeState
import QCommon.XCommandT
import QCommon.CmdFunctionT
import qualified Constants
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified QCommon.FS as FS

aliasLoopCount :: Int
aliasLoopCount = 16

init :: Quake ()
init = do
    addCommand "exec" (Just execF)
    addCommand "echo" (Just echoF)
    addCommand "cmdlist" (Just listF)
    addCommand "alias" (Just aliasF)
    addCommand "wait" (Just waitF)

addCommand :: B.ByteString -> Maybe XCommandT -> Quake ()
addCommand cmdName f = do
    varStr <- CVar.variableString cmdName
    cmdExists <- commandExists cmdName

    -- fail if the command is a variable name
    -- fail if the command already exists
    -- add new command otherwise
    if | B.length varStr > 0 ->
           Com.printf $ "Cmd.addCommand: "
             `B.append` cmdName
             `B.append` " already defined as a var\n"
       | cmdExists ->
           Com.printf $ "Cmd.addCommand: "
             `B.append` cmdName
             `B.append` " already defined\n"
       | otherwise ->
           cmdGlobals.cgCmdFunctions %= (CmdFunctionT cmdName f <|)

  where commandExists :: B.ByteString -> Quake Bool
        commandExists name = do
          allCommands <- use $ cmdGlobals.cgCmdFunctions
          case find (\cmd -> cmd^.cfName == name) allCommands of
            Just _ -> return True
            Nothing -> return False

execF :: XCommandT
execF = do
    c <- argc

    if c /= 2
      then Com.printf "exec <filename> : execute a script file\n"
      else do
        v1 <- argv 1
        f <- FS.loadFile v1

        case f of
          Nothing -> Com.printf $ "couldn't exec " `B.append` v1 `B.append` "\n"
          Just contents -> do
            Com.printf $ "execing " `B.append` v1 `B.append` "\n"
            CBuf.insertText contents

echoF :: XCommandT
echoF = do
    v <- liftM (V.drop 1) (use $ cmdGlobals.cgCmdArgv)
    _ <- traverse (\arg -> Com.printf $ arg `B.append` " ") v
    Com.printf "'\n"

listF :: XCommandT
listF = do
    allCommands <- use $ cmdGlobals.cgCmdFunctions
    _ <- traverse (\cf -> Com.printf $ (cf^.cfName) `B.append` "\n") allCommands
    Com.printf $ BC.pack (show $ Seq.length allCommands) `B.append` " commands \n" -- IMPROVE: maybe use Data.Binary for Int to Bytestring conversion ?

aliasF :: XCommandT
aliasF = do
    c <- argc

    if c == 1
      then do
        Com.printf "Current alias commands:\n"
        aliases <- use $ globals.cmdAlias
        void $ traverse (\alias -> Com.printf $ (alias^.caName)
                                    `B.append` " : "
                                    `B.append` (alias^.caValue)
                        ) aliases
      else do
        s <- argv 1
        let sUp = BC.map toUpper s

        if B.length s > Constants.maxAliasName
          then Com.printf "Alias name is too long\n"
          else do
            -- if the alias already exists, reuse it
            aliases <- use $ globals.cmdAlias
            let foundAlias = find (\a -> BC.map toUpper (a^.caName) == sUp) aliases
                (alias, existing) = case foundAlias of
                                      Nothing -> (newCmdAliasT, False)
                                      Just a -> (a, True)

            cmd <- liftM (`B.append` "\n") (restOfCommandLine 2 c "")
            let updatedAlias = alias { _caName = s, _caValue = cmd }

            if existing
              then globals.cmdAlias %= fmap (\a -> if a == alias then updatedAlias else a)
              else globals.cmdAlias %= (updatedAlias Seq.<|)

  where restOfCommandLine :: Int -> Int -> B.ByteString -> Quake B.ByteString
        restOfCommandLine idx count accum
          | idx >= count = return accum
          | idx == count - 1 = do
              vi <- argv idx
              return $ accum `B.append` vi
          | otherwise = do
              vi <- argv idx
              restOfCommandLine (idx + 1) count (accum `B.append` vi `B.append` " ")

waitF :: XCommandT
waitF = globals.cmdWait .= True

argc :: Quake Int
argc = use $ cmdGlobals.cgCmdArgc

args :: Quake B.ByteString
args = use $ cmdGlobals.cgCmdArgs

argv :: Int -> Quake B.ByteString
argv idx = do
    c <- argc

    if (idx < 0) || (idx >= c)
      then return ""
      else do
        v <- use $ cmdGlobals.cgCmdArgv
        return $ v V.! idx

-- Cmd_ExecuteStrig
--
-- A complete command line has been parsed, so try to execute it 
executeString :: B.ByteString -> Quake ()
executeString text = do
    tokenizeString text True

    c <- argc

    -- only when there are some tokens
    when (c > 0) $ do
      cmdFunctions <- use $ cmdGlobals.cgCmdFunctions
      cmdArgv <- use $ cmdGlobals.cgCmdArgv
      let name = BC.map toUpper (cmdArgv V.! 0)

      case find (sameFunctionNameAs name) cmdFunctions of
        Just cmdFunction ->
                    -- forward to server command
          fromMaybe (executeString $ "cmd " `B.append` text) (cmdFunction^.cfFunction)
        Nothing -> do
          cmdAliases <- use $ globals.cmdAlias
          case find (sameAliasNameAs name) cmdAliases of
            Just alias -> do
              count <- use $ globals.aliasCount
              globals.aliasCount .= count + 1

              if count + 1 >= aliasLoopCount
                then do
                  Com.printf "ALIAS_LOOP_COUNT\n"
                  return ()
                else CBuf.insertText (alias^.caValue)
            Nothing -> do
              cmd <- CVar.command -- check cvars
              unless cmd forwardToServer -- send it as a server command if we are connected

  where sameNameAs :: B.ByteString -> B.ByteString -> Bool
        sameNameAs expected given =
          let current = BC.map toUpper given
          in expected == current

        sameFunctionNameAs :: B.ByteString -> CmdFunctionT -> Bool
        sameFunctionNameAs expected fn = sameNameAs expected (fn^.cfName)

        sameAliasNameAs :: B.ByteString -> CmdAliasT -> Bool
        sameAliasNameAs expected alias = sameNameAs expected (alias^.caName)

-- Cmd_TokenizeString
--
-- Parses the given string into command line tokens. $Cvars will
-- be expanded unless they are in a quoted token.
tokenizeString :: B.ByteString -> Bool -> Quake ()
tokenizeString text macroExpand = do
    cmdGlobals.cgCmdArgc .= 0
    cmdGlobals.cgCmdArgs .= ""

    expandedText <- if macroExpand
                      then macroExpandString text (B.length text)
                      else return (Just text)

    case expandedText of
      Nothing -> return ()
      Just newText -> tokenize newText 0

  where tokenize :: B.ByteString -> Int -> Quake ()
        tokenize txt idx = do
          -- skip whitespace up to a \n
          let newIdx = skipWhitesToEOL txt idx

          -- a newline separates commands in the buffer
          unless (newIdx == B.length txt || txt `BC.index` newIdx == '\n') $ do
            cmdArgc <- use $ cmdGlobals.cgCmdArgc

            -- set cmd_args to everything after the first arg
            when (cmdArgc == 1) $ do
              let str = BC.drop newIdx txt
                  trimmedStr = B.reverse . BC.dropWhile (<= ' ') . B.reverse $ str
              cmdGlobals.cgCmdArgs .= trimmedStr

            (parsedVar, updatedIdx) <- Com.parse txt (B.length txt) newIdx

            case parsedVar of
              Nothing -> return ()
              Just var -> do
                when (cmdArgc < Constants.maxStringTokens) $ do
                  cmdGlobals.cgCmdArgv %= (V.// [(cmdArgc, var)])
                  cmdGlobals.cgCmdArgc .= cmdArgc + 1
                tokenize txt updatedIdx

        skipWhitesToEOL :: B.ByteString -> Int -> Int
        skipWhitesToEOL str startIdx =
          let droppedStr = B.drop startIdx str
          in startIdx + B.length (BC.takeWhile (\c -> c <= ' ' && c /= '\n' && c /= chr 0) droppedStr)

-- Cmd_MacroExpandString
macroExpandString :: B.ByteString -> Int -> Quake (Maybe B.ByteString)
macroExpandString text len =
    if len >= Constants.maxStringChars
      then do
        Com.printf $ "Line exceeded "
          `B.append` BC.pack (show Constants.maxStringChars) -- IMPROVE: use binary package for Int to ByteString conversion?
          `B.append` " chars, discarded.\n"
        return Nothing
      else expand text False len 0 0

  where expand :: B.ByteString -> Bool -> Int -> Int -> Int -> Quake (Maybe B.ByteString)
        expand txt inquote newLen count idx =
          if idx == newLen
            then if inquote
                   then do
                     Com.printf "Line has unmatched quote, discarded.\n"
                     return Nothing
                   else return $ Just txt
            else do
              let newInQuote = if txt `BC.index` idx == '"'
                                 then not inquote
                                 else inquote

              if newInQuote || (txt `BC.index` idx /= '$')
                then expand txt newInQuote newLen count (idx + 1)
                else do
                  (parsedVar, newIdx) <- Com.parse txt newLen (idx + 1)

                  case parsedVar of
                    Nothing -> expand txt newInQuote newLen count newIdx
                    Just var -> do
                      token <- CVar.variableString var
                      let updatedLen = newLen + B.length token

                      if updatedLen >= Constants.maxStringChars
                        then do
                          Com.printf $ "Expanded line exceeded "
                            `B.append` BC.pack (show Constants.maxStringChars) -- IMPROVE: convert Int to ByteString using binary package?
                            `B.append` " chars, discarded.\n"
                          return Nothing
                        else do
                          let newTxt = B.take idx txt `B.append` token `B.append` B.drop newIdx txt

                          if count + 1 == 100
                            then do
                              Com.printf "Macro expansion loop, discarded.\n"
                              return Nothing
                            else expand newTxt newInQuote (B.length newTxt) (count + 1) idx

{-
- Adds the current command line as a clc_stringcmd to the client message.
- things like godmode, noclip, etc, are commands directed to the server, so
- when they are typed in at the console, they will need to be forwarded.
-}
forwardToServer :: Quake ()
forwardToServer = do
    cmd <- argv 0

    clientStatic <- use $ globals.cls
    let ch = cmd `BC.index` 0

    if (clientStatic^.csState) <= Constants.caConnected || ch == '-' || ch == '+'
      then Com.printf $ "Unknown command \"" `B.append` cmd `B.append` "\"\n"
      else do 
        MSG.writeByteI (globals.cls.csNetChan.ncMessage) (fromIntegral Constants.clcStringCmd)
        SZ.print (globals.cls.csNetChan.ncMessage) cmd

        c <- argc

        when (c > 1) $ do
          cmdArgs <- args
          SZ.print (globals.cls.csNetChan.ncMessage) " "
          SZ.print (globals.cls.csNetChan.ncMessage) cmdArgs

clientCommand :: EdictReference -> Quake ()
clientCommand _ = do
    io (putStrLn "Cmd.clientCommand") >> undefined -- TODO

helpF :: EdictReference -> Quake ()
helpF _ = do
    io (putStrLn "Cmd.helpF") >> undefined -- TODO
