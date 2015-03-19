{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.CVar where

-- CVar implements console variables. The original code is located in cvar.c

import Data.Maybe (isJust, isNothing, fromJust)
import Data.Bits ((.&.), (.|.))
import Control.Lens ((^.), (%=), (.=), use)
import Control.Monad (void, when, liftM)
import Data.Traversable (traverse)
import System.IO (Handle, IOMode(ReadWriteMode), hSeek, hFileSize, SeekMode(AbsoluteSeek))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import QCommon.XCommandT
import qualified Game.CVarT as CVarT
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.FS as FS
import qualified Constants
import qualified Util.Lib as Lib
import {-# SOURCE #-} qualified Game.Cmd as Cmd

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get varName varValue flags = do
    let isUserOrServerInfo = flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)

    if isUserOrServerInfo /= 0 && not (infoValidate varName)
      then do
        Com.printf "invalid info cvar name\n"
        return Nothing
      else do
        var <- findVar varName

        if | isJust var -> do
               let cvar = fromJust var
                   updatedCVar = cvar { _cvFlags = (cvar^.cvFlags) .|. flags }
               update updatedCVar
               return $ Just updatedCVar
           | isUserOrServerInfo /= 0 && not (infoValidate varValue) -> do
               Com.printf "invalid info cvar value\n"
               return Nothing
           | otherwise -> do
               let newCVar = CVarT.newCVarT { _cvName     = varName
                                            , _cvString   = varValue
                                            , _cvModified = True
                                            , _cvValue    = Lib.atof varValue
                                            , _cvFlags    = flags
                                            }

               globals.cvarVars %= M.insert varName newCVar

               return $ Just newCVar

update :: CVarT -> Quake ()
update cvar = globals.cvarVars %= M.insert (cvar^.cvName) cvar

init :: Quake ()
init = do
    Cmd.addCommand "set" (Just setF)
    Cmd.addCommand "cvarlist" (Just listF)

variableString :: B.ByteString -> Quake B.ByteString
variableString varName = do
    foundVar <- findVar varName

    case foundVar of
      Just v -> return $ v^.cvString
      Nothing -> return ""

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar varName = do
    vars <- use $ globals.cvarVars
    return $ M.lookup varName vars

getExisting :: B.ByteString -> Quake CVarT
getExisting = liftM fromJust . findVar

-- Creates a variable if not found and sets their value, the parsed float value and their flags.
fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet varName value flags = do
    var <- findVar varName

    case var of
      Nothing -> do -- create it
        Just newCVar <- get varName value flags
        return newCVar

      Just cvar -> do
        when (((cvar^.cvFlags) .&. Constants.cvarUserInfo) /= 0) $
          globals.userInfoModified .= True

        let updatedCVar = cvar { _cvModified = True
                               , _cvString = value
                               , _cvValue = Lib.atof value
                               , _cvFlags = flags
                               }

        update updatedCVar

        return updatedCVar

-- Sets the value of the variable without forcing.
set :: B.ByteString -> B.ByteString -> Quake CVarT
set varName value = set2 varName value False

-- Sets the value of the variable with forcing.
forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet varName value = set2 varName value True

-- Gereric set function, sets the value of the variable, with forcing its even possible to 
-- override the variables write protection. 
set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 varName value force = do
    var <- findVar varName

    case var of
      Nothing -> liftM fromJust (get varName value 0) -- create it
      Just cvar -> do
        let cvarFlags = cvar^.cvFlags
            userServerFlag = cvarFlags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)
            noSetFlag = cvarFlags .&. Constants.cvarNoSet
            latchFlag = cvarFlags .&. Constants.cvarLatch

        if | userServerFlag /= 0 && not (infoValidate value) -> do
               Com.printf "invalid info cvar value\n"
               return cvar
           | not force && noSetFlag /= 0 -> do
               Com.printf $ varName `B.append` " is write protected.\n"
               return cvar
           | not force && latchFlag /= 0 -> do
               let latchedString = cvar^.cvLatchedString
               state <- use $ globals.serverState

               if | isJust latchedString && value == fromJust latchedString -> return cvar
                  | isNothing latchedString && value == (cvar^.cvString) -> return cvar
                  | state /= 0 -> do
                      Com.printf $ varName `B.append` " will be changed for next game.\n"
                      let updatedCVar = cvar { _cvLatchedString = Just value }
                      update updatedCVar
                      return updatedCVar
                  | otherwise -> do
                      let updatedCVar = cvar { _cvLatchedString = Nothing
                                             , _cvString = value
                                             , _cvValue = Lib.atof value
                                             }

                      update updatedCVar

                      when (varName == "game") $ do
                        FS.setGameDir value
                        FS.execAutoexec

                      return updatedCVar
           | otherwise -> do
             let updatedCVar = if force && isJust (cvar^.cvLatchedString)
                                 then cvar { _cvLatchedString = Nothing }
                                 else cvar

             if value == (updatedCVar^.cvString)
               then return updatedCVar -- not changed
               else do
                 when (((updatedCVar^.cvFlags) .&. Constants.cvarUserInfo) /= 0) $
                   globals.userInfoModified .= True -- transmit at next oportunity

                 let finalCVar = updatedCVar { _cvModified = True
                                             , _cvString = value
                                             , _cvValue = Lib.atof value
                                             }

                 update finalCVar
                 return finalCVar

-- Set command, sets variables.
setF :: XCommandT
setF = do
    c <- Cmd.argc

    if | (c /= 3) && (c /= 4) ->
           Com.printf "usage: set <variable> <value> [u / s]\n"
       | c == 4 -> do
           v1 <- Cmd.argv 1
           v2 <- Cmd.argv 2
           v3 <- Cmd.argv 3
           if | v3 == "u" -> void $ fullSet v1 v2 Constants.cvarUserInfo
              | v3 == "s" -> void $ fullSet v1 v2 Constants.cvarServerInfo
              | otherwise -> Com.printf "flags can only be 'u' or 's'\n"
       | otherwise -> do
           v1 <- Cmd.argv 1
           v2 <- Cmd.argv 2
           void $ set v1 v2

-- List command, lists all available commands
listF :: XCommandT
listF = do
    vars <- use $ globals.cvarVars

    _ <- traverse printCVar vars

    Com.printf $ BC.pack (show (M.size vars)) `B.append` " cvars\n" -- TODO: maybe use binary package for Int to ByteString conversion?

  where printCVar var = do
          Com.printf $ if (var^.cvFlags .&. Constants.cvarArchive) /= 0 then "*" else " "
          Com.printf $ if (var^.cvFlags .&. Constants.cvarUserInfo) /= 0 then "U" else " "
          Com.printf $ if (var^.cvFlags .&. Constants.cvarServerInfo) /= 0 then "S" else " "

          Com.printf $ if | (var^.cvFlags .&. Constants.cvarNoSet) /= 0 -> "-"
                          | (var^.cvFlags .&. Constants.cvarLatch) /= 0 -> "L"
                          | otherwise -> " "

          Com.printf $ " " `B.append` (var^.cvName)
                           `B.append` " \""
                           `B.append` (var^.cvString)
                           `B.append` "\"\n"

{-
- Sets a float value of a variable.
- 
- The overloading is very important, there was a problem with 
- networt "rate" string --> 10000 became "10000.0" and that wasn't right.
-}
setValueI :: B.ByteString -> Int -> Quake ()
setValueI = undefined -- TODO

setValueF :: B.ByteString -> Float -> Quake ()
setValueF = undefined -- TODO

-- Returns the float value of a variable
variableValue :: B.ByteString -> Quake Float
variableValue varName = do
    var <- findVar varName
    case var of
      Nothing -> return 0
      Just cvar -> return $ Lib.atof (cvar^.cvString)

command :: Quake Bool
command = undefined -- TODO

bitInfo :: Int -> Quake B.ByteString
bitInfo = undefined -- TODO

-- Returns an info string containing all the CVAR_SERVERINFO cvars.
serverInfo :: Quake B.ByteString
serverInfo = bitInfo Constants.cvarServerInfo

-- Any variables with latched values will be updated.
getLatchedVars :: Quake ()
getLatchedVars = undefined -- TODO

-- Returns an info string containing all the CVAR_USERINFO cvars.
userInfo :: Quake B.ByteString
userInfo = bitInfo Constants.cvarUserInfo

-- Appends lines containing \"set vaqriable value\" for all variables
-- with the archive flag set true. 
writeVariables :: B.ByteString -> Quake ()
writeVariables path = do
    f <- Lib.fOpen path ReadWriteMode

    case f of
      Nothing -> return ()
      Just h -> do
        -- IMPROVE: catch exceptions
        fileSize <- io $ hFileSize h
        io $ hSeek h AbsoluteSeek fileSize

        vars <- use $ globals.cvarVars
        void $ traverse (writeVar h) vars
        Lib.fClose h

  where writeVar :: Handle -> CVarT -> Quake ()
        writeVar handle cvar =
          when ((cvar^.cvFlags) .&. Constants.cvarArchive /= 0) $
            io $ B.hPut handle $ "set " `B.append` (cvar^.cvName) `B.append` " \"" `B.append` (cvar^.cvString) `B.append` "\"\n"

-- Variable typing auto completition.
completeVariable :: B.ByteString -> UV.Vector B.ByteString
completeVariable = undefined -- TODO

-- Some characters are invalid for info strings.
infoValidate :: B.ByteString -> Bool
infoValidate s = not ('\\' `BC.elem` s || '"' `BC.elem` s || ';' `BC.elem` s)
