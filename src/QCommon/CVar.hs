module QCommon.CVar 
    ( command
    , findVar
    , forceSet
    , fullSet
    , get
    , getLatchedVars
    , initialize
    , initializeCVars
    , serverInfo
    , set
    , setValueF
    , setValueI
    , update
    , userInfo
    , variableString
    , variableValue
    , writeVariables
    ) where

import           Control.Lens          (use, (^.), (.=), (%=), (&), (.~), (%~))
import           Control.Monad         (void, when, foldM)
import           Data.Bits             ((.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy     as HM
import           Data.Maybe            (isJust, isNothing)
import           System.IO             (Handle, IOMode(ReadWriteMode), hSeek, hFileSize, SeekMode(AbsoluteSeek))

import qualified Constants
import qualified Game.Cmd              as Cmd
import qualified Game.Info             as Info
import           Game.CVarT
import qualified QCommon.FS            as FS
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import           Util.Binary           (encode)

import {-# SOURCE #-} qualified QCommon.Com    as Com

initialize :: Quake ()
initialize = do
    Cmd.addCommand "set" (Just setF)
    Cmd.addCommand "cvarlist" (Just listF)

initializeCVars :: [(B.ByteString, B.ByteString, Int)] -> Quake ()
initializeCVars = mapM_ (\(name, val, flags) -> get name val flags)

setF :: XCommandT
setF = XCommandT "CVar.setF" $
    checkParamsAndSet =<< Cmd.argc
  where
    checkParamsAndSet c
        | c /= 3 && c /= 4 = showUsageMessage
        | c == 4 = handleFullSet
        | otherwise = handleSet
    showUsageMessage = Com.printf "usage: set <variable> <value> [u / s]\n"
    handleFullSet = do
        [v1, v2, v3] <- mapM Cmd.argv [1, 2, 3] -- IMPROVE
        checkParamsAndFullSet v1 v2 v3
    handleSet = do
        [v1, v2] <- mapM Cmd.argv [1, 2] -- IMPROVE
        void (set v1 v2)
    checkParamsAndFullSet v1 v2 "u" = void (fullSet v1 v2 Constants.cvarUserInfo)
    checkParamsAndFullSet v1 v2 "s" = void (fullSet v1 v2 Constants.cvarServerInfo)
    checkParamsAndFullSet _ _ _ = Com.printf "flags can only be 'u' or 's'\n"

fullSet :: B.ByteString -> B.ByteString -> Int -> Quake CVarT
fullSet name value flags = do
    var <- findVar name
    maybe varNotFound updateFoundVar var
  where
    varNotFound = do
        newVar <- get name value flags
        maybe createFailedError return newVar
    updateFoundVar var = do
        checkUserInfoModified var
        let updatedCVar = updateFields var
        update updatedCVar
        return updatedCVar
    updateFields var = var & cvModified .~ True
                           & cvString .~ value
                           & cvValue .~ Lib.atof value
                           & cvFlags .~ flags

listF :: XCommandT
listF = XCommandT "CVar.listF" $ error "CVar.listF" -- TODO

serverInfo :: Quake B.ByteString
serverInfo = error "CVar.serverInfo" -- TODO

setValueF :: B.ByteString -> Float -> Quake ()
setValueF name value = void (set name val)
  where
    val | value == fromIntegral tv = encode tv
        | otherwise = encode value
    tv = truncate value :: Int

setValueI :: B.ByteString -> Int -> Quake ()
setValueI name value = void (set name (encode value))

variableValue :: B.ByteString -> Quake Float
variableValue name = do
    var <- findVar name
    return (maybe 0 (Lib.atof . (^.cvString)) var)

userInfo :: Quake B.ByteString
userInfo = bitInfo Constants.cvarUserInfo

bitInfo :: Int -> Quake B.ByteString
bitInfo bit = do
    vars <- fmap (filter (\v -> (v^.cvFlags) .&. bit /= 0) . HM.elems) (use (globals.gCVars))
    collectInfo B.empty vars
  where
    collectInfo = foldM (\info var -> Info.setValueForKey info (var^.cvName) (var^.cvString))

getLatchedVars :: Quake ()
getLatchedVars = updateLatchedVars =<< use (globals.gCVars)

updateLatchedVars :: HM.HashMap B.ByteString CVarT -> Quake ()
updateLatchedVars cVars = do
    globals.gCVars .= updatedCVars
    when (gameVarBefore /= gameVarAfter) $
        maybe (return ()) autoexec gameVarAfter
  where
    updatedCVars = HM.map doUpdateLatchedVars cVars
    gameVarBefore = HM.lookup "game" cVars
    gameVarAfter = HM.lookup "game" updatedCVars
    autoexec game = do
        FS.setGameDir (game^.cvString)
        FS.execAutoexec

doUpdateLatchedVars :: CVarT -> CVarT
doUpdateLatchedVars var = maybe var doUpdate (var^.cvLatchedString)
  where
    doUpdate latchedString = var & cvString .~ latchedString
                                 & cvLatchedString .~ Nothing
                                 & cvValue .~ Lib.atof latchedString

writeVariables :: B.ByteString -> Quake ()
writeVariables path = do
    fileHandle <- Lib.fOpen path ReadWriteMode
    maybe (return ()) proceedWriteVariables fileHandle
  where
    proceedWriteVariables fileHandle = do
        vars <- use (globals.gCVars)
        io $ do
            fileSize <- hFileSize fileHandle -- IMPROVE: exceptions
            hSeek fileHandle AbsoluteSeek fileSize
            mapM_ (writeVariable fileHandle) vars
        Lib.fClose fileHandle

writeVariable :: Handle -> CVarT -> IO ()
writeVariable fileHandle var
    | (var^.cvFlags) .&. Constants.cvarArchive /= 0 =
        B.hPut fileHandle (B.concat ["set ", var^.cvName, " \"", var^.cvString, "\"\n"])
    | otherwise = return ()

get :: B.ByteString -> B.ByteString -> Int -> Quake (Maybe CVarT)
get name value flags = do
    existingVar <- findVar name
    maybe tryCreatingNewCVar updateExistingVar existingVar
  where
    tryCreatingNewCVar
        | isUserOrServerInfo && not isValidName = invalidCVar "invalid info cvar name\n"
        | isUserOrServerInfo && not isValidValue = invalidCVar "invalid info cvar value\n"
        | otherwise = createNewCVar
    updateExistingVar var = do
        let updatedVar = var & cvFlags %~ (.|. flags)
        update updatedVar
        return (Just updatedVar)
    isUserOrServerInfo = (flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)) /= 0
    isValidName = infoValidate name
    isValidValue = infoValidate value
    invalidCVar msg = do
        Com.printf msg
        return Nothing
    createNewCVar = do
        let newCVar = CVarT name value Nothing flags True (Lib.atof value)
        globals.gCVars %= HM.insert name newCVar
        return (Just newCVar)

-- Some characters are invalid for info strings.
infoValidate :: B.ByteString -> Bool
infoValidate s = not ('\\' `BC.elem` s || '"' `BC.elem` s || ';' `BC.elem` s)

findVar :: B.ByteString -> Quake (Maybe CVarT)
findVar name = do
    vars <- use (globals.gCVars)
    return (HM.lookup name vars)

set :: B.ByteString -> B.ByteString -> Quake CVarT
set name value = set2 name value False

set2 :: B.ByteString -> B.ByteString -> Bool -> Quake CVarT
set2 name value force = do
    var <- findVar name
    maybe createNewCVar (proceedSet2 name value force) var
  where
    createNewCVar = do
        var <- get name value 0
        maybe createFailedError return var

proceedSet2 :: B.ByteString -> B.ByteString -> Bool -> CVarT -> Quake CVarT
proceedSet2 name value force var
    | hasUserServerFlag && invalidValue = do
        Com.printf "invalid info cvar value\n"
        return var
    | not force && writeProtected = do
        Com.printf (name `B.append` " is write protected.\n")
        return var
    | not force && hasLatchFlag = updateLatchedVar var name value
    | otherwise = updateVar var value force
  where
    hasUserServerFlag = userServerFlag /= 0
    writeProtected = noSetFlag /= 0
    hasLatchFlag = latchFlag /= 0
    latchFlag = flags .&. Constants.cvarLatch
    noSetFlag = flags .&. Constants.cvarNoSet
    userServerFlag = flags .&. (Constants.cvarUserInfo .|. Constants.cvarServerInfo)
    flags = var^.cvFlags
    invalidValue = not (infoValidate value)

updateLatchedVar :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
updateLatchedVar var name value = do
    state <- use (globals.gServerState)
    proceedUpdate state
  where
    proceedUpdate state
        | Just value == latchedString = return var
        | isNothing latchedString && value == (var^.cvString) = return var
        | state /= 0 = delayedVarUpdate var name value
        | otherwise = normalVarUpdate var name value
    latchedString = var^.cvLatchedString

delayedVarUpdate :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
delayedVarUpdate var name value = do
    Com.printf (name `B.append` " will be changed for next game.\n")
    update updatedCVar
    return updatedCVar
  where
    updatedCVar = var & cvLatchedString .~ Just value

normalVarUpdate :: CVarT -> B.ByteString -> B.ByteString -> Quake CVarT
normalVarUpdate var name value = do
    update updatedCVar
    handleGameCVar
    return updatedCVar
  where
    updatedCVar = var & cvLatchedString .~ Nothing
                      & cvString .~ value
                      & cvValue .~ Lib.atof value
    handleGameCVar =
        when (name == "game") $ do
            FS.setGameDir value
            FS.execAutoexec

updateVar :: CVarT -> B.ByteString -> Bool -> Quake CVarT
updateVar var value force
    | value == updatedCVar^.cvString = return updatedCVar
    | otherwise = do
        checkUserInfoModified updatedCVar
        update finalCVar
        return finalCVar
  where
    updatedCVar
        | force && isJust (var^.cvLatchedString) = var & cvLatchedString .~ Nothing
        | otherwise = var
    finalCVar = updatedCVar & cvModified .~ True
                            & cvString .~ value
                            & cvValue .~ Lib.atof value

update :: CVarT -> Quake ()
update cvar = globals.gCVars %= HM.insert (cvar^.cvName) cvar

variableString :: B.ByteString -> Quake B.ByteString
variableString varName = do
    foundVar <- findVar varName
    return (maybe B.empty (^.cvString) foundVar)

command :: Quake Bool
command = do
    -- check variables
    v <- Cmd.argv 0
    var <- findVar v
    maybe (return False) doCommand var
  where
    doCommand cvar = do
        c <- Cmd.argc
        printOrSet cvar c
        return True
    printOrSet cvar c
        | c == 1 = Com.printf (B.concat ["\"", cvar^.cvName, "\" is \"", cvar^.cvString, "\"\n"])
        | otherwise = Cmd.argv 1 >>= void . (set (cvar^.cvName))

checkUserInfoModified :: CVarT -> Quake () -- make compiler happy
checkUserInfoModified var =
    when ((var^.cvFlags) .&. Constants.cvarUserInfo /= 0) $
        globals.gUserInfoModified .= True

createFailedError :: Quake CVarT
createFailedError = do
    Com.fatalError "Failed to create cvar"
    return (CVarT B.empty B.empty Nothing 0 False 0)

forceSet :: B.ByteString -> B.ByteString -> Quake CVarT
forceSet name value = set2 name value True
