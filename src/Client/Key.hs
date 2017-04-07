module Client.Key
    ( clearStates
    , clearTyping
    , event
    , initialize
    , writeBindings
    ) where

import           Control.Lens          (use, ix, (.=), (^.), (%=), (+=), (-=))
import           Control.Monad         (unless, when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (ord, toUpper, chr)
import           Data.Maybe            (fromMaybe, isNothing)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as UV
import           System.IO             (Handle)

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.Console        as Console
import           Client.FrameT
import           Client.KeyConstants
import qualified Client.Menu           as Menu
import qualified Constants
import qualified Game.Cmd              as Cmd
import           Game.PlayerStateT
import qualified QCommon.CBufShared    as CBuf
import qualified QCommon.Com           as Com
import           QCommon.XCommandT     (runXCommandT)
import           QuakeState
import           Types
import           Util.Binary           (encode)

keyboardButtons :: [Int]
keyboardButtons = 
    [ kEnter, kKpEnter, kTab, kLeftArrow, kKpLeftArrow, kRightArrow, kKpRightArrow
    , kUpArrow, kKpUpArrow, kDownArrow, kKpDownArrow, kBackspace, kHome, kKpHome
    , kEnd, kKpEnd, kPgUp, kKpPgUp, kPgDn, kKpPgDn, kShift, kIns, kKpIns, kKpDel
    , kKpSlash, kKpPlus, kKpMinus, kKp5 ]

consoleKeys :: [Int]
consoleKeys = [32..127] ++ keyboardButtons

menuBoundKeys :: [Int]
menuBoundKeys = [kF1, kF2, kF3, kF4, kF5, kF6, kF7, kF8, kF9, kF10, kF11, kF12, kEscape]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
    [ ("bind", Just bindF), ("unbind", Just unbindF)
    , ("unbindall", Just unbindAllF), ("bindlist", Just bindListF)
    ] 

initialize :: Quake ()
initialize = do
    globals.gKeyLines .= V.replicate 32 (B.pack [93, 0]) -- 93 is ']'
    globals.gKeyLinePos .= 1
    keyGlobals.kgConsoleKeys %= (UV.// ((consoleKeys `zip` repeat True) ++ [(96, False), (126, False)])) -- 96 is '`', 126 is '~'
    keyGlobals.kgMenuBound %= (UV.// (menuBoundKeys `zip` repeat True))
    Cmd.addInitialCommands initialCommands

bindF :: XCommandT
bindF = XCommandT "Key.bindF" $
    checkArgs =<< Cmd.argc
  where
    checkArgs c
        | c < 2 = Com.printf "bind <key> [command] : attach a command to a key\n"
        | otherwise = do
            arg <- Cmd.argv 1
            key <- stringToKeynum arg
            bind arg key c

bind :: B.ByteString -> Int -> Int -> Quake ()
bind arg key c
    | key == -1 = Com.printf (B.concat ["\"", arg, "\" isn't a valid key\n"])
    | c == 2 = bindInfo arg key
    | otherwise = do
        cmd <- fmap (B.intercalate " ") (mapM Cmd.argv [2..c-1])
        setBinding key (Just cmd)

bindInfo :: B.ByteString -> Int -> Quake ()
bindInfo arg key = do
    keyBindings <- use (globals.gKeyBindings)
    maybe notBound printBindInfo (keyBindings V.! key)
  where
    notBound = Com.printf (B.concat ["\"", arg, "\" is not bound\n"])
    printBindInfo binding = Com.printf (B.concat ["\"", arg, "\" = \"", binding, "\"\n"])

setBinding :: Int -> Maybe B.ByteString -> Quake ()
setBinding keyNum binding =
    unless (keyNum == -1) $
        globals.gKeyBindings %= (V.// [(keyNum, binding)])

stringToKeynum :: B.ByteString -> Quake Int
stringToKeynum str
    | B.length str == 1 = return (ord (BC.index str 0))
    | otherwise = do
        keyNames <- use (keyGlobals.kgKeyNames)
        return (fromMaybe (-1) (V.findIndex (== Just upperStr) keyNames))
  where
    upperStr = BC.map toUpper str

unbindF :: XCommandT
unbindF = XCommandT "Key.unbindF" $
    checkArgs =<< Cmd.argc
  where
    checkArgs c
        | c /= 2 = Com.printf "unbind <key> : remove commands from a key\n"
        | otherwise = unbind

unbind :: Quake ()
unbind = do
    arg <- Cmd.argv 1
    keynum <- stringToKeynum arg
    clearBinding arg keynum
  where
    clearBinding arg keynum
        | keynum == -1 = Com.printf (B.concat ["\"", arg, "\" isn't a valid key\n"])
        | otherwise = setBinding keynum Nothing

unbindAllF :: XCommandT
unbindAllF = XCommandT "Key.unbindAllF" (globals.gKeyBindings .= V.replicate 256 Nothing)

bindListF :: XCommandT
bindListF = XCommandT "Key.bindListF" $ do
    bindings <- use (globals.gKeyBindings)
    V.imapM_ printBinding bindings

printBinding :: Int -> Maybe B.ByteString -> Quake ()
printBinding _ Nothing = return ()
printBinding idx (Just binding)
    | B.null binding = return ()
    | otherwise = do
        strKeynum <- keynumToString idx
        Com.printf (bindingInfo strKeynum)
  where
    bindingInfo strKeynum = B.concat [strKeynum, " \"", binding, "\"\n"]

keynumToString :: Int -> Quake B.ByteString
keynumToString keynum = do
    keyNames <- use (keyGlobals.kgKeyNames)
    return (convertKeyNum keyNames)
  where
    convertKeyNum keyNames
        | keynum < 0 || keynum > 255 = "<KEY NOT FOUND>"
        | keynum > 32 && keynum < 127 = BC.pack [chr keynum]
        | otherwise = fromMaybe "<UNKNOWN KEYNUM>" (keyNames V.! keynum)

writeBindings :: Handle -> Quake ()
writeBindings fileHandle = do
    kb <- use (globals.gKeyBindings)
    V.imapM_ (writeKeyBinding fileHandle) kb -- IMPROVE: move io outside

writeKeyBinding :: Handle -> Int -> Maybe B.ByteString -> Quake ()
writeKeyBinding _ _ Nothing = return ()
writeKeyBinding fileHandle num (Just binding) = do
    keyStr <- keynumToString num
    io (B.hPut fileHandle (B.concat ["bind ", keyStr, " \"", binding, "\"\n"]))

event :: Int -> Bool -> Int -> Quake ()
event key down time = do
    -- TODO: do we need this?
    -- // hack for modal presses
    -- if (key_waiting == -1) {
    --     if (down)
    --         key_waiting = key;
    --     return;
    -- }
    cls <- use (globals.gCls)
    done <- updateAutoRepeatStatus cls
    unless done $ do
        when (key == kShift) $
            keyGlobals.kgShiftDown .= down
        -- console key is hardcoded, so the user can never unbind it
        done' <- checkConsoleKey
        unless done' $ do
            cl <- use (globals.gCl)
            let k = if (cl^.csAttractLoop) && (cls^.csKeyDest) /= Constants.keyMenu && not (key >= kF1 && key <= kF12)
                        then kEscape
                        else key
            processEvent cl cls k down time
  where
    updateAutoRepeatStatus cls
        | down = do
            keyGlobals.kgKeyRepeats.ix key += 1
            keyRepeats <- use (keyGlobals.kgKeyRepeats)
            keyBindings <- use (globals.gKeyBindings)
            checkAutoRepeatStatus cls keyRepeats keyBindings
        | otherwise = do
            keyGlobals.kgKeyRepeats.ix key .= 0
            return False
    checkAutoRepeatStatus cls keyRepeats keyBindings
        | keyRepeats UV.! key > 1 && (cls^.csKeyDest) == Constants.keyGame && not ((cls^.csState) == Constants.caDisconnected) = -- ignore most autorepeats
            return True
        | key >= 200 && isNothing (keyBindings V.! key) = do
            v <- keynumToString key
            Com.printf (v `B.append` " is unbound, hit F4 to set.\n")
            return False
        | otherwise =
            return False
    checkConsoleKey
        | key == ord '`' || key == ord '~' = consoleEvent
        | otherwise = return False
    consoleEvent
        | not down = return True
        | otherwise = do
            runXCommandT Console.toggleConsoleF
            return True
processEvent :: ClientStateT -> ClientStaticT -> Int -> Bool -> Int -> Quake ()
processEvent cl cls key down time
    | key == kEscape && down = processEscapeKey cl cls key
    | key == kEscape = return ()
    | otherwise = do
        globals.gKeyDown.ix key .= down
        checkAnyKeyDown
        checkKeyUpAndDown
        error "Key.processEvent" -- TODO
  where
    checkAnyKeyDown
        | down = do
            keyRepeats <- use (keyGlobals.kgKeyRepeats)
            when (keyRepeats UV.! key == 1) $
                keyGlobals.kgAnyKeyDown += 1
        | otherwise = do
            anyKeyDown <- use (keyGlobals.kgAnyKeyDown)
            when (anyKeyDown > 0) $
                keyGlobals.kgAnyKeyDown -= 1
    checkKeyUpAndDown
        | not down = do
            keyBindings <- use (globals.gKeyBindings)
            maybe (return ()) checkUpEvent (keyBindings V.! key)
        | otherwise = do
            menuBound <- use (keyGlobals.kgMenuBound)
            consoleKeys <- use (keyGlobals.kgConsoleKeys)
            doProcessEvent cl cls key down time (menuBound UV.! key) (consoleKeys UV.! key)
    checkUpEvent binding =
        when (B.length binding > 0 && binding `BC.index` 0 == '+') $
            CBuf.addText (B.concat ["-", B.drop 1 binding, " ", encode key, " ", encode time, "\n"])

doProcessEvent :: ClientStateT -> ClientStaticT -> Int -> Bool -> Int -> Bool -> Bool -> Quake ()
doProcessEvent cl cls key down time menuBound consoleKey
    | ((cls^.csKeyDest) == Constants.keyMenu && menuBound) ||
      ((cls^.csKeyDest) == Constants.keyConsole && not consoleKey) ||
      ((cls^.csKeyDest) == Constants.keyGame && ((cls^.csState) == Constants.caActive || not consoleKey)) = do
        keyBindings <- use (globals.gKeyBindings)
        maybe (return ()) addKeyCommand (keyBindings V.! key)
    | otherwise =
        when down processKeyDownEvent
  where
    addKeyCommand binding
        | B.length binding > 0 && binding `BC.index` 0 == '+' =
            CBuf.addText (B.concat [binding, " ", encode key, " ", encode time, "\n"])
        | otherwise =
            CBuf.addText (binding `B.append` "\n")
    processKeyDownEvent
        | (cls^.csKeyDest) == Constants.keyMessage = message key
        | (cls^.csKeyDest) == Constants.keyMenu = Menu.keyDown key
        | (cls^.csKeyDest) == Constants.keyGame || (cls^.csKeyDest) == Constants.keyConsole = console key
        | otherwise = Com.fatalError "Bad cls.key_dest"

processEscapeKey :: ClientStateT -> ClientStaticT -> Int -> Quake ()
processEscapeKey cl cls key
    | (cl^.csFrame.fPlayerState.psStats) UV.! Constants.statLayouts /= 0 && (cls^.csKeyDest) == Constants.keyGame =
        CBuf.addText "cmd putaway\n"
    | (cls^.csKeyDest) == Constants.keyMessage = message key
    | (cls^.csKeyDest) == Constants.keyMenu = Menu.keyDown key
    | (cls^.csKeyDest) == Constants.keyGame || (cls^.csKeyDest) == Constants.keyConsole = runXCommandT Menu.menuMainF
    | otherwise = Com.fatalError "Bad cls.key_dest"

clearStates :: Quake ()
clearStates = error "Key.clearStates" -- TODO

clearTyping :: Quake ()
clearTyping = do
    editLine <- use (globals.gEditLine)
    globals.gKeyLines.ix editLine .= B.pack [93, 0] -- clear any typing
    globals.gKeyLinePos .= 1

message :: Int -> Quake ()
message key
    | key `elem` [kEnter, kKpEnter] = do
        chatTeam <- use (globals.gChatTeam)
        CBuf.addText (if chatTeam then "say_team \"" else "say \"")
        chatBuffer <- use (globals.gChatBuffer)
        CBuf.addText chatBuffer
        CBuf.addText "\"\n"
        globals.gCls.csKeyDest .= Constants.keyGame
        globals.gChatBuffer .= B.empty
    | key == kEscape = do
        globals.gCls.csKeyDest .= Constants.keyGame
        globals.gChatBuffer .= B.empty
    | key < 32 || key > 127 =
        return () -- non printable
    | key == kBackspace = do
        chatBuffer <- use (globals.gChatBuffer)
        globals.gChatBuffer .= (if B.length chatBuffer > 2 then B.init chatBuffer else B.empty)
    | otherwise = do
        chatBuffer <- use (globals.gChatBuffer)
        unless (B.length chatBuffer > Constants.maxCmdLine) $
            globals.gChatBuffer %= (\v -> v `B.append` encode key) -- IMPROVE?

-- Interactive line editing and console scrollback
console :: Int -> Quake ()
console = error "Key.console" -- TODO