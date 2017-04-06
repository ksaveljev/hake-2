module Client.Console
    ( checkResize
    , clearNotify
    , drawAltString
    , drawConsole
    , drawNotify
    , drawString
    , initialize
    , printConsole
    , toggleConsoleF
    ) where

import           Control.Lens                 (preuse, use, ix, (.=), (^.))
import           Control.Monad                (void, unless)
import           Data.Bits                    (shiftR, shiftL, xor, (.&.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Char                    (ord)
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import           Data.Word                    (Word8)
import           System.IO                    (Handle)
import           Text.Printf                  (printf)

import           Client.ClientStaticT
import           Client.ConsoleT
import           Client.RefExportT
import qualified Client.SCRShared             as SCR
import           Client.VidDefT
import qualified Constants
import qualified Game.Cmd                     as Cmd
import qualified QCommon.CVar                 as CVar
import           QuakeState
import           Render.Renderer
import           Types
import           Util.Unsafe

import {-# SOURCE #-} qualified QCommon.Com as Com

checkResize :: VidDefT -> Quake ()
checkResize vidDef = do
    lineWidth <- use (globals.gCon.cLineWidth)
    proceedCheckResize width lineWidth
  where
    w = ((vidDef^.vdWidth) `shiftR` 3) - 2
    width = min w Constants.maxCmdLine

proceedCheckResize :: Int -> Int -> Quake ()
proceedCheckResize width lineWidth
    | width == lineWidth = return ()
    | width < 1 = do -- video hasn't been initialized yet
        globals.gCon.cLineWidth .= 38
        globals.gCon.cTotalLines .= Constants.conTextSize `div` 38
        emptyConsole
        updateConsoleValues
    | otherwise = do
        oldWidth <- use (globals.gCon.cLineWidth)
        oldTotalLines <- use (globals.gCon.cTotalLines)
        doResize width oldWidth oldTotalLines lineWidth
        updateConsoleValues
  where
    emptyConsole = do
        consoleBuffer <- use (globals.gCon.cText)
        io (consoleBuffer `MSV.set` ' ')
    updateConsoleValues = do
        totalLines <- use (globals.gCon.cTotalLines)
        globals.gCon.cCurrent .= totalLines - 1
        globals.gCon.cDisplay .= totalLines - 1

doResize :: Int -> Int -> Int -> Int -> Quake ()
doResize width oldWidth oldTotalLines lineWidth = do
    globals.gCon.cLineWidth .= width
    globals.gCon.cTotalLines .= totalLines
    currentLine <- use (globals.gCon.cCurrent)
    refillConsoleBuffer width oldWidth totalLines oldTotalLines currentLine lineWidth
    clearNotify
  where
    totalLines = Constants.conTextSize `div` width

refillConsoleBuffer :: Int -> Int -> Int -> Int -> Int -> Int -> Quake ()
refillConsoleBuffer width oldWidth totalLines oldTotalLines currentLine lineWidth = do
    consoleBuffer <- use (globals.gCon.cText)
    io $ do
        tbuf <- MSV.clone consoleBuffer
        fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth 0 0

fillInBuf :: MSV.IOVector Char -> MSV.IOVector Char -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth i j
    | i >= numLines = return ()
    | j >= numChars = fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth (i + 1) 0
    | otherwise = do
        let idx = (totalLines - 1 - i) * lineWidth + j
            idx2 = ((currentLine - i + oldTotalLines) `mod` oldTotalLines) * oldWidth + j
        tbuf `MSV.read` idx2 >>= MSV.write consoleBuffer idx
        fillInBuf consoleBuffer tbuf width oldWidth totalLines oldTotalLines currentLine lineWidth i (j + 1)
  where
    numLines = min totalLines oldTotalLines
    numChars = min width oldWidth

clearF :: XCommandT
clearF = XCommandT "Console.clearF" $
    error "Console.clearF" -- TODO

clearNotify :: Quake ()
clearNotify = globals.gCon.cTimes .= UV.replicate Constants.numConTimes 0

drawConsole :: Float -> VidDefT -> Quake ()
drawConsole frac vidDef = do
    renderer <- use (globals.gRenderer)
    unless (tmpLinesNum <= 0) $ do
        (renderer^.rRefExport.reDrawStretchPic) 0 (-height + linesNum) width height "conback"
        SCR.addDirtyPoint 0 0
        SCR.addDirtyPoint (width - 1) (linesNum - 1)
        drawVersion renderer version width linesNum 0 5
        globals.gCon.cVisLines .= linesNum
        console <- use (globals.gCon)
        (rows, y) <- calcRowsAndDrawArrows renderer console linesNum
        drawText renderer console (console^.cDisplay) y 0 rows
        checkDownload =<< use (globals.gCls.csDownload)
        drawInput renderer
  where
    width = vidDef^.vdWidth
    height = vidDef^.vdHeight
    tmpLinesNum = truncate ((fromIntegral height) * frac)
    linesNum | tmpLinesNum > height = height
             | otherwise = tmpLinesNum
    version = BC.pack (printf "v%4.2f" Constants.version)

drawText :: Renderer -> ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
drawText renderer console row y idx maxIdx
    | idx >= maxIdx || row < 0 || (console^.cCurrent) - row >= (console^.cTotalLines) = return ()
    | otherwise = do
        let text = mVectorToByteString 'a' (console^.cText)
        drawLine drawChar text first y 0 (console^.cLineWidth)
        drawText renderer console (row - 1) (y - 8) (idx + 1) maxIdx
  where
    first = (row `mod` (console^.cTotalLines)) * (console^.cLineWidth)
    drawChar = renderer^.rRefExport.reDrawChar

drawLine :: (Int -> Int -> Int -> Quake ()) -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawLine drawChar text first y idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        let ch = text `BC.index` (idx + first)
        drawChar ((idx + 1) `shiftL` 3) y $! (ord ch)
        drawLine drawChar text first y (idx + 1) maxIdx

-- IMPROVE: mapM_ or something
drawVersion :: Renderer -> B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
drawVersion renderer version width linesNum idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        (renderer^.rRefExport.reDrawChar) (width - 44 + idx * 8) (linesNum - 12) (128 + ord (BC.index version idx))
        drawVersion renderer version width linesNum (idx + 1) maxIdx

calcRowsAndDrawArrows :: Renderer -> ConsoleT -> Int -> Quake (Int, Int)
calcRowsAndDrawArrows renderer console linesNum
    | (console^.cDisplay) /= (console^.cCurrent) = do
        drawArrows renderer 0 (console^.cLineWidth) y
        return (rows - 1, y - 8)
    | otherwise = return (rows, y)
  where
    rows = (linesNum - 22) `shiftR` 3
    y = linesNum - 30
        
drawArrows :: Renderer -> Int -> Int -> Int -> Quake ()
drawArrows renderer x maxX y
    | x >= maxX = return ()
    | otherwise = do
        (renderer^.rRefExport.reDrawChar) ((x + 1) `shiftL` 3) y (ord '^')
        drawArrows renderer (x + 4) maxX y

checkDownload :: Maybe Handle -> Quake ()
checkDownload Nothing = return ()
checkDownload _ = error "Console.checkDownload" -- TODO

drawNotify :: Quake ()
drawNotify = do
    console <- use (globals.gCon)
    vidDefWidth <- use (globals.gVidDef.vdWidth)
    cls <- use (globals.gCls)
    io (putStrLn "Console.drawNotify... IMPLEMENT ME!") >> return () -- TODO
{-
    v <- draw cls' console 0 ((console^.cCurrent) - Constants.numConTimes + 1) (console^.cCurrent)

    v' <- if (cls'^.csKeyDest) == Constants.keyMessage
            then do
              chatTeam' <- use $ globals.chatTeam
              skip <- if chatTeam'
                        then do
                          drawString 8 v "say_team:"
                          return 11
                        else do
                          drawString 8 v "say:"
                          return 5

              s <- use $ globals.chatBuffer
              chatLen <- use $ globals.chatBufferLen
              let s' = if chatLen > (vidDefWidth `shiftR` 3) - (skip + 1)
                         then B.drop (chatLen - ((vidDefWidth `shiftR` 3) - (skip + 1))) s 
                         else s
              drawChat s' skip v 0 (B.length s')
              return (v + 8)
            else
              return v

    when (v' /= 0) $ do
      SCR.addDirtyPoint 0 0
      SCR.addDirtyPoint (vidDefWidth - 1) v'

  where draw :: ClientStaticT -> ConsoleT -> Int -> Int -> Int -> Quake Int
        draw cls' console v idx maxIdx
          | idx > maxIdx = return v
          | idx < 0 = draw cls' console v (idx + 1) maxIdx
          | otherwise = do
              let time = truncate ((console^.cTimes) UV.! (idx `mod` Constants.numConTimes)) :: Int

              if time == 0
                then draw cls' console v (idx + 1) maxIdx
                else do
                  let time' = (cls'^.csRealTime) - time
                  conNotifyTimeValue <- liftM (^.cvValue) conNotifyTimeCVar
                  if time' > truncate (conNotifyTimeValue * 1000)
                    then draw cls' console v (idx + 1) maxIdx
                    else do
                      let text = (idx `mod` (console^.cTotalLines)) * (console^.cLineWidth)
                      drawText console v text 0 (console^.cLineWidth)
                      draw cls' console (v + 8) (idx + 1) maxIdx

        drawText :: ConsoleT -> Int -> Int -> Int -> Int -> Quake ()
        drawText console v text idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just renderer <- use $ globals.re
              c <- io $ MSV.read (console^.cText) (text + idx)
              (renderer^.rRefExport.reDrawChar) ((idx + 1) `shiftL` 3) v (ord c)
              drawText console v text (idx + 1) maxIdx

        drawChat :: B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
        drawChat s skip v idx maxIdx
          | idx >= maxIdx = do
              realTime <- use $ globals.cls.csRealTime
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) ((idx + skip) `shiftL` 3) v (10 + ((realTime `shiftR` 8) .&. 1))
          | otherwise = do
              Just renderer <- use $ globals.re
              (renderer^.rRefExport.reDrawChar) ((idx + skip) `shiftL` 3) v (ord $ s `BC.index` idx)
              drawChat s skip v (idx + 1) maxIdx
              -}

drawInput :: Renderer -> Quake ()
drawInput renderer = do
    keyDest <- use (globals.gCls.csKeyDest)
    state <- use (globals.gCls.csState)
    unless (keyDest == Constants.keyMenu || (keyDest /= Constants.keyConsole && state == Constants.caActive)) $ do
        editLine <- use (globals.gEditLine)
        text <- preuse (globals.gKeyLines.ix editLine)
        linePos <- use (globals.gKeyLinePos)
        lineWidth <- use (globals.gCon.cLineWidth)
        realTime <- use (globals.gCls.csRealTime)
        visLines <- use (globals.gCon.cVisLines)
        proceedDrawInput renderer text linePos lineWidth realTime visLines

proceedDrawInput :: Renderer -> Maybe B.ByteString -> Int -> Int -> Int -> Int -> Quake ()
proceedDrawInput _ Nothing _ _ _ _ =
    Com.fatalError "Console.drawInput text is Nothing"
proceedDrawInput renderer (Just text) linePos lineWidth realTime visLines =
    drawInputLine (renderer^.rRefExport.reDrawChar) fullLine (visLines - 22) start lineWidth -- TODO: make sure start is here (jake2 bug with not using start?)
  where 
    cursorFrame = fromIntegral (10 + ((realTime `shiftR` 8) .&. 1)) :: Word8
    fullLine = text `B.append` B.unfoldr buildRemainder 0
    buildRemainder idx
        | idx == 0 = Just (cursorFrame, 1)
        | linePos + 1 + idx < lineWidth = Just (32, idx + 1)
        | otherwise = Nothing
    start | linePos >= lineWidth = 1 + linePos - lineWidth
          | otherwise = 0

-- TODO: mapM_ or something?
drawInputLine :: (Int -> Int -> Int -> Quake ()) -> B.ByteString -> Int -> Int -> Int -> Quake ()
drawInputLine drawChar text y idx maxIdx
    | idx >= maxIdx = return ()
    | otherwise = do
        drawChar ((idx + 1) `shiftL` 3) y (ord (BC.index text idx))
        drawInputLine drawChar text y (idx + 1) maxIdx

drawAltString :: Int -> Int -> B.ByteString -> Quake ()
drawAltString x y s = doDrawString x y (B.length s) (\idx -> (ord (s `BC.index` idx)) `xor` 0x80)

drawString :: Int -> Int -> B.ByteString -> Quake ()
drawString x y s = doDrawString x y (B.length s) (\idx -> ord (s `BC.index` idx))

doDrawString :: Int -> Int -> Int -> (Int -> Int) -> Quake ()
doDrawString x y maxIdx f = do
    renderer <- use (globals.gRenderer)
    draw x 0 renderer
  where
    draw x' idx renderer
        | idx >= maxIdx = return ()
        | otherwise = do
            (renderer^.rRefExport.reDrawChar) x' y (f idx)
            draw (x' + 8) (idx + 1) renderer

dumpF :: XCommandT
dumpF = XCommandT "Console.dumpF" $
    error "Console.dumpF" -- TODO

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
    [ ("toggleconsole", Just toggleConsoleF), ("togglechat", Just toggleChatF)
    , ("messagemode", Just messageModeF), ("messagemode2", Just messageMode2F)
    , ("clear", Just clearF), ("condump", Just dumpF) ]

initialize :: Quake ()
initialize = do
    globals.gCon.cLineWidth .= -1
    checkResize =<< use (globals.gVidDef)
    Com.printf "Console initialized.\n"
    void (CVar.get "con_notifytime" "3" 0)
    Cmd.addInitialCommands initialCommands
    globals.gCon.cInitialized .= True

messageModeF :: XCommandT
messageModeF = XCommandT "Console.messageModeF" $
    error "Console.messageModeF" -- TODO

messageMode2F :: XCommandT
messageMode2F = XCommandT "Console.messageMode2F" $
    error "Console.messageMode2F" -- TODO

toggleChatF :: XCommandT
toggleChatF = XCommandT "Console.toggleChatF" $
    error "Console.toggleChatF" -- TODO

toggleConsoleF :: XCommandT
toggleConsoleF = XCommandT "Console.toggleConsoleF" $
    error "Console.toggleConsoleF" -- TODO

printConsole :: B.ByteString -> Quake ()
printConsole = error "Console.print" -- TODO
