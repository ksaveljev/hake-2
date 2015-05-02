{-# LANGUAGE OverloadedStrings #-}
module Client.CLParse where

import Control.Lens (use, (^.))
import Control.Monad (when, liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CL as CL
import qualified Client.CLView as CLView
import qualified QCommon.Com as Com
import qualified QCommon.MSG as MSG

downloadF :: XCommandT
downloadF = io (putStrLn "CLParse.downloadF") >> undefined -- TODO

showNet :: B.ByteString -> Quake ()
showNet str = do
    showNetValue <- liftM (^.cvValue) clShowNetCVar
    when (showNetValue >= 2) $ do
      readCount <- use $ globals.netMessage.sbReadCount
      Com.printf $ BC.pack (show (readCount - 1)) `B.append` ":" `B.append` str `B.append` "\n"

parseServerMessage :: Quake ()
parseServerMessage = do
    parseMessage

    CLView.addNetGraph

    -- we don't know if it is ok to save a demo message until
    -- after we have parsed the frame
    demoRecording <- use $ globals.cls.csDemoRecording
    demoWaiting <- use $ globals.cls.csDemoWaiting
    when (demoRecording && not demoWaiting) $
      CL.writeDemoMessage

  where parseMessage :: Quake ()
        parseMessage = do
          nm <- use $ globals.netMessage

          if (nm^.sbReadCount) > (nm^.sbCurSize)
            then Com.comError Constants.errFatal "CL_ParseServerMessage: Bad server message:"
            else do
              cmd <- MSG.readByte (globals.netMessage)

              if cmd == -1
                then showNet "END OF MESSAGE"
                else do
                  showNetValue <- liftM (^.cvValue) clShowNetCVar

                  when (showNetValue >= 2) $ do
                    io (putStrLn "CLParse.parseServerMessage#parseMessage") >> undefined -- TODO

                  io (putStrLn "CLParse.parseServerMessage#parseMessage") >> undefined -- TODO
