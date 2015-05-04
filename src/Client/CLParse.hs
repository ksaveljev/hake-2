{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
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
import qualified QCommon.CBuf as CBuf
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

                  if | cmd == Constants.svcNop -> return ()

                     | cmd == Constants.svcDisconnect -> Com.comError Constants.errDisconnect "Server disconnected\n"

                     | cmd == Constants.svcReconnect -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcReconnect") >> undefined -- TODO

                     | cmd == Constants.svcPrint -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcPrint") >> undefined -- TODO

                     | cmd == Constants.svcCenterPrint -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcCenterPrint") >> undefined -- TODO

                     | cmd == Constants.svcStuffText -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcStuffText") >> undefined -- TODO

                     | cmd == Constants.svcServerData -> do
                         CBuf.execute -- make sure any stuffed commands are done
                         parseServerData

                     | cmd == Constants.svcConfigString -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcConfigString") >> undefined -- TODO

                     | cmd == Constants.svcSound -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcSound") >> undefined -- TODO

                     | cmd == Constants.svcSpawnBaseline -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcSpawnBaseline") >> undefined -- TODO

                     | cmd == Constants.svcTempEntity -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcTempEntity") >> undefined -- TODO

                     | cmd == Constants.svcMuzzleFlash -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcMuzzleFlash") >> undefined -- TODO

                     | cmd == Constants.svcMuzzleFlash2 -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcMuzzleFlash2") >> undefined -- TODO

                     | cmd == Constants.svcDownload -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcDownload") >> undefined -- TODO

                     | cmd == Constants.svcFrame -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcFrame") >> undefined -- TODO

                     | cmd == Constants.svcInventory -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcInventory") >> undefined -- TODO

                     | cmd == Constants.svcLayout -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcLayout") >> undefined -- TODO

                     | cmd == Constants.svcPlayerInfo || cmd == Constants.svcPacketEntities || cmd == Constants.svcDeltaPacketEntities -> io (putStrLn "CLParse.parseServerMessage#parseMessage#svcFinalErrorSmth") >> undefined -- TODO

                     | otherwise -> io (putStrLn "CLParse.otherwise") >> undefined -- TODO

                  parseMessage

parseServerData :: Quake ()
parseServerData = io (putStrLn "CLParse.parseServerData") >> undefined -- TODO
