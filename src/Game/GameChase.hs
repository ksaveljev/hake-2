{-# LANGUAGE OverloadedStrings #-}
module Game.GameChase where

import Control.Lens (use, (^.), (.=), zoom, ix, preuse)
import Control.Monad (unless, liftM)

import Quake
import QuakeState
import CVarVariables

getChaseTarget :: EdictReference -> Quake ()
getChaseTarget edictRef = do
    maxClientsValue <- liftM (^.cvValue) maxClientsCVar
    done <- findChaseTarget 1 (truncate maxClientsValue)

    unless done $ do
      centerPrintf <- use $ gameBaseGlobals.gbGameImport.giCenterPrintf
      centerPrintf edictRef "No other players to chase."

  where findChaseTarget :: Int -> Int -> Quake Bool
        findChaseTarget idx maxIdx
          | idx > maxIdx = return False
          | otherwise = do
              let otherRef = newEdictReference idx
              other <- readEdictT otherRef
              let Just (GClientReference gClientIdx) = other^.eClient
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

              if (other^.eInUse) && not (gClient^.gcResp.crSpectator)
                then do
                  zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                    gcChaseTarget .= Just otherRef
                    gcUpdateChase .= True

                  updateChaseCam edictRef
                  return True

                else
                  findChaseTarget (idx + 1) maxIdx

updateChaseCam :: EdictReference -> Quake ()
updateChaseCam _ = do
    io (putStrLn "GameChase.updateChaseCam") >> undefined -- TODO
