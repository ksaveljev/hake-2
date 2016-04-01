module Client.CL
  ( clearState
  , dropClient
  , fixUpGender
  , frame
  , initialize
  , quitF
  , shutdown
  , writeConfiguration
  , writeDemoMessage
  ) where

import Types

clearState :: Quake ()
dropClient :: Quake ()
fixUpGender :: Quake ()
frame :: Int -> Quake ()
initialize :: Quake ()
quitF :: XCommandT
shutdown :: Quake ()
writeConfiguration :: Quake ()
writeDemoMessage :: Quake ()