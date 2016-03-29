module Client.CL
  ( dropClient
  , fixUpGender
  , frame
  , initialize
  , quitF
  , shutdown
  , writeConfiguration
  ) where

import Types

dropClient :: Quake ()
fixUpGender :: Quake ()
frame :: Int -> Quake ()
initialize :: Quake ()
quitF :: XCommandT
shutdown :: Quake ()
writeConfiguration :: Quake ()