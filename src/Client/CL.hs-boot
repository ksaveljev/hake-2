module Client.CL
    ( clearState
    , dropClient
    , fixUpGender
    , frame
    , initialize
    , pingServersF
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
pingServersF :: XCommandT
quitF :: XCommandT
shutdown :: Quake ()
writeConfiguration :: Quake ()
writeDemoMessage :: Quake ()