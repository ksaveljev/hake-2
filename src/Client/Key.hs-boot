module Client.Key
    ( clearStates
    , clearTyping
    , event
    , initialize
    , writeBindings
    ) where

import           System.IO (Handle)

import           Types

initialize :: Quake ()
writeBindings :: Handle -> Quake ()
event :: Int -> Bool -> Int -> Quake ()
clearStates :: Quake ()
clearTyping :: Quake ()