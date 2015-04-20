module Client.Menu where

import Quake

init :: Quake ()
init = io (putStrLn "Menu.init") >> undefined -- TODO

-- TODO: IMPROVE: instead of Int use "Reference" newtype ?
addItem :: Int -> Int -> Quake ()
addItem _ _ = io (putStrLn "Menu.addItem") >> undefined -- TODO

-- TODO: IMPROVE: instead of Int use "Reference" newtype ?
center :: Int -> Quake ()
center _ = io (putStrLn "Menu.center") >> undefined -- TODO
