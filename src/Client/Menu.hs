module Client.Menu where

import Control.Lens (use, preuse, ix, (.=))

import Quake
import QuakeState

init :: Quake ()
init = io (putStrLn "Menu.init") >> undefined -- TODO

-- TODO: IMPROVE: instead of Int use "Reference" newtype ?
addItem :: Int -> Int -> Quake ()
addItem _ _ = io (putStrLn "Menu.addItem") >> undefined -- TODO

-- TODO: IMPROVE: instead of Int use "Reference" newtype ?
center :: Int -> Quake ()
center menuIdx = do
    Just height <- preuse $ menuGlobals.mgMenuFrameworks.ix menuIdx.mfY
    h <- use $ globals.vidDef.vdHeight

    menuGlobals.mgMenuFrameworks.ix menuIdx.mfY .= (h - (height + 10)) `div` 2
