{-# LANGUAGE OverloadedStrings #-}
module Client.CLInput where

import Control.Monad (void)

import Quake
import QCommon.XCommandT
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import qualified Sys.IN as IN

initInput :: Quake()
initInput = do
    Cmd.addCommand "centerview" (Just IN.centerView)
    Cmd.addCommand "+moveup" (Just upDown)
    Cmd.addCommand "-moveup" (Just upUp)
    Cmd.addCommand "+movedown" (Just downDown)
    Cmd.addCommand "-movedown" (Just downUp)
    Cmd.addCommand "+left" (Just leftDown)
    Cmd.addCommand "-left" (Just leftUp)
    Cmd.addCommand "+right" (Just rightDown)
    Cmd.addCommand "-right" (Just rightUp)
    Cmd.addCommand "+forward" (Just forwardDown)
    Cmd.addCommand "-forward" (Just forwardUp)
    Cmd.addCommand "+back" (Just backDown)
    Cmd.addCommand "-back" (Just backUp)
    Cmd.addCommand "+lookup" (Just lookUpDown)
    Cmd.addCommand "-lookup" (Just lookUpUp)
    Cmd.addCommand "+lookdown" (Just lookDownDown)
    Cmd.addCommand "-lookdown" (Just lookDownUp)
    Cmd.addCommand "+strafe" (Just strafeDown)
    Cmd.addCommand "-strafe" (Just strafeUp)
    Cmd.addCommand "+moveleft" (Just moveLeftDown)
    Cmd.addCommand "-moveleft" (Just moveLeftUp)
    Cmd.addCommand "+moveright" (Just moveRightDown)
    Cmd.addCommand "-moveright" (Just moveRightUp)
    Cmd.addCommand "+speed" (Just speedDown)
    Cmd.addCommand "-speed" (Just speedUp)
    Cmd.addCommand "+attack" (Just attackDown)
    Cmd.addCommand "-attack" (Just attackUp)
    Cmd.addCommand "+use" (Just useDown)
    Cmd.addCommand "-use" (Just useUp)
    Cmd.addCommand "impulse" (Just impulse)
    Cmd.addCommand "+klook" (Just kLookDown)
    Cmd.addCommand "-klook" (Just kLookUp)

    void $ CVar.get "cl_nodelta" "0" 0

upDown :: XCommandT
upDown = io (putStrLn "CLInput.upDown") >> undefined -- TODO

upUp :: XCommandT
upUp = io (putStrLn "CLInput.upUp") >> undefined -- TODO

downDown :: XCommandT
downDown = io (putStrLn "CLInput.downDown") >> undefined -- TODO

downUp :: XCommandT
downUp = io (putStrLn "CLInput.downUp") >> undefined -- TODO

leftDown :: XCommandT
leftDown = io (putStrLn "CLInput.leftDown") >> undefined -- TODO

leftUp :: XCommandT
leftUp = io (putStrLn "CLInput.leftUp") >> undefined -- TODO

rightDown :: XCommandT
rightDown = io (putStrLn "CLInput.rightDown") >> undefined -- TODO

rightUp :: XCommandT
rightUp = io (putStrLn "CLInput.rightUp") >> undefined -- TODO

forwardDown :: XCommandT
forwardDown = io (putStrLn "CLInput.forwardDown") >> undefined -- TODO

forwardUp :: XCommandT
forwardUp = io (putStrLn "CLInput.forwardUp") >> undefined -- TODO

backDown :: XCommandT
backDown = io (putStrLn "CLInput.backDown") >> undefined -- TODO

backUp :: XCommandT
backUp = io (putStrLn "CLInput.backUp") >> undefined -- TODO

lookUpDown :: XCommandT
lookUpDown = io (putStrLn "CLInput.lookUpDown") >> undefined -- TODO

lookUpUp :: XCommandT
lookUpUp = io (putStrLn "CLInput.lookUpUp") >> undefined -- TODO

lookDownDown :: XCommandT
lookDownDown = io (putStrLn "CLInput.lookDownDown") >> undefined -- TODO

lookDownUp :: XCommandT
lookDownUp = io (putStrLn "CLInput.lookDownUp") >> undefined -- TODO

strafeDown :: XCommandT
strafeDown = io (putStrLn "CLInput.strafeDown") >> undefined -- TODO

strafeUp :: XCommandT
strafeUp = io (putStrLn "CLInput.strafeUp") >> undefined -- TODO

moveLeftDown :: XCommandT
moveLeftDown = io (putStrLn "CLInput.moveLeftDown") >> undefined -- TODO

moveLeftUp :: XCommandT
moveLeftUp = io (putStrLn "CLInput.moveLeftUp") >> undefined -- TODO

moveRightDown :: XCommandT
moveRightDown = io (putStrLn "CLInput.moveRightDown") >> undefined -- TODO

moveRightUp :: XCommandT
moveRightUp = io (putStrLn "CLInput.moveRightUp") >> undefined -- TODO

speedDown :: XCommandT
speedDown = io (putStrLn "CLInput.speedDown") >> undefined -- TODO

speedUp :: XCommandT
speedUp = io (putStrLn "CLInput.speedUp") >> undefined -- TODO

attackDown :: XCommandT
attackDown = io (putStrLn "CLInput.attackDown") >> undefined -- TODO

attackUp :: XCommandT
attackUp = io (putStrLn "CLInput.attackUp") >> undefined -- TODO

useDown :: XCommandT
useDown = io (putStrLn "CLInput.useDown") >> undefined -- TODO

useUp :: XCommandT
useUp = io (putStrLn "CLInput.useUp") >> undefined -- TODO

impulse :: XCommandT
impulse = io (putStrLn "CLInput.impulse") >> undefined -- TODO

kLookDown :: XCommandT
kLookDown = io (putStrLn "CLInput.kLookDown") >> undefined -- TODO

kLookUp :: XCommandT
kLookUp = io (putStrLn "CLInput.kLookUp") >> undefined -- TODO
