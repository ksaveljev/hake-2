{-# LANGUAGE Rank2Types #-}
module Client.CLInput
  ( initializeInput
  ) where

import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar
import           QuakeState
import qualified Sys.IN as IN
import           Types
import qualified Util.Lib as Lib

import           Control.Lens (Lens', (.=))
import           Control.Monad (void)
import qualified Data.ByteString as B

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("centerview", Just IN.centerView)
  , ("+moveup", Just upDown)
  , ("-moveup", Just upUp)
  , ("+movedown", Just downDown)
  , ("-movedown", Just downUp)
  , ("+left", Just leftDown)
  , ("-left", Just leftUp)
  , ("+right", Just rightDown)
  , ("-right", Just rightUp)
  , ("+forward", Just forwardDown)
  , ("-forward", Just forwardUp)
  , ("+back", Just backDown)
  , ("-back", Just backUp)
  , ("+lookup", Just lookUpDown)
  , ("-lookup", Just lookUpUp)
  , ("+lookdown", Just lookDownDown)
  , ("-lookdown", Just lookDownUp)
  , ("+strafe", Just strafeDown)
  , ("-strafe", Just strafeUp)
  , ("+moveleft", Just moveLeftDown)
  , ("-moveleft", Just moveLeftUp)
  , ("+moveright", Just moveRightDown)
  , ("-moveright", Just moveRightUp)
  , ("+speed", Just speedDown)
  , ("-speed", Just speedUp)
  , ("+attack", Just attackDown)
  , ("-attack", Just attackUp)
  , ("+use", Just useDown)
  , ("-use", Just useUp)
  , ("impulse", Just impulse)
  , ("+klook", Just kLookDown)
  , ("-klook", Just kLookUp)
  ]

initializeInput :: Quake ()
initializeInput =
  do Cmd.addInitialCommands initialCommands
     void (CVar.get "cl_nodelta" "0" 0)

upDown :: XCommandT
upDown = XCommandT "CLInput.upDown" (inputKeyDown (clientGlobals.cgInUp))

upUp :: XCommandT
upUp = XCommandT "CLInput.upUp" (inputKeyUp (clientGlobals.cgInUp))

downDown :: XCommandT
downDown = XCommandT "CLInput.downDown" (inputKeyDown (clientGlobals.cgInDown))

downUp :: XCommandT
downUp = XCommandT "CLInput.downUp" (inputKeyUp (clientGlobals.cgInDown))

leftDown :: XCommandT
leftDown = XCommandT "CLInput.leftDown" (inputKeyDown (clientGlobals.cgInLeft))

leftUp :: XCommandT
leftUp = XCommandT "CLInput.leftUp" (inputKeyUp (clientGlobals.cgInLeft))

rightDown :: XCommandT
rightDown = XCommandT "CLInput.rightDown" (inputKeyDown (clientGlobals.cgInRight))

rightUp :: XCommandT
rightUp = XCommandT "CLInput.rightUp" (inputKeyUp (clientGlobals.cgInRight))

forwardDown :: XCommandT
forwardDown = XCommandT "CLInput.forwardDown" (inputKeyDown (clientGlobals.cgInForward))

forwardUp :: XCommandT
forwardUp = XCommandT "CLInput.forwardUp" (inputKeyUp (clientGlobals.cgInForward))

backDown :: XCommandT
backDown = XCommandT "CLInput.backDown" (inputKeyDown (clientGlobals.cgInBack))

backUp :: XCommandT
backUp = XCommandT "CLInput.backUp" (inputKeyUp (clientGlobals.cgInBack))

lookUpDown :: XCommandT
lookUpDown = XCommandT "CLInput.lookUpDown" (inputKeyDown (clientGlobals.cgInLookUp))

lookUpUp :: XCommandT
lookUpUp = XCommandT "CLInput.lookUpUp" (inputKeyUp (clientGlobals.cgInLookUp))

lookDownDown :: XCommandT
lookDownDown = XCommandT "CLInput.lookDownDown" (inputKeyDown (clientGlobals.cgInLookDown))

lookDownUp :: XCommandT
lookDownUp = XCommandT "CLInput.lookDownUp" (inputKeyUp (clientGlobals.cgInLookDown))

strafeDown :: XCommandT
strafeDown = XCommandT "CLInput.strafeDown" (inputKeyDown (clientGlobals.cgInStrafe))

strafeUp :: XCommandT
strafeUp = XCommandT "CLInput.strafeUp" (inputKeyUp (clientGlobals.cgInStrafe))

moveLeftDown :: XCommandT
moveLeftDown = XCommandT "CLInput.moveLeftDown" (inputKeyDown (clientGlobals.cgInMoveLeft))

moveLeftUp :: XCommandT
moveLeftUp = XCommandT "CLInput.moveLeftUp" (inputKeyUp (clientGlobals.cgInMoveLeft))

moveRightDown :: XCommandT
moveRightDown = XCommandT "CLInput.moveRightDown" (inputKeyDown (clientGlobals.cgInMoveRight))

moveRightUp :: XCommandT
moveRightUp = XCommandT "CLInput.moveRightUp" (inputKeyUp (clientGlobals.cgInMoveRight))

speedDown :: XCommandT
speedDown = XCommandT "CLInput.speedDown" (inputKeyDown (clientGlobals.cgInSpeed))

speedUp :: XCommandT
speedUp = XCommandT "CLInput.speedUp" (inputKeyUp (clientGlobals.cgInSpeed))

attackDown :: XCommandT
attackDown = XCommandT "CLInput.attackDown" (inputKeyDown (clientGlobals.cgInAttack))

attackUp :: XCommandT
attackUp = XCommandT "CLInput.attackUp" (inputKeyUp (clientGlobals.cgInAttack))

useDown :: XCommandT
useDown = XCommandT "CLInput.useDown" (inputKeyDown (clientGlobals.cgInUse))

useUp :: XCommandT
useUp = XCommandT "CLInput.useUp" (inputKeyUp (clientGlobals.cgInUse))

impulse :: XCommandT
impulse = XCommandT "CLInput.impulse" $
  do arg <- Cmd.argv 1
     clientGlobals.cgInImpulse .= Lib.atoi arg

kLookDown :: XCommandT
kLookDown = XCommandT "CLInput.kLookDown" (inputKeyDown (clientGlobals.cgInKLook))

kLookUp :: XCommandT
kLookUp = XCommandT "CLInput.kLookUp" (inputKeyUp (clientGlobals.cgInKLook))

inputKeyDown :: Lens' QuakeState KButtonT -> Quake ()
inputKeyDown = error "CLInput.inputKeyDown" -- TODO

inputKeyUp :: Lens' QuakeState KButtonT -> Quake ()
inputKeyUp = error "CLInput.inputKeyUp" -- TODO