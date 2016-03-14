module Game.GameBase
  ( runFrame
  , setMoveDir
  ) where

import           Game.EdictT
import           Game.EntityStateT
import           QuakeRef
import           Types
import qualified Util.Math3D as Math3D

import           Control.Lens ((^.), (&), (.~))
import           Linear (V3(..))

vecUp :: V3 Float
vecUp = V3 0 (-1) 0

moveDirUp :: V3 Float
moveDirUp = V3 0 0 1

vecDown :: V3 Float
vecDown = V3 0 (-2) 0

moveDirDown :: V3 Float
moveDirDown = V3 0 0 (-1)

runFrame :: Quake ()
runFrame = error "GameBase.runFrame" -- TODO

setMoveDir :: Ref EdictT -> EdictT -> Quake ()
setMoveDir edictRef edict =
  modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                              & eMoveDir .~ moveDir)
  where angles = edict^.eEntityState.esAngles
        moveDir | angles == vecUp = moveDirUp
                | angles == vecDown = moveDirDown
                | otherwise = let (forward, _, _) = Math3D.angleVectors angles True False False
                              in forward