module Client.V
  ( initialize
  ) where

import           Client.ClientStateT
import           Client.RefDefT
import           Client.RefExportT
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QuakeState
import           Render.Renderer
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (.=), (+=), (%=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Linear (_x, _y, _z)
import           Text.Printf (printf)

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
  [ ("gun_next", Just gunNextF)
  , ("gun_prev", Just gunPrevF)
  , ("gun_model", Just gunModelF)
  , ("viewpos", Just viewPosF)
  ]

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("crosshair", "0", Constants.cvarArchive) , ("cl_testblend", "0", 0)
  , ("cl_testparticles", "0", 0) , ("cl_testentities", "0", 0)
  , ("cl_testlights", "0", 0) , ("cl_stats", "0", 0)
  ]

initialize :: Quake ()
initialize =
  do Cmd.addInitialCommands initialCommands
     CVar.initializeCVars initialCVars

gunNextF :: XCommandT
gunNextF = XCommandT "V.gunNextF" $
  do globals.gGunFrame += 1
     gunFrame <- use (globals.gGunFrame)
     Com.printf (B.concat ["frame ", encode gunFrame, "\n"])

gunPrevF :: XCommandT
gunPrevF = XCommandT "V.gunPrevF" $
  do globals.gGunFrame %= \v -> max 0 (v - 1)
     gunFrame <- use $ globals.gGunFrame
     Com.printf (B.concat ["frame ", encode gunFrame, "\n"])

gunModelF :: XCommandT
gunModelF = XCommandT "V.gunModelF" $
  setGunModel =<< Cmd.argc
  where setGunModel c
          | c /= 2 = globals.gGunModel .= Nothing
          | otherwise =
              do arg <- Cmd.argv 1
                 renderer <- use (globals.gRenderer)
                 maybe rendererError (registerModel arg) renderer
        rendererError = Com.fatalError "V.gunModelF globals.gRenderer is Nothing"

registerModel :: B.ByteString -> Renderer -> Quake ()
registerModel name renderer =
  do model <- (renderer^.rRefExport.reRegisterModel) modelName
     globals.gGunModel .= model
  where modelName = B.concat ["models/", name, "/tris.md2"]

viewPosF :: XCommandT
viewPosF = XCommandT "V.viewPosF" $
  do refDef <- use (globals.gCl.csRefDef)
     printPos refDef

printPos :: RefDefT -> Quake ()
printPos refDef = Com.printf (BC.pack line)
  where line = printf "(%i %i %i) : %i\n" orgX orgY orgZ angleY
        orgX = truncate (refDef^.rdViewOrg._x) :: Int
        orgY = truncate (refDef^.rdViewOrg._y) :: Int
        orgZ = truncate (refDef^.rdViewOrg._z) :: Int
        angleY = truncate (refDef^.rdViewAngles._y) :: Int