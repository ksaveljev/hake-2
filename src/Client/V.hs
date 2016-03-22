module Client.V
  ( initialize
  , renderView
  ) where

import qualified Client.CLEnts as CLEnts
import           Client.ClientStateT
import           Client.ClientStaticT
import           Client.FrameT
import           Client.RefDefT
import           Client.RefExportT
import qualified Client.SCRShared as SCR
import           Client.VRectT
import qualified Constants
import qualified Game.Cmd as Cmd
import           Game.CVarT
import           Game.PlayerStateT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QuakeState
import           Render.Renderer
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)
import qualified Util.Math3D as Math3D

import           Control.Applicative (liftA2, liftA3)
import           Control.Lens (use, (^.), (.=), (+=), (%=), (&), (.~), (+~))
import           Control.Monad (when, unless, join)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Linear (V3(..), V4(..), _x, _y, _z)
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
  printPos =<< use (globals.gCl.csRefDef)

printPos :: RefDefT -> Quake ()
printPos refDef = Com.printf (BC.pack line)
  where line = printf "(%i %i %i) : %i\n" orgX orgY orgZ angleY
        orgX = truncate (refDef^.rdViewOrg._x) :: Int
        orgY = truncate (refDef^.rdViewOrg._y) :: Int
        orgZ = truncate (refDef^.rdViewOrg._z) :: Int
        angleY = truncate (refDef^.rdViewAngles._y) :: Int

renderView :: Float -> Quake ()
renderView stereoSeparation =
  do skip <- shouldSkip
     unless skip $
       do checkTimeDemo =<< timeDemoCVar
          join (liftA3 (doRenderView stereoSeparation) frameValid forceRefDef pausedCVar)
          undefined -- TODO
          undefined -- TODO
          undefined -- TODO
          undefined -- TODO
          undefined -- TODO
  where frameValid = use (globals.gCl.csFrame.fValid)
        forceRefDef = use (globals.gCl.csForceRefDef)
{-
      Just renderer <- use $ globals.re
      use (globals.cl.csRefDef) >>= \rd ->
        (renderer^.rRefExport.reRenderFrame) rd

      clStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0) $
          Com.printf $ "ent: ??? lt: ??? part: ???" -- TODO: add info here!

      logFile <- use $ globals.logStatsFile
      logStatsCVar >>= \c ->
        when ((c^.cvValue) /= 0 && isJust logFile) $
          io (putStrLn "V.renderView: implement me!")

      finishRenderView
-}
          
shouldSkip :: Quake Bool
shouldSkip =
  do state <- use (globals.gCls.csState)
     refreshPrepped <- use (globals.gCl.csRefreshPrepped)
     return (state /= Constants.caActive || not refreshPrepped)
     
checkTimeDemo :: CVarT -> Quake ()
checkTimeDemo timeDemo
  | (timeDemo^.cvValue) /= 0 =
      do timeDemoStart <- use (globals.gCl.csTimeDemoStart)
         when (timeDemoStart == 0) $
           do ms <- Timer.milliseconds
              globals.gCl.csTimeDemoStart .= ms
         globals.gCl.csTimeDemoFrames += 1
  | otherwise = return ()

doRenderView :: Float -> Bool -> Bool -> CVarT -> Quake ()
doRenderView stereoSeparation frameValid forceRefDef paused
  | frameValid && (forceRefDef || (paused^.cvValue) == 0) =
      do globals.gCl.csForceRefDef .= False
         clearScene
         CLEnts.addEntities
         checkNonZeroVar testParticles =<< clTestParticlesCVar
         checkNonZeroVar testEntities =<< clTestParticlesCVar
         checkNonZeroVar testLights =<< clTestLightsCVar
         checkNonZeroVar (globals.gCl.csRefDef.rdBlend .= V4 1.0 0.5 0.25 0.5) =<< clTestBlendCVar
         checkStereoSeparation stereoSeparation
         join (liftA2 updateViewOrg (use (globals.gScrVRect)) (use (globals.gCl)))
         checkZeroVar (vGlobals.vgNumEntities .= 0) =<< clAddEntitiesCVar
         checkZeroVar (vGlobals.vgNumParticles .= 0) =<< clAddParticlesCVar
         checkZeroVar (vGlobals.vgNumDLights .= 0) =<< clAddLightsCVar
         checkZeroVar (globals.gCl.csRefDef.rdBlend .= V4 0 0 0 0) =<< clAddBlendCVar
         copyValue (vGlobals.vgNumEntities) (globals.gCl.csRefDef.rdNumEntities)
         copyValue (vGlobals.vgNumParticles) (globals.gCl.csRefDef.rdNumParticles)
         copyValue (vGlobals.vgNumDLights) (globals.gCl.csRefDef.rdNumDLights)
         copyValue (vGlobals.vgEntities) (globals.gCl.csRefDef.rdEntities)
         copyValue (vGlobals.vgDLights) (globals.gCl.csRefDef.rdDLights)
         copyValue (vGlobals.vgLightStyles) (globals.gCl.csRefDef.rdLightStyles)
         copyValue (globals.gCl.csFrame.fPlayerState.psRDFlags) (globals.gCl.csRefDef.rdRdFlags)
         undefined -- TODO
  | otherwise = return ()
  where checkNonZeroVar f var = when ((var^.cvValue) /= 0) f
        checkZeroVar f var = when ((var^.cvValue) == 0) f
        copyValue aLens bLens =
          do v <- use aLens
             bLens .= v

checkStereoSeparation :: Float -> Quake ()
checkStereoSeparation stereoSeparation
  | stereoSeparation /= 0 =
      do vright <- use (globals.gCl.csVRight)
         globals.gCl.csRefDef.rdViewOrg += fmap (* stereoSeparation) vright
  | otherwise = return ()

updateViewOrg :: VRectT -> ClientStateT -> Quake ()
updateViewOrg vrect cl =
  globals.gCl.csRefDef %= (\v -> v & rdViewOrg +~ (V3 (1.0 / 16) (1.0 / 16) (1.0 / 16))
                                   & rdX .~ (vrect^.vrX)
                                   & rdY .~ (vrect^.vrY)
                                   & rdWidth .~ (vrect^.vrWidth)
                                   & rdHeight .~ (vrect^.vrHeight)
                                   & rdFovY .~ Math3D.calcFov (cl^.csRefDef.rdFovX) (fromIntegral (vrect^.vrWidth)) (fromIntegral (vrect^.vrHeight))
                                   & rdTime .~ (fromIntegral (cl^.csTime)) * 0.001
                                   & rdAreaBits .~ (cl^.csFrame.fAreaBits))

finishRenderView :: Quake ()
finishRenderView =
  do vrect <- use (globals.gScrVRect)
     SCR.addDirtyPoint (vrect^.vrX) (vrect^.vrY)
     SCR.addDirtyPoint ((vrect^.vrX) + (vrect^.vrWidth) - 1) ((vrect^.vrY) + (vrect^.vrHeight) - 1)
     SCR.drawCrosshair

clearScene :: Quake ()
clearScene =
  do vGlobals.vgNumDLights .= 0
     vGlobals.vgNumEntities .= 0
     vGlobals.vgNumParticles .= 0

testParticles :: Quake ()
testParticles = error "V.testParticles" -- TODO

testEntities :: Quake ()
testEntities = error "V.testEntities" -- TODO

testLights :: Quake ()
testLights = error "V.testLights" -- TODO