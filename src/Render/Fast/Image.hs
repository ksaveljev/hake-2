module Render.Fast.Image
  ( glShutdownImages
  , rRegisterSkin
  ) where

import           QuakeIOState
import           QuakeState
import           Render.ImageT
import           Types

import           Control.Lens (use, (^.))
import qualified Data.ByteString as B
import           Data.IORef (IORef)
import qualified Data.Vector.Mutable as MV
import           Foreign.Marshal.Utils (with)
import qualified Graphics.GL as GL

rRegisterSkin :: B.ByteString -> Quake (Maybe (IORef ImageT))
rRegisterSkin = error "Image.rRegisterSkin" -- TODO

glShutdownImages :: Quake ()
glShutdownImages =
  do numTextures <- use (fastRenderAPIGlobals.frNumGLTextures)
     request $
       do textures <- use frGLTextures
          clearTextures textures 0 numTextures

clearTextures :: MV.IOVector ImageT -> Int -> Int -> QuakeIO ()
clearTextures textures idx maxIdx
  | idx >= maxIdx = return ()
  | otherwise =
      do image <- MV.read textures idx
         clearWithRegSeq (image^.iTexNum) (image^.iRegistrationSequence)
         clearTextures textures (idx + 1) maxIdx
  where clearWithRegSeq _ 0 = return ()
        clearWithRegSeq texNum _ = io $
          with (fromIntegral texNum) $ \ptr ->
            do GL.glDeleteTextures 1 ptr
               MV.write textures idx (newImageT idx)