{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.RenderAPIConstants where

import qualified Data.ByteString as B

import Render.OpenGL.QGLConstants

glColorIndex8Ext = glColorIndex :: Int
refVersion = "GL 0.01" :: B.ByteString

pitch = 0 :: Int -- up / down
yaw   = 1 :: Int -- left / right
roll  = 2 :: Int -- fall over

{-
 - skins will be outline flood filled and mip mapped pics and sprites with
 - alpha will be outline flood filled pic won't be mip mapped
 - 
 - model skin sprite frame wall texture pic
 -}
-- enum imagetype_t
itSkin   = 0 :: Int
itSprite = 1 :: Int
itWall   = 2 :: Int
itPic    = 3 :: Int
itSky    = 4 :: Int

-- enum modtype_t
modBad    = 0 :: Int
modBrush  = 1 :: Int
modSprite = 2 :: Int
modAlias  = 3 :: Int
texNumLightmaps = 1024 :: Int
texNumScraps    = 1152 :: Int
texNumImages    = 1153 :: Int
maxGLTextures   = 1024 :: Int
maxLBMHeight    =  480 :: Int
backfaceEpsilon = 0.01 :: Float

-- GL config stuff
glRendererVoodoo     = 0x00000001 :: Int
glRendererVoodoo2    = 0x00000002 :: Int
glRendererVoodooRush = 0x00000004 :: Int
glRendererBanshee    = 0x00000008 :: Int
glRenderer3DFX       = 0x0000000F :: Int
glRendererPCX1       = 0x00000010 :: Int
glRendererPCX2       = 0x00000020 :: Int
glRendererPMX        = 0x00000040 :: Int
glRendererPowerVR    = 0x00000070 :: Int
glRendererPerMedia2  = 0x00000100 :: Int
glRendererGlintMX    = 0x00000200 :: Int
glRendererGlintTX    = 0x00000400 :: Int
glRenderer3DLabsMisc = 0x00000800 :: Int
glRenderer3DLabs     = 0x00000F00 :: Int
glRendererRealizm    = 0x00001000 :: Int
glRendererRealizm2   = 0x00002000 :: Int
glRendererIntergraph = 0x00003000 :: Int
glRenderer3DPro      = 0x00004000 :: Int
glRendererReal3D     = 0x00008000 :: Int
glRendererRiva128    = 0x00010000 :: Int
glRendererDYPIC      = 0x00020000 :: Int
glRendererV1000      = 0x00040000 :: Int
glRendererV2100      = 0x00080000 :: Int
glRendererV2200      = 0x00100000 :: Int
glRendererRendition  = 0x001C0000 :: Int
glRendererO2         = 0x00100000 :: Int
glRendererImpact     = 0x00200000 :: Int
glRendererRE         = 0x00400000 :: Int
glRendererIR         = 0x00800000 :: Int
glRendererSGI        = 0x00F00000 :: Int
glRendererMCD        = 0x01000000 :: Int
glRendererOther      = 0x80000000 :: Int

-- enum rserr_t
rsErrOk                = 0 :: Int
rsErrInvalidFullscreen = 1 :: Int
rsErrInvalidMode       = 2 :: Int
rsErrUnknown           = 3 :: Int
