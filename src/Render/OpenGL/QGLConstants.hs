{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Render.OpenGL.QGLConstants where

-- alpha functions
glNever    = 0x0200 :: Int
glLess     = 0x0201 :: Int
glEqual    = 0x0202 :: Int
glLequal   = 0x0203 :: Int
glGreater  = 0x0204 :: Int
glNotEqual = 0x0205 :: Int
glGEqual   = 0x0206 :: Int
glAlways   = 0x0207 :: Int

-- attribute masks
glDepthBufferBit   = 0x00000100 :: Int
glStencilBufferBit = 0x00000400 :: Int
glColorBufferBit   = 0x00004000 :: Int

-- begin modes
glPoints         = 0x0000 :: Int
glLines          = 0x0001 :: Int
glLineLoop       = 0x0002 :: Int
glLineStrip      = 0x0003 :: Int
glTriangles      = 0x0004 :: Int
glTriangleStrip  = 0x0005 :: Int
glTriangleFan    = 0x0006 :: Int
glQuads          = 0x0007 :: Int
glQuadStrip      = 0x0008 :: Int
glPolygon        = 0x0009 :: Int

-- blending factors
glZero             =      0 :: Int
glOne              =      1 :: Int
glSrcColor         = 0x0300 :: Int
glOneMinusSrcColor = 0x0301 :: Int
glSrcAlpha         = 0x0302 :: Int
glOneMinusSrcAlpha = 0x0303 :: Int
glDstAlpha         = 0x0304 :: Int
glOneMinusDstAlpha = 0x0305 :: Int

-- boolean
glTrue  = 1 :: Int
glFalse = 0 :: Int

-- data types
glByte           = 0x1400 :: Int
glUnsignedByte   = 0x1401 :: Int
glShort          = 0x1402 :: Int
glUnsignedShort  = 0x1403 :: Int
glInt            = 0x1404 :: Int
glUnsignedInt    = 0x1405 :: Int
glFloat          = 0x1406 :: Int

-- draw buffer modes
glFront        = 0x0404 :: Int
glBack         = 0x0405 :: Int
glFrontAndBack = 0x0408 :: Int

-- errors
glNoError         =      0 :: Int
glPointSmooth     = 0x0B10 :: Int
glCullFace        = 0x0B44 :: Int
glDepthTest       = 0x0B71 :: Int
glModelViewMatrix = 0x0BA6 :: Int
glAlphaTest       = 0x0BC0 :: Int
glBlend           = 0x0BE2 :: Int
glScissorTest     = 0x0C11 :: Int
glPackAlignment   = 0x0D05 :: Int
glTexture2D       = 0x0DE1 :: Int

-- hints
glPerspectiveCorrectionHint = 0x0C50 :: Int
glDontCare                  = 0x1100 :: Int
glFastest                   = 0x1101 :: Int
glNicest                    = 0x1102 :: Int

-- matrix modes
glModelView  = 0x1700 :: Int
glProjection = 0x1701 :: Int

-- pixel formats
glColorIndex     = 0x1900 :: Int
glRed            = 0x1903 :: Int
glGreen          = 0x1904 :: Int
glBlue           = 0x1905 :: Int
glAlpha          = 0x1906 :: Int
glRgb            = 0x1907 :: Int
glRgba           = 0x1908 :: Int
glLuminance      = 0x1909 :: Int
glLuminanceAlpha = 0x190A :: Int

-- polygon modes
glPoint = 0x1B00 :: Int
glLine  = 0x1B01 :: Int
glFill  = 0x1B02 :: Int

-- shading models
glFlat    = 0x1D00 :: Int
glSmooth  = 0x1D01 :: Int
glReplace = 0x1E01 :: Int

-- string names
glVendor     = 0x1F00 :: Int
glRenderer   = 0x1F01 :: Int
glVersion    = 0x1F02 :: Int
glExtensions = 0x1F03 :: Int

-- TextureEnvMode
glModulate = 0x2100 :: Int

-- TextureEnvParameter
glTextureEnvMode  = 0x2200 :: Int
glTextureEnvColor = 0x2201 :: Int

-- TextureEnvTarget
glTextureEnv           = 0x2300 :: Int
glNearest              = 0x2600 :: Int
glLinear               = 0x2601 :: Int
glNearestMipmapNearest = 0x2700 :: Int
glLinearMipmapNearest  = 0x2701 :: Int
glNearestMipmapLinear  = 0x2702 :: Int
glLinearMipmapLinear   = 0x2703 :: Int

-- TextureParameterName
glTextureMagFilter = 0x2800 :: Int
glTextureMinFilter = 0x2801 :: Int
glTextureWrapS     = 0x2802 :: Int
glTextureWrapT     = 0x2803 :: Int

-- TextureWrapMode
glClamp  = 0x2900 :: Int
glRepeat = 0x2901 :: Int

-- texture
glLuminance8 = 0x8040 :: Int
glIntensity8 = 0x804B :: Int
glR3G3B2     = 0x2A10 :: Int
glRGB4       = 0x804F :: Int
glRGB5       = 0x8050 :: Int
glRGB8       = 0x8051 :: Int
glRGBA2      = 0x8055 :: Int
glRGBA4      = 0x8056 :: Int
glRGB5A1     = 0x8057 :: Int
glRGBA8      = 0x8058 :: Int

-- vertex arrays
glVertexArray       = 0x8074 :: Int
glColorArray        = 0x8076 :: Int
glTextureCoordArray = 0x8078 :: Int
glT2fV3f            = 0x2A27 :: Int

-- OpenGL 1.2, 1.3 constants
glSharedTexturePaletteExt = 0x81FB :: Int
glTexture0                = 0x84C0 :: Int
glTexture1                = 0x84C1 :: Int
glTexture0ARB             = 0x84C0 :: Int
glTexture1ARB             = 0x84C1 :: Int
glBGR                     = 0x80E0 :: Int
glBGRA                    = 0x80E1 :: Int

-- point parameters
glPointSizeMinExt           = 0x8126 :: Int
glPointSizeMaxExt           = 0x8127 :: Int
glPointFadeThresholdSizeExt = 0x8128 :: Int
glDistanceAttenuationExt    = 0x8129 :: Int
