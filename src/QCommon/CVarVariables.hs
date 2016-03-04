{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module QCommon.CVarVariables where

import qualified QCommon.CVar as CVar

clStereoSeparationCVar       = CVar.getExisting "cl_stereo_separation"
clStereoCVar                 = CVar.getExisting "cl_stereo"
clAddBlendCVar               = CVar.getExisting "cl_blend"
clAddLightsCVar              = CVar.getExisting "cl_lights"
clAddParticlesCVar           = CVar.getExisting "cl_particles"
clAddEntitiesCVar            = CVar.getExisting "cl_entities"
clGunCVar                    = CVar.getExisting "cl_gun"
clFootstepsCVar              = CVar.getExisting "cl_footsteps"
clNoSkinsCVar                = CVar.getExisting "cl_noskins"
clAutoSkinsCVar              = CVar.getExisting "cl_autoskins"
clPredictCVar                = CVar.getExisting "cl_predict"
clMaxFPSCVar                 = CVar.getExisting "cl_maxfps"
clUpSpeedCVar                = CVar.getExisting "cl_upspeed"
clForwardSpeedCVar           = CVar.getExisting "cl_forwardspeed"
clSideSpeedCVar              = CVar.getExisting "cl_sidespeed"
clYawSpeedCVar               = CVar.getExisting "cl_yawspeed"
clPitchSpeedCVar             = CVar.getExisting "cl_pitchspeed"
clAngleSpeedkeyCVar          = CVar.getExisting "cl_anglespeedkey"
clRunCVar                    = CVar.getExisting "cl_run"
lookSpringCVar               = CVar.getExisting "lookspring"
lookStrafeCVar               = CVar.getExisting "lookstrafe"
mPitchCVar                   = CVar.getExisting "m_pitch"
mYawCVar                     = CVar.getExisting "m_yaw"
mForwardCVar                 = CVar.getExisting "m_forward"
mSideCVar                    = CVar.getExisting "m_side"
clShowNetCVar                = CVar.getExisting "cl_shownet"
clShowMissCVar               = CVar.getExisting "cl_showmiss"
clTimeoutCVar                = CVar.getExisting "cl_timeout"
showClampCVar              = CVar.getExisting "showclamp"
timeDemoCVar                 = CVar.getExisting "timedemo"
pausedCVar                   = CVar.getExisting "paused"
rconClientPasswordCVar       = CVar.getExisting "rcon_password"
rconAddressCVar              = CVar.getExisting "rcon_address"
clLightLevelCVar             = CVar.getExisting "r_lightlevel"
infoPasswordCVar             = CVar.getExisting "password"
infoSpectatorCVar            = CVar.getExisting "spectator"
nameCVar                     = CVar.getExisting "name"
skinCVar                     = CVar.getExisting "skin"
rateCVar                     = CVar.getExisting "rate"
msgCVar                      = CVar.getExisting "msg"
handCVar                     = CVar.getExisting "hand"
fovCVar                      = CVar.getExisting "fov"
genderCVar                   = CVar.getExisting "gender"
clVwepCVar                   = CVar.getExisting "cl_vwep"
conNotifyTimeCVar            = CVar.getExisting "con_notifytime"
noStdoutCVar                 = CVar.getExisting "nostdout"
hostSpeedsCVar               = CVar.getExisting "host_speeds"
logStatsCVar                 = CVar.getExisting "log_stats"
developerCVar                = CVar.getExisting "developer"
timeScaleCVar                = CVar.getExisting "timescale"
fixedTimeCVar                = CVar.getExisting "fixedtime"
logfileActiveCVar            = CVar.getExisting "logfile"
showTraceCVar                = CVar.getExisting "showtrace"
dedicatedCVar                = CVar.getExisting "dedicated"
mFilterCVar                  = CVar.getExisting "m_filter"
inMouseCVar                  = CVar.getExisting "in_mouse"
freeLookCVar                 = CVar.getExisting "freelook"
sensitivityCVar              = CVar.getExisting "sensitivity"
maxClientsCVar               = CVar.getExisting "maxclients"
hostnameCVar                 = CVar.getExisting "hostname"
timeoutCVar                  = CVar.getExisting "timeout"
zombieTimeCVar               = CVar.getExisting "zombietime"
svEnforceTimeCVar            = CVar.getExisting "sv_enforcetime"
allowDownloadCVar            = CVar.getExisting "allow_download"
allowDownloadPlayersCVar     = CVar.getExisting "allow_download_players"
allowDownloadModelsCVar      = CVar.getExisting "allow_download_models"
allowDownloadSoundsCVar      = CVar.getExisting "allow_download_sounds"
allowDownloadMapsCVar        = CVar.getExisting "allow_download_maps"
svNoReloadCVar               = CVar.getExisting "sv_noreload"
svAirAccelerateCVar          = CVar.getExisting "sv_airaccelerate"
publicServerCVar             = CVar.getExisting "public"
svReconnectLimitCVar         = CVar.getExisting "sv_reconnect_limit"
fsBaseDirCVar                = CVar.getExisting "basedir"
fsCdDirCVar                  = CVar.getExisting "cddir"
fsGameDirVarCVar             = CVar.getExisting "game"
maxEntitiesCVar              = CVar.getExisting "maxentities"
mapNoAreasCVar               = CVar.getExisting "map_noareas"
skillCVar                    = CVar.getExisting "skill"
coopCVar                     = CVar.getExisting "coop"
deathmatchCVar               = CVar.getExisting "deathmatch"
dmFlagsCVar                  = CVar.getExisting "dmflags"
svMaxVelocityCVar            = CVar.getExisting "sv_maxvelocity"
svRollAngleCVar              = CVar.getExisting "sv_rollangle"
svRollSpeedCVar              = CVar.getExisting "sv_rollspeed"
svGravityCVar                = CVar.getExisting "sv_gravity"
timeLimitCVar                = CVar.getExisting "timelimit"
fragLimitCVar                = CVar.getExisting "fraglimit"
spectatorPasswordCVar        = CVar.getExisting "spectator_password"
vidWidthCVar                 = CVar.getExisting "vid_width"
vidHeightCVar                = CVar.getExisting "vid_height"
vidRefCVar                   = CVar.getExisting "vid_ref"
vidFullScreenCVar            = CVar.getExisting "vid_fullscreen"
vidXPosCVar                  = CVar.getExisting "vid_xpos"
vidYPosCVar                  = CVar.getExisting "vid_ypos"
vidGammaCVar                 = CVar.getExisting "vid_gamma"
glModeCVar                   = CVar.getExisting "gl_mode"
glPicMipCVar                 = CVar.getExisting "gl_picmip"
glExtPalettedTextureCVar     = CVar.getExisting "gl_ext_palettedtexture"
glSwapIntervalCVar           = CVar.getExisting "gl_swapinterval"
glMonoLightMapCVar           = CVar.getExisting "gl_monolightmap"
gl3DLabsBrokenCVar           = CVar.getExisting "gl_3dlabs_broken"
glExtCompiledVertexArrayCVar = CVar.getExisting "gl_ext_compiled_vertex_array"
glExtPointParametersCVar     = CVar.getExisting "gl_ext_pointparameters"
glExtMultiTextureCVar        = CVar.getExisting "gl_ext_multitexture"
glTextureModeCVar            = CVar.getExisting "gl_texturemode"
glTextureAlphaModeCVar       = CVar.getExisting "gl_texturealphamode"
glTextureSolidModeCVar       = CVar.getExisting "gl_texturesolidmode"
glParticleAttACVar           = CVar.getExisting "gl_particle_att_a"
glParticleAttBCVar           = CVar.getExisting "gl_particle_att_b"
glParticleAttCCVar           = CVar.getExisting "gl_particle_att_c"
glParticleMinSizeCVar        = CVar.getExisting "gl_particle_min_size"
glParticleMaxSizeCVar        = CVar.getExisting "gl_particle_max_size"
glNoBindCVar                 = CVar.getExisting "gl_nobind"
glRoundDownCVar              = CVar.getExisting "gl_round_down"
glLogCVar                    = CVar.getExisting "gl_log"
glDrawBufferCVar             = CVar.getExisting "gl_drawbuffer"
glZTrickCVar                 = CVar.getExisting "gl_ztrick"
glClearCVar                  = CVar.getExisting "gl_clear"
glSkyMipCVar                 = CVar.getExisting "gl_skymip"
glFinishCVar                 = CVar.getExisting "gl_finish"
glFlashBlendCVar             = CVar.getExisting "gl_flashblend"
glCullCVar                   = CVar.getExisting "gl_cull"
glLightMapCVar               = CVar.getExisting "gl_lightmap"
glShowTrisCVar               = CVar.getExisting "gl_showtris"
glDynamicCVar                = CVar.getExisting "gl_dynamic"
glModulateCVar               = CVar.getExisting "gl_modulate"
glShadowsCVar                = CVar.getExisting "gl_shadows"
glPolyBlendCVar              = CVar.getExisting "gl_polyblend"
glDriverCVar                 = CVar.getExisting "gl_driver"
intensityCVar                = CVar.getExisting "intensity"
viewSizeCVar                 = CVar.getExisting "viewsize"
clNoDeltaCVar                = CVar.getExisting "cl_nodelta"
clTestParticlesCVar          = CVar.getExisting "cl_testparticles"
clTestEntitiesCVar           = CVar.getExisting "cl_testentities"
clTestLightsCVar             = CVar.getExisting "cl_testlights"
clTestBlendCVar              = CVar.getExisting "cl_testblend"
clStatsCVar                  = CVar.getExisting "cl_stats"
fpsCVar                      = CVar.getExisting "fps"
scrDrawAllCVar               = CVar.getExisting "scr_drawall"
scrShowPauseCVar             = CVar.getExisting "scr_showpause"
scrConSpeedCVar              = CVar.getExisting "scr_conspeed"
qportCVar                    = CVar.getExisting "qport"
showPacketsCVar              = CVar.getExisting "showpackets"
showDropCVar                 = CVar.getExisting "showdrop"
scrDebugGraphCVar            = CVar.getExisting "debuggraph"
scrTimeGraphCVar             = CVar.getExisting "timegraph"
crosshairCVar                = CVar.getExisting "crosshair"
runRollCVar                  = CVar.getExisting "run_roll"
runPitchCVar                 = CVar.getExisting "run_pitch"
bobPitchCVar                 = CVar.getExisting "bob_pitch"
bobRollCVar                  = CVar.getExisting "bob_roll"
bobUpCVar                    = CVar.getExisting "bob_up"
gunXCVar                     = CVar.getExisting "gun_x"
gunYCVar                     = CVar.getExisting "gun_y"
gunZCVar                     = CVar.getExisting "gun_z"
noRefreshCVar                = CVar.getExisting "r_norefresh"
speedsCVar                   = CVar.getExisting "r_speeds"
noVisCVar                    = CVar.getExisting "r_novis"
drawWorldCVar                = CVar.getExisting "r_drawworld"
noCullCVar                   = CVar.getExisting "r_nocull"
drawEntitiesCVar             = CVar.getExisting "r_drawentities"
lerpModelsCVar               = CVar.getExisting "r_lerpmodels"
scrCenterTimeCVar            = CVar.getExisting "scr_centertime"
gSelectEmptyCVar             = CVar.getExisting "g_select_empty"
genderAutoCVar               = CVar.getExisting "gender_auto"
maxSpectatorsCVar            = CVar.getExisting "maxspectators"