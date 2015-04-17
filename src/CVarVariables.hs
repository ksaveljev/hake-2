{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module CVarVariables where

import qualified QCommon.CVar as CVar

clStereoSeparationCVar   = CVar.getExisting "cl_stereo_separation"
clStereoCVar             = CVar.getExisting "cl_stereo"
clAddBlendCVar           = CVar.getExisting "cl_blend"
clAddLightsCVar          = CVar.getExisting "cl_lights"
clAddParticlesCVar       = CVar.getExisting "cl_particles"
clAddEntitiesCVar        = CVar.getExisting "cl_entities"
clGunCVar                = CVar.getExisting "cl_gun"
clFootstepsCVar          = CVar.getExisting "cl_footsteps"
clNoSkinsCVar            = CVar.getExisting "cl_noskins"
clAutoSkinsCVar          = CVar.getExisting "cl_autoskins"
clPredictCVar            = CVar.getExisting "cl_predict"
clMaxFPSCVar             = CVar.getExisting "cl_maxfps"
clUpSpeedCVar            = CVar.getExisting "cl_upspeed"
clForwardSpeedCVar       = CVar.getExisting "cl_forwardspeed"
clSideSpeedCVar          = CVar.getExisting "cl_sidespeed"
clYawSpeedCVar           = CVar.getExisting "cl_yawspeed"
clPitchSpeedCVar         = CVar.getExisting "cl_pitchspeed"
clAngleSpeedkeyCVar      = CVar.getExisting "cl_anglespeedkey"
clRunCVar                = CVar.getExisting "cl_run"
lookSpringCVar           = CVar.getExisting "lookspring"
lookStrafeCVar           = CVar.getExisting "lookstrafe"
mPitchCVar               = CVar.getExisting "m_pitch"
mYawCVar                 = CVar.getExisting "m_yaw"
mForwardCVar             = CVar.getExisting "m_forward"
mSideCVar                = CVar.getExisting "m_side"
clShowNetCVar            = CVar.getExisting "cl_shownet"
clShowMissCVar           = CVar.getExisting "cl_showmiss"
clShowClampCVar          = CVar.getExisting "showclamp"
clTimeoutCVar            = CVar.getExisting "cl_timeout"
clPausedCVar             = CVar.getExisting "paused"
clTimeDemoCVar           = CVar.getExisting "timedemo"
rconClientPasswordCVar   = CVar.getExisting "rcon_password"
rconAddressCVar          = CVar.getExisting "rcon_address"
clLightLevelCVar         = CVar.getExisting "r_lightlevel"
infoPasswordCVar         = CVar.getExisting "password"
infoSpectatorCVar        = CVar.getExisting "spectator"
nameCVar                 = CVar.getExisting "name"
skinCVar                 = CVar.getExisting "skin"
rateCVar                 = CVar.getExisting "rate"
msgCVar                  = CVar.getExisting "msg"
handCVar                 = CVar.getExisting "hand"
fovCVar                  = CVar.getExisting "fov"
genderCVar               = CVar.getExisting "gender"
clVwepCVar               = CVar.getExisting "cl_vwep"
conNotifyTimeCVar        = CVar.getExisting "con_notifytime"
noStdoutCVar             = CVar.getExisting "nostdout"
hostSpeedsCVar           = CVar.getExisting "host_speeds"
logStatsCVar             = CVar.getExisting "log_stats"
developerCVar            = CVar.getExisting "developer"
timeScaleCVar            = CVar.getExisting "timescale"
fixedTimeCVar            = CVar.getExisting "fixedtime"
logfileActiveCVar        = CVar.getExisting "logfile"
showTraceCVar            = CVar.getExisting "showtrace"
dedicatedCVar            = CVar.getExisting "dedicated"
mFilterCVar              = CVar.getExisting "m_filter"
inMouseCVar              = CVar.getExisting "in_mouse"
freeLookCVar             = CVar.getExisting "freelook"
sensitivityCVar          = CVar.getExisting "sensitivity"
maxClientsCVar           = CVar.getExisting "maxclients"
hostnameCVar             = CVar.getExisting "hostname"
timeoutCVar              = CVar.getExisting "timeout"
zombieTimeCVar           = CVar.getExisting "zombietime"
svShowClampCVar          = CVar.getExisting "showclamp" -- IMPROVE: do we need it? we have a "client" version
svPausedCVar             = CVar.getExisting "paused" -- IMPROVE: do we need it? we have a "client" version
svTimeDemoCVar           = CVar.getExisting "timedemo" -- IMPROVE: do we need it? we have a "client" version
svEnforceTimeCVar        = CVar.getExisting "sv_enforcetime"
allowDownloadCVar        = CVar.getExisting "allow_download"
allowDownloadPlayersCVar = CVar.getExisting "allow_download_players"
allowDownloadModelsCVar  = CVar.getExisting "allow_download_models"
allowDownloadSoundsCVar  = CVar.getExisting "allow_download_sounds"
allowDownloadMapsCVar    = CVar.getExisting "allow_download_maps"
svNoReloadCVar           = CVar.getExisting "sv_noreload"
svAirAccelerateCVar      = CVar.getExisting "sv_airaccelerate"
publicServerCVar         = CVar.getExisting "public"
svReconnectLimitCVar     = CVar.getExisting "sv_reconnect_limit"
fsBaseDirCVar            = CVar.getExisting "basedir"
fsCdDirCVar              = CVar.getExisting "cddir"
fsGameDirVarCVar         = CVar.getExisting "game"
maxEntitiesCVar          = CVar.getExisting "maxentities"
mapNoAreasCVar           = CVar.getExisting "map_noareas"
skillCVar                = CVar.getExisting "skill"
coopCVar                 = CVar.getExisting "coop"
deathmatchCVar           = CVar.getExisting "deathmatch"
dmFlagsCVar              = CVar.getExisting "dmflags"
svMaxVelocityCVar        = CVar.getExisting "sv_maxvelocity"
svGravityCVar            = CVar.getExisting "sv_gravity"
timeLimitCVar            = CVar.getExisting "timelimit"
fragLimitCVar            = CVar.getExisting "fraglimit"
passwordCVar             = CVar.getExisting "password" -- do we need this? we have infoPasswordCVar
spectatorPasswordCVar    = CVar.getExisting "spectator_password"
vidWidthCVar             = CVar.getExisting "vid_width"
vidHeightCVar            = CVar.getExisting "vid_height"
vidRefCVar               = CVar.getExisting "vid_ref"
vidFullScreenCVar        = CVar.getExisting "vid_fullscreen"
vidXPosCVar              = CVar.getExisting "vid_xpos"
vidYPosCVar              = CVar.getExisting "vid_ypos"
