{-# LANGUAGE Rank2Types #-}
module Client.CLParseShared
    ( loadClientInfo
    , parseClientInfo
    , showNet
    , svcStrings
    ) where

import           Control.Lens          (Traversal', use, preuse, ix, (^.), (.=), (&), (.~))
import           Control.Monad         (when)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isNothing, fromMaybe)
import qualified Data.Vector           as V

import           Client.ClientInfoT
import           Client.ClientStateT
import           Client.RefExportT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QCommon.SizeBufT
import           QuakeState
import           Render.Renderer
import           Types
import           Util.Binary           (encode)

svcStrings :: V.Vector B.ByteString
svcStrings = V.fromList
    [ "svc_bad", "svc_muzzleflash", "svc_muzzlflash2"
    , "svc_temp_entity", "svc_layout", "svc_inventory"
    , "svc_nop", "svc_disconnect", "svc_reconnect"
    , "svc_sound", "svc_print", "svc_stufftext"
    , "svc_serverdata", "svc_configstring", "svc_spawnbaseline"
    , "svc_centerprint", "svc_download", "svc_playerinfo"
    , "svc_packetentities", "svc_deltapacketentities", "svc_frame"
    ]

parseClientInfo :: Int -> Quake ()
parseClientInfo player = do
    str <- preuse (globals.gCl.csConfigStrings.ix (player + Constants.csPlayerSkins))
    maybe clientInfoError (loadClientInfo (globals.gCl.csClientInfo.ix player)) str
  where
    clientInfoError = Com.fatalError "CLParse.parseClientInfo config string is Nothing"

loadClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> Quake ()
loadClientInfo clientInfo str = do
    renderer <- use (globals.gRenderer)
    noSkins <- fmap (^.cvValue) clNoSkinsCVar
    proceedLoadClientInfo clientInfo renderer noSkins name str'
  where
    (name, str') = case '\\' `BC.elemIndex` str of
                       Nothing -> (str, str)
                       Just idx -> (B.take idx str, B.drop (idx + 1) str)

proceedLoadClientInfo :: Traversal' QuakeState ClientInfoT -> Maybe Renderer -> Float -> B.ByteString -> B.ByteString -> Quake ()
proceedLoadClientInfo _ Nothing _ _ _ = Com.fatalError "CLParse.proceedLoadClientInfo renderer is Nothing"
proceedLoadClientInfo clientInfo (Just renderer) noSkins name str = do
    (modelRef, weaponModels, skinRef, iconName, iconRef) <- doLoadClientInfo renderer noSkins str
    saveClientInfo clientInfo name str modelRef weaponModels skinRef iconName iconRef

doLoadClientInfo :: Renderer -> Float -> B.ByteString -> Quake (Maybe (Ref' ModelT), V.Vector (Maybe (Ref' ModelT)), Maybe (Ref' ImageT), B.ByteString, Maybe (Ref' ImageT))
doLoadClientInfo renderer noSkins str
    | noSkins /= 0 || B.null str = getDefaultClientInfo renderer
    | otherwise =  getClientInfo renderer str

getDefaultClientInfo :: Renderer -> Quake (Maybe (Ref' ModelT), V.Vector (Maybe (Ref' ModelT)), Maybe (Ref' ImageT), B.ByteString, Maybe (Ref' ImageT))
getDefaultClientInfo renderer = do
    modelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
    weaponModelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/weapon.md2"
    skinRef <- (renderer^.rRefExport.reRegisterSkin) "players/male/grunt.pcx"
    iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
    return (modelRef, defaultWeaponModels weaponModelRef, skinRef, iconName, iconRef)
  where
    iconName = "/players/male/grunt_i.pcx"
    defaultWeaponModels weaponModelRef =
        V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef else Nothing)

getClientInfo :: Renderer -> B.ByteString -> Quake (Maybe (Ref' ModelT), V.Vector (Maybe (Ref' ModelT)), Maybe (Ref' ImageT), B.ByteString, Maybe (Ref' ImageT))
getClientInfo renderer str = do
    when (isNothing nameIdx1 && isNothing nameIdx2) $
        Com.fatalError ("Invalid model name:" `B.append` str)
    modelRef <- (renderer^.rRefExport.reRegisterModel) modelFileName
    (modelName', modelRef') <- checkModelRef modelRef
    skinRef <- (renderer^.rRefExport.reRegisterSkin) (B.concat ["players/", modelName', "/", skinName, ".pcx"])
    (modelName'', modelRef'', skinRef') <- checkSkinRef skinRef modelName' modelRef'
    skinRef'' <- recheckSkinRef skinRef' modelName''
    weaponModels <- loadWeaponModels renderer modelName''
    (iconName, iconRef) <- loadIcon renderer modelName'' skinName
    return (modelRef'', weaponModels, skinRef'', iconName, iconRef)
  where
    nameIdx1 = '/' `BC.elemIndex` str
    nameIdx2 = '\\' `BC.elemIndex` str
    pos = fromMaybe (fromMaybe 0 nameIdx2) nameIdx1
    modelName = B.take pos str
    skinName = B.drop (pos + 1) str
    modelFileName = B.concat ["players/", modelName, "/tris.md2"]
    checkModelRef Nothing = do
        modelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
        return ("male", modelRef)
    checkModelRef modelRef = return (modelName, modelRef)
    checkSkinRef skinRef modelName' modelRef'
        | isNothing skinRef && (BC.map toLower modelName') /= "male" = do
            modelRef'' <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
            skinRef' <- (renderer^.rRefExport.reRegisterSkin) (B.concat ["players/male/", skinName, ".pcx"])
            return ("male", modelRef'', skinRef')
        | otherwise = return (modelName', modelRef', skinRef)
    recheckSkinRef Nothing modelName'' =
        (renderer^.rRefExport.reRegisterSkin) (B.concat ["players/", modelName'', "/grunt.pcx"])
    recheckSkinRef skinRef _ = return skinRef

loadWeaponModels :: Renderer -> B.ByteString -> Quake (V.Vector (Maybe (Ref' ModelT)))
loadWeaponModels renderer modelName = do
    vwep <- fmap (^.cvValue) clVwepCVar
    clWeaponModels <- use (clientGlobals.cgWeaponModels)
    doLoadWeaponModels vwep clWeaponModels
  where
    doLoadWeaponModels :: Float -> V.Vector B.ByteString -> Quake (V.Vector (Maybe (Ref' ModelT)))
    doLoadWeaponModels vwep clWeaponModels
        | vwep == 0 = do
            weaponModelRef <- (renderer^.rRefExport.reRegisterModel) (B.concat ["players/", modelName, "/", clWeaponModels V.! 0])
            weaponModelRef' <- if isNothing weaponModelRef && (BC.map toLower modelName) == "cyborg"
                                   then (renderer^.rRefExport.reRegisterModel) ("players/male/" `B.append` (clWeaponModels V.! 0))
                                   else return weaponModelRef
            return (V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef' else Nothing))
        | otherwise = do
            numWeaponModels <- use (clientGlobals.cgNumCLWeaponModels)
            loadedWeapons <- loadWeapons renderer clWeaponModels modelName 0 numWeaponModels []
            return ((V.replicate Constants.maxClientWeaponModels Nothing) V.// loadedWeapons)

loadWeapons :: Renderer -> V.Vector B.ByteString -> B.ByteString -> Int -> Int -> [(Int, Maybe (Ref' ModelT))] -> Quake [(Int, Maybe (Ref' ModelT))]
loadWeapons renderer clWeaponModels modelName idx maxIdx acc
    | idx >= maxIdx = return acc
    | otherwise = do
        weaponModelRef <- (renderer^.rRefExport.reRegisterModel) (B.concat ["players/", modelName, "/", clWeaponModels V.! idx])
        weaponModelRef' <- if isNothing weaponModelRef && (BC.map toLower modelName) == "cyborg"
                               then (renderer^.rRefExport.reRegisterModel) ("players/male/" `B.append` (clWeaponModels V.! idx))
                               else return weaponModelRef
        loadWeapons renderer clWeaponModels modelName (idx + 1) maxIdx ((idx, weaponModelRef') : acc)

loadIcon :: Renderer -> B.ByteString -> B.ByteString -> Quake (B.ByteString, Maybe (Ref' ImageT))
loadIcon renderer modelName skinName = do
    iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
    return (iconName, iconRef)
  where
    iconName = B.concat ["/players/", modelName, "/", skinName, "_i.pcx"]

saveClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> B.ByteString -> Maybe (Ref' ModelT) -> V.Vector (Maybe (Ref' ModelT)) -> Maybe (Ref' ImageT) -> B.ByteString -> Maybe (Ref' ImageT) -> Quake ()
saveClientInfo clientInfo name str modelRef weaponModels skinRef iconName iconRef
    | isNothing skinRef || isNothing iconRef || isNothing modelRef || isNothing (weaponModels V.! 0) =
        clientInfo .= (newClientInfoT & ciName        .~ name
                                     & ciCInfo       .~ str
                                     & ciSkin        .~ Nothing
                                     & ciIcon        .~ Nothing
                                     & ciIconName    .~ iconName
                                     & ciModel       .~ Nothing
                                     & ciWeaponModel .~ V.replicate Constants.maxClientWeaponModels Nothing)
    | otherwise =
        clientInfo .= (newClientInfoT & ciName        .~ name
                                     & ciCInfo       .~ str
                                     & ciSkin        .~ skinRef
                                     & ciIcon        .~ iconRef
                                     & ciIconName    .~ iconName
                                     & ciModel       .~ modelRef
                                     & ciWeaponModel .~ weaponModels)

showNet :: B.ByteString -> CVarT -> Quake ()
showNet str showNetVar
    | (showNetVar^.cvValue) >= 2 = do
        readCount <- use (globals.gNetMessage.sbReadCount)
        Com.printf (B.concat [encode (readCount - 1), ":", str, "\n"])
    | otherwise = return ()