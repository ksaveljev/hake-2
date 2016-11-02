{-# LANGUAGE Rank2Types #-}
module Client.CLParseShared
    ( loadClientInfo
    , parseClientInfo
    ) where

import           Control.Lens          (Traversal', use, preuse, ix, (^.), (.=), (&), (.~))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (isNothing)
import qualified Data.Vector           as V

import           Client.ClientInfoT
import           Client.ClientStateT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeState
import           Types

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
    (modelRef, weaponModels, skinRef, iconName, iconRef) <- doLoadClientInfo
    saveClientInfo clientInfo name str modelRef weaponModels skinRef iconName iconRef
  where
    (name, str') = case '\\' `BC.elemIndex` str of
                       Nothing -> (str, str)
                       Just idx -> (B.take idx str, B.drop (idx + 1) str)

doLoadClientInfo = do
    undefined -- TODO
{-
    (modelRef, weaponModels, skinRef, iconName, iconRef) <-
      if noSkinsValue /= 0 || B.length str' == 0
        then do
          modelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
          weaponModelRef <- (renderer^.rRefExport.reRegisterModel) "players/male/weapon.md2"
          skinRef <- (renderer^.rRefExport.reRegisterSkin) "players/male/grunt.pcx"
          let iconName = "/players/male/grunt_i.pcx"
              weaponModels = V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef else Nothing)
          iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
          return (modelRef, weaponModels, skinRef, iconName, iconRef)
        else do
          -- isolate the model name
          pos <- case '/' `BC.elemIndex` str' of
                   Nothing -> case '\\' `BC.elemIndex` str' of
                                Nothing -> do
                                  Com.comError Constants.errFatal ("Invalid model name:" `B.append` str')
                                  return 0
                                Just idx ->
                                  return idx
                   Just idx -> return idx

          let modelName = B.take pos str'
              -- isolate the skin name
              skinName = B.drop (pos + 1) str'
              -- model file
              modelFileName = "players/" `B.append` modelName `B.append` "/tris.md2"

          modelRef <- (renderer^.rRefExport.reRegisterModel) modelFileName
          (modelName', modelRef') <- case modelRef of
                                       Just _ -> return (modelName, modelRef)
                                       Nothing -> do
                                         ref <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
                                         return ("male", ref)

          -- skin file
          let skinFileName = "players/" `B.append` modelName' `B.append` "/" `B.append` skinName `B.append` ".pcx"
          skinRef <- (renderer^.rRefExport.reRegisterSkin) skinFileName

          -- if we don't have the skin and the model wasn't male,
          -- see if the male has it (this is for CTF's skins)
          (modelName'', modelRef'', skinRef') <-
            if isNothing skinRef && (BC.map toLower modelName') /= "male"
              then do
                -- change model to male
                ref <- (renderer^.rRefExport.reRegisterModel) "players/male/tris.md2"
                -- see if the skin exists for the male model
                let skinFileName' = "players/male/" `B.append` skinName `B.append` ".pcx"
                sr <- (renderer^.rRefExport.reRegisterSkin) skinFileName'
                return ("male", ref, sr)
              else
                return (modelName', modelRef', skinRef)
          
          -- if we still don't have a skin, it means that the male model
          -- didn't have it, so default to grunt
          skinRef'' <- if isNothing skinRef'
                         then do
                           -- see if the skin exists for the male model
                           let skinFileName' = "players/" `B.append` modelName'' `B.append` "/grunt.pcx"
                           (renderer^.rRefExport.reRegisterSkin) skinFileName'
                         else
                           return skinRef'

          -- weapon file
          vwepValue <- liftM (^.cvValue) clVwepCVar
          clWeaponModels <- use $ clientGlobals.cgWeaponModels
          weaponModels <- if vwepValue == 0
                            then do
                              let weaponFileName = "players/" `B.append` modelName'' `B.append` "/" `B.append` (clWeaponModels V.! 0)
                              weaponModelRef <- (renderer^.rRefExport.reRegisterModel) weaponFileName
                              if isNothing weaponModelRef && (BC.map toLower modelName'') == "cyborg"
                                then do
                                  -- try male
                                  let weaponFileName' = "players/male/" `B.append` (clWeaponModels V.! 0)
                                  weaponModelRef' <- (renderer^.rRefExport.reRegisterModel) weaponFileName'
                                  return $ V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef' else Nothing)
                                else
                                  return $ V.generate Constants.maxClientWeaponModels (\idx -> if idx == 0 then weaponModelRef else Nothing)
                            else do
                              numWeaponModels <- use $ clientGlobals.cgNumCLWeaponModels
                              let emptyWeapons = V.replicate Constants.maxClientWeaponModels Nothing
                              updates <- loadWeapons clWeaponModels modelName'' 0 numWeaponModels []
                              return $ emptyWeapons V.// updates

          -- icon file
          let iconName = "/players/" `B.append` modelName'' `B.append` "/" `B.append` skinName `B.append` "_i.pcx"
          iconRef <- (renderer^.rRefExport.reRegisterPic) iconName
          return (modelRef'', weaponModels, skinRef'', iconName, iconRef)

  where loadWeapons :: V.Vector B.ByteString -> B.ByteString -> Int -> Int -> [(Int, Maybe (IORef ModelT))] -> Quake [(Int, Maybe (IORef ModelT))]
        loadWeapons clWeaponModels modelName idx maxIdx acc
          | idx >= maxIdx = return acc
          | otherwise = do
              Just renderer <- use $ globals.re
              let weaponFileName = "players/" `B.append` modelName `B.append` "/" `B.append` (clWeaponModels V.! idx)
              weaponModelRef <- (renderer^.rRefExport.reRegisterModel) weaponFileName
              if isNothing weaponModelRef && (BC.map toLower modelName) == "cyborg"
                then do
                  -- try male
                  let weaponFileName' = "players/male/" `B.append` (clWeaponModels V.! idx)
                  weaponModelRef' <- (renderer^.rRefExport.reRegisterModel) weaponFileName'
                  loadWeapons clWeaponModels modelName (idx + 1) maxIdx ((idx, weaponModelRef') : acc)
                else
                  loadWeapons clWeaponModels modelName (idx + 1) maxIdx ((idx, weaponModelRef) : acc)
                  -}

saveClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> B.ByteString -> Maybe (Ref ModelT) -> V.Vector (Maybe (Ref ModelT)) -> Maybe (Ref ImageT) -> B.ByteString -> Maybe (Ref ImageT) -> Quake ()
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