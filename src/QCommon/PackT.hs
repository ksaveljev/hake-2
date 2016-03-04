{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT
  ( module QCommon.PackT
  ) where

import qualified Constants
import           QCommon.PackFileT (getPackFile, packFileSize)
import           QCommon.DPackHeaderT (getDPackHeader)
import           Types
import           Util.Binary (encode)

import           Control.Lens (makeLenses)
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import qualified Data.HashMap.Lazy as HM
import           Pipes.Binary (Get)
import           System.IO (Handle)

makeLenses ''PackT

getPackT :: B.ByteString -> Handle -> Get PackT
getPackT packName fileHandle =
  do DPackHeaderT ident _ len <- getDPackHeader
     when (ident /= Constants.idPakHeader) $
       error (BC.unpack (packName `B.append` " is not a packfile"))
     let numPackFiles = len `div` packFileSize
     when (numPackFiles > Constants.maxFilesInPack) $
       error (BC.unpack (B.concat [packName, " has ", encode numPackFiles, " files"]))
     directoryFiles <- getDirectoryFiles numPackFiles HM.empty
     return (PackT packName (Just fileHandle) "" numPackFiles directoryFiles)
  where getDirectoryFiles n files
          | n == 0 = return files
          | otherwise =
              do file@(PackFileT name _ _) <- getPackFile
                 getDirectoryFiles (n - 1) (HM.insert (BC.map toLower name) file files)