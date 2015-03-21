module QCommon.CM where

import qualified Data.ByteString as B

import Quake
import Game.CModelT
import Util.QuakeFile (QuakeFile)
-- import qualified Util.QuakeFile as QuakeFile

loadMap :: B.ByteString -> Bool -> [Int] -> Quake (CModelT, [Int])
loadMap = undefined -- TODO

inlineModel :: B.ByteString -> Quake CModelT
inlineModel = undefined -- TODO

numInlineModels :: Quake Int
numInlineModels = undefined -- TODO

entityString :: Quake B.ByteString
entityString = undefined -- TODO

-- CM_WritePortalState writes the portal state to a savegame file.
writePortalState :: QuakeFile -> Quake ()
writePortalState = undefined -- TODO

-- CM_ReadPortalState reads the portal state from a savegame file and recalculates the area connections.
readPortalState :: QuakeFile -> Quake ()
readPortalState = undefined -- TODO

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState = undefined -- TODO

areasConnected :: Int -> Int -> Quake Bool
areasConnected = undefined -- TODO
