module QCommon.CM where

import qualified Data.ByteString as B

import Quake
import Game.CModelT
import Util.QuakeFile (QuakeFile)
-- import qualified Util.QuakeFile as QuakeFile

-- Loads in the map and all submodels.
loadMap :: B.ByteString -> Bool -> [Int] -> Quake (CModelT, [Int])
loadMap name clientLoad checksum = io (putStrLn "CM.loadMap") >> undefined -- TODO

inlineModel :: B.ByteString -> Quake CModelT
inlineModel _ = io (putStrLn "CM.inlineModel") >> undefined -- TODO

numInlineModels :: Quake Int
numInlineModels = io (putStrLn "CM.numInlineModels") >> undefined -- TODO

entityString :: Quake B.ByteString
entityString = io (putStrLn "CM.entityString") >> undefined -- TODO

-- CM_WritePortalState writes the portal state to a savegame file.
writePortalState :: QuakeFile -> Quake ()
writePortalState _ = io (putStrLn "CM.writePortalState") >> undefined -- TODO

-- CM_ReadPortalState reads the portal state from a savegame file and recalculates the area connections.
readPortalState :: QuakeFile -> Quake ()
readPortalState _ = io (putStrLn "CM.readPortalState") >> undefined -- TODO

setAreaPortalState :: Int -> Bool -> Quake ()
setAreaPortalState _ _ = io (putStrLn "CM.setAreaPortalState") >> undefined -- TODO

areasConnected :: Int -> Int -> Quake Bool
areasConnected _ _ = io (putStrLn "CM.areasConnected") >> undefined -- TODO
