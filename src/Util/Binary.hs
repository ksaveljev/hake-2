module Util.Binary
  ( encode
  , getInt
  , getInt16
  , getInt4T
  , getInt8
  , getMany
  , getV3Float
  , getV3Int16
  , getV4Float
  ) where

import           Control.Monad.State.Strict (runStateT)
import           Data.Binary.Get (Get, getWord16le, getWord32le, getWord8)
import           Data.Binary.IEEE754 (getFloat32le)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int8, Int16, Int32)
import           Pipes (Producer, lift, yield)
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PBS
import           Linear (V3(..), V4(..))

encode :: Show a => a -> B.ByteString
encode = BC.pack . show

getInt :: Get Int
getInt = let !x = fromIntegral <$> getWord32le :: Get Int32
         in fromIntegral <$> x

getInt16 :: Get Int16
getInt16 = fromIntegral <$> getWord16le

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt4T :: Get (Int, Int, Int, Int)
getInt4T = (,,,) <$> getInt <*> getInt <*> getInt <*> getInt

getV3Int16 :: Get (V3 Int16)
getV3Int16 = V3 <$> getInt16 <*> getInt16 <*> getInt16

getV3Float :: Get (V3 Float)
getV3Float = V3 <$> getFloat32le <*> getFloat32le <*> getFloat32le

getV4Float :: Get (V4 Float)
getV4Float = V4 <$> getFloat32le
                <*> getFloat32le
                <*> getFloat32le
                <*> getFloat32le

getMany :: Monad m => Get a -> Producer PBS.ByteString m r -> Producer a m PB.DecodingError
getMany getA = go
  where go p = do
          (!x, !p') <- lift (runStateT (PB.decodeGet getA) p)
          case x of
            Left err -> return err
            Right !a -> do
              yield a
              go p'
