module Util.Unsafe
    ( mVectorToByteString
    ) where

import qualified Data.ByteString              as B
import qualified Data.ByteString.Internal     as BI
import qualified Data.Vector.Storable.Mutable as MSV
import           Foreign.Storable             (Storable, sizeOf)
import           Foreign.ForeignPtr           (castForeignPtr)

mVectorToByteString :: (Storable a) => a -> MSV.IOVector a -> B.ByteString
mVectorToByteString element vec =
    BI.fromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
  where
    (fptr, off, len) = MSV.unsafeToForeignPtr vec
    scale = (* sizeOf element)