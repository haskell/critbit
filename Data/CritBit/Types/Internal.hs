module Data.CritBit.Types.Internal
    where

import Data.Word
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B

data Node k v = Empty
              | Leaf k v
              | Internal {
                  ileft, iright :: Node k v
                , ibyte :: Int
                , iotherBits :: Word8
                }
                deriving (Show)

newtype CritBit k v = CritBit {
      cbRoot :: Node k v
    } deriving (Show)

class (Eq k) => CritBitKey k where
    byteCount :: k -> Int
    getByte :: k -> Int -> Word8

instance CritBitKey ByteString where
    byteCount = B.length
    getByte bs n
        | n < B.length bs = fromIntegral (B.unsafeIndex bs n)
        | otherwise       = 0
