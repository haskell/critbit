module Data.CritBit.Types.Internal
    where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B

data Node k v = Empty
              | Leaf k v
              | Internal {
                  ileft, iright :: Node k v
                , ibyte :: Int
                , iotherBits :: Word16
                }
                deriving (Show)

newtype CritBit k v = CritBit {
      cbRoot :: Node k v
    } deriving (Show)

class (Eq k) => CritBitKey k where
    byteCount :: k -> Int
    getByte :: k -> Int -> Word16

instance CritBitKey ByteString where
    byteCount = B.length
    getByte bs n
        | n < B.length bs = fromIntegral (B.unsafeIndex bs n) .|. 256
        | otherwise       = 0
