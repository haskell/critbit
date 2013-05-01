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

instance (Eq k, Eq v) => Eq (Node k v) where
    Empty        == Empty       = True
    Leaf k0 v0   == Leaf k1 v1  = k0 == k1 && v0 == v1
    i0@(Internal _ _ _ _)  == i1@(Internal _ _ _ _) =
        ibyte i0 == ibyte i1 && iotherBits i0 == iotherBits i1 &&
        ileft i0 == ileft i1 && iright i0 == iright i1
    _            == _           = False

newtype CritBit k v = CritBit {
      cbRoot :: Node k v
    } deriving (Show, Eq)

class (Eq k) => CritBitKey k where
    byteCount :: k -> Int
    getByte :: k -> Int -> Word16

instance CritBitKey ByteString where
    byteCount = B.length
    getByte bs n
        | n < B.length bs = fromIntegral (B.unsafeIndex bs n) .|. 256
        | otherwise       = 0
