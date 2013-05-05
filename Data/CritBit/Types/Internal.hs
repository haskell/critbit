{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      :  Data.CritBit.Types.Internal
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
module Data.CritBit.Types.Internal
    (
      CritBitKey(..)
    , CritBit(..)
    , Node(..)
    ) where

import Control.DeepSeq (NFData(..))
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Text ()
import Data.Text.Internal (Text(..))
import Data.Word (Word16)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Array as T

data Node k v = Internal {
                  ileft, iright :: !(Node k v)
                , ibyte         :: !Int
                , iotherBits    :: !Word16
                }
              | Leaf k v
              | Empty
                deriving (Show)

instance (NFData k, NFData v) => NFData (Node k v) where
    rnf (Internal l r _ _) = rnf l `seq` rnf r
    rnf (Leaf k v)         = rnf k `seq` rnf v
    rnf Empty              = ()

instance (Eq k, Eq v) => Eq (Node k v) where
    i0@(Internal _ _ _ _)  == i1@(Internal _ _ _ _) =
        ibyte i0 == ibyte i1 && iotherBits i0 == iotherBits i1 &&
        ileft i0 == ileft i1 && iright i0 == iright i1
    Leaf k0 v0   == Leaf k1 v1  = k0 == k1 && v0 == v1
    Empty        == Empty       = True
    _            == _           = False

newtype CritBit k v = CritBit {
      cbRoot :: Node k v
    } deriving (Show, Eq, NFData)

class (Eq k) => CritBitKey k where
    -- | Return the number of bytes used by this key.
    --
    -- For reasonable performance, implementations must be inlined and
    -- /O(1)/.
    byteCount :: k -> Int

    -- | Return the byte at the given offset (counted in bytes) of
    -- this key, bitwise-ORed with 256. If the offset is past the end
    -- of the key, return zero.
    --
    -- For reasonable performance, implementations must be inlined and
    -- /O(1)/.
    getByte :: k -> Int -> Word16

instance CritBitKey ByteString where
    byteCount = B.length
    {-# INLINE byteCount #-}

    getByte bs n
        | n < B.length bs = fromIntegral (B.unsafeIndex bs n) .|. 256
        | otherwise       = 0
    {-# INLINE getByte #-}

instance CritBitKey Text where
    byteCount (Text _ _ len) = len `shiftL` 1
    {-# INLINE byteCount #-}

    getByte (Text arr off len) n
        | n < len `shiftL` 1 =
            let word       = T.unsafeIndex arr (off + (n `shiftR` 1))
                byteInWord = (word `shiftR` ((n .&. 1) `shiftL` 3)) .&. 0xff
            in byteInWord .|. 256
        | otherwise       = 0
    {-# INLINE getByte #-}
