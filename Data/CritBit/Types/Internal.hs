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
    , BitMask
    , Node(..)
    , toList
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable, traverse)
import Data.Text ()
import Data.Text.Internal (Text(..))
import Data.Word (Word16)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Array as T

type BitMask = Word16

data Node k v =
    Internal {
      ileft, iright :: !(Node k v)
    , ibyte         :: !Int
    -- ^ The byte at which the left and right subtrees differ.
    , iotherBits    :: !BitMask
    -- ^ The bitmask representing the critical bit within the
    -- differing byte. If the critical bit is e.g. 0x8, the bitmask
    -- will have every bit below 0x8 set, hence 0x7.
    }
    | Leaf k v
    | Empty
    -- ^ Logically, the 'Empty' constructor is a property of the tree,
    -- rather than a node (a non-empty tree will never contain any
    -- 'Empty' constructors). In practice, turning 'CritBit' from a
    -- newtype into an ADT with an 'Empty' constructor adds a
    -- pattern-match and a memory indirection to every function, which
    -- slows them all down.
      deriving (Eq, Show)

instance (NFData k, NFData v) => NFData (Node k v) where
    rnf (Internal l r _ _) = rnf l `seq` rnf r
    rnf (Leaf k v)         = rnf k `seq` rnf v
    rnf Empty              = ()

instance Functor (Node k) where
    fmap f i@(Internal l r _ _) = i { ileft = fmap f l, iright = fmap f r }
    fmap f (Leaf k v)           = Leaf k (f v)
    fmap _ Empty                = Empty

instance Foldable (Node k) where
    foldMap f (Internal l r _ _) = mappend (foldMap f l) (foldMap f r)
    foldMap f (Leaf _ v)         = f v
    foldMap _ Empty              = mempty

instance Traversable (Node k) where
    traverse f i@(Internal l r _ _) = 
      let constr l' r' = i { ileft = l', iright = r' }
      in constr <$> traverse f l <*> traverse f r
    traverse f (Leaf k v)           = (Leaf k) <$> f v
    traverse _ Empty                = pure Empty

-- | A crit-bit tree.
newtype CritBit k v = CritBit {
      cbRoot :: Node k v
    } deriving (Eq, NFData, Functor, Foldable, Traversable)

instance (Show k, Show v) => Show (CritBit k v) where
    show t = "fromList " ++ show (toList t)

-- | A type that can be used as a key in a crit-bit tree.
--
-- We use 9 bits to represent 8-bit bytes so that we can distinguish
-- between an interior byte that is zero (which must have the 9th bit
-- set) and a byte past the end of the input (which must /not/ have
-- the 9th bit set).
--
-- Without this trick, the critical bit calculations would fail on
-- zero bytes /within/ a string, and our tree would be unable to
-- handle arbitrary binary data.
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

-- | /O(n)/. Convert the map to a list of key\/value pairs. The list
-- returned will be sorted in lexicographically ascending order.
--
-- > toList (fromList [("b",3), ("a",5)]) == [("a",5),("b",3)]
-- > toList empty == []
toList :: CritBit k v -> [(k, v)]
toList (CritBit root) = go root []
  where
    go (Internal l r _ _) next = go l (go r next)
    go (Leaf k v) next         = (k,v) : next
    go Empty next              = next
