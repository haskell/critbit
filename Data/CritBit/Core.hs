-- |
-- Module      :  Data.CritBit.Tree
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- "Core" functions that implement the crit-bit tree algorithms.
--
-- I plopped these functions into their own source file to demonstrate
-- just how small the core of the crit-bit tree concept is.
--
-- I have also commented this module a bit more heavily than I usually
-- do, in the hope that the comments will make the code more
-- approachable to less experienced Haskellers.
module Data.CritBit.Core
    (
    -- * Public functions
      insert
    , lookupWith
    , delete
    -- * Internal functions
    , calcDirection
    , direction
    , followPrefixes
    ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, xor)
import Data.CritBit.Types.Internal
import Data.Word (Word16)

-- | /O(log n)/. Insert a new key and value in the map.  If the key is
-- already present in the map, the associated value is replaced with
-- the supplied value. 'insert' is equivalent to @'insertWith'
-- 'const'@.
--
-- > insert "b" 7 (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",7)]
-- > insert "x" 7 (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",3), ("x",7)]
-- > insert "x" 5 empty                         == singleton "x" 5
insert :: (CritBitKey k) => k -> v -> CritBit k v -> CritBit k v
insert k v (CritBit root) = CritBit . go $ root
  where
    go i@(Internal left right _ _)
      | direction k i == 0 = go left
      | otherwise          = go right
    go (Leaf lk _)         = rewalk root
      where
        rewalk i@(Internal left right byte otherBits)
          | byte > n                     = finish i
          | byte == n && otherBits > nob = finish i
          | direction k i == 0           = i { ileft = rewalk left }
          | otherwise                    = i { iright = rewalk right }
        rewalk i                         = finish i

        finish (Leaf _ _) | k == lk = Leaf lk v
        finish node
          | nd == 0   = Internal { ileft = node, iright = Leaf k v,
                                   ibyte = n, iotherBits = nob }
          | otherwise = Internal { ileft = Leaf k v, iright = node,
                                   ibyte = n, iotherBits = nob }

        (n, nob, c) = followPrefixes k lk
        nd          = calcDirection nob c
    go Empty = Leaf k v
{-# INLINABLE insert #-}

lookupWith :: (CritBitKey k) =>
              a                 -- ^ Failure continuation
           -> (v -> a)          -- ^ Success continuation
           -> k
           -> CritBit k v -> a
-- We use continuations here to avoid reimplementing the lookup
-- algorithm with trivial variations.
lookupWith notFound found k (CritBit root) = go root
  where
    go i@(Internal left right _ _)
       | direction k i == 0  = go left
       | otherwise           = go right
    go (Leaf lk v) | k == lk = found v
    go _                     = notFound
{-# INLINE lookupWith #-}

-- | /O(log n)/. Delete a key and its value from the map. When the key
-- is not a member of the map, the original map is returned.
--
-- > delete "a" (fromList [("a",5), ("b",3)]) == singleton "b" 3
-- > delete "c" (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",3)]
-- > delete "a" empty                         == empty
delete :: (CritBitKey k) => k -> CritBit k v -> CritBit k v
-- Once again with the continuations! It's somewhat faster to do
-- things this way than to expicitly unwind our recursion once we've
-- found the leaf to delete. It's also a ton less code.
--
-- (If you want a good little exercise, rewrite this function without
-- using continuations, and benchmark the two versions.)
delete k t@(CritBit root) = go root CritBit
  where
    go i@(Internal left right _ _) cont
      | direction k i == 0 = go left $ \new ->
                             case new of
                               Empty -> cont right
                               l     -> cont $! i { ileft = l }
      | otherwise          = go right $ \new ->
                             case new of
                               Empty -> cont left
                               r     -> cont $! i { iright = r }
    go (Leaf lk _) cont
       | k == lk = cont Empty
    go _ _       = t
{-# INLINABLE delete #-}

-- | Determine which direction we should move down the tree based on
-- the critical bitmask at the current node and the corresponding byte
-- in the key. Left is 0, right is 1.
direction :: (CritBitKey k) => k -> Node k v -> Int
direction k (Internal _ _ byte otherBits) =
    calcDirection otherBits (getByte k byte)
direction _ _ = error "Data.CritBit.Core.direction: unpossible!"
{-# INLINE direction #-}

-- Given a critical bitmask and a byte, return 0 to move left, 1 to
-- move right.
calcDirection :: BitMask -> Word16 -> Int
calcDirection otherBits c = (1 + fromIntegral (otherBits .|. c)) `shiftR` 9
{-# INLINE calcDirection #-}

-- | Figure out the byte offset at which the key we are interested in
-- differs from the leaf we reached when we initially walked the tree.
--
-- We return some auxiliary stuff that we'll bang on to help us figure
-- out which direction to go in to insert a new node.
followPrefixes :: (CritBitKey k) =>
                  k             -- ^ The key from "outside" the tree.
               -> k             -- ^ Key from the leaf we reached.
               -> (Int, BitMask, Word16)
{-# INLINE followPrefixes #-}
followPrefixes k l = go 0
  where
    go n | n == byteCount k = (n, maskLowerBits c, c)
         | n == byteCount l = (n, maskLowerBits b, 0)
         | b /= c           = (n, maskLowerBits (b `xor` c), c)
         | otherwise        = go (n+1)
      where b = getByte k n
            c = getByte l n

    maskLowerBits v = (n3 .&. (complement (n3 `shiftR` 1))) `xor` 511
      where
        n3 = n2 .|. (n2 `shiftR` 8)
        n2 = n1 .|. (n1 `shiftR` 4)
        n1 = n0 .|. (n0 `shiftR` 2)
        n0 = v  .|. (v  `shiftR` 1)
