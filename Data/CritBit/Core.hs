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
      insertWithKey
    , lookupWith
    , updateLookupWithKey
    , leftmost
    , rightmost
    -- * Internal functions
    , calcDirection
    , direction
    , followPrefixes
    , followPrefixesFrom
    ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, xor)
import Data.CritBit.Types.Internal
import Data.Word (Word16)

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value cb@
-- will insert the pair (key, value) into cb if key does not exist in the map.
-- If the key does exist, the function will insert the pair
-- @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to insertWithKey.
--
-- > let f key new_value old_value = byteCount key + new_value + old_value
-- > insertWithKey f "a" 1 (fromList [("a",5), ("b",3)]) == fromList [("a",7), ("b",3)]
-- > insertWithKey f "c" 1 (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",3), ("c",1)]
-- > insertWithKey f "a" 1 empty                         == singleton "a" 1
insertWithKey :: CritBitKey k => (k -> v -> v -> v) -> k -> v -> CritBit k v
              -> CritBit k v
insertWithKey f k v (CritBit root) = CritBit . go $ root
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

        finish (Leaf _ v') | k == lk = Leaf k (f k v v')
        finish node
          | nd == 0   = Internal { ileft = node, iright = Leaf k v,
                                   ibyte = n, iotherBits = nob }
          | otherwise = Internal { ileft = Leaf k v, iright = node,
                                   ibyte = n, iotherBits = nob }

        (n, nob, c) = followPrefixes k lk
        nd          = calcDirection nob c
    go Empty = Leaf k v
{-# INLINABLE insertWithKey #-}

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

-- | /O(log n)/. Lookup and update; see also 'updateWithKey'.
-- This function returns the changed value if it is updated, or
-- the original value if the entry is deleted.
--
-- > let f k x = if x == 5 then Just (x + fromEnum (k < "d")) else Nothing
-- > updateLookupWithKey f "a" (fromList [("b",3), ("a",5)]) == (Just 6, fromList [("a", 6), ("b",3)])
-- > updateLookupWithKey f "c" (fromList [("a",5), ("b",3)]) == (Nothing, fromList [("a",5), ("b",3)])
-- > updateLookupWithKey f "b" (fromList [("a",5), ("b",3)]) == (Just 3, singleton "a" 5)
updateLookupWithKey :: (CritBitKey k) => (k -> v -> Maybe v) -> k
                       -> CritBit k v -> (Maybe v, CritBit k v)
-- Once again with the continuations! It's somewhat faster to do
-- things this way than to expicitly unwind our recursion once we've
-- found the leaf to delete. It's also a ton less code.
--
-- (If you want a good little exercise, rewrite this function without
-- using continuations, and benchmark the two versions.)
updateLookupWithKey f k t@(CritBit root) = go root CritBit
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
    go (Leaf lk lv) cont
      | k == lk = case f k lv of
                    Just lv' -> (Just lv', cont (Leaf lk lv'))
                    Nothing  -> (Just lv, cont Empty)
    go _ _    = (Nothing, t)
{-# INLINABLE updateLookupWithKey #-}

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
followPrefixes = followPrefixesFrom 0
{-# INLINE followPrefixes #-}

-- | Figure out the offset of the first different byte in two keys,
-- starting from specified position.
--
-- We return some auxiliary stuff that we'll bang on to help us figure
-- out which direction to go in to insert a new node.
followPrefixesFrom :: (CritBitKey k) =>
                      Int           -- ^ Positition to start from
                   -> k             -- ^ First key.
                   -> k             -- ^ Second key.
                   -> (Int, BitMask, Word16)
followPrefixesFrom position k l = go position
  where
    -- Warning! 'fromAscListWithKey' assumes that last element of the result
    -- argument is zero if and only if keys are equal
    go n | n == byteCount k = (n, maskLowerBits c, c)
         | n == byteCount l = (n, maskLowerBits b, 0)
         | b /= c           = (n, maskLowerBits (b `xor` c), c)
         | otherwise        = go (n+1)
      where b = getByte k n
            c = getByte l n

    maskLowerBits :: Word16 -> Word16
    maskLowerBits v = (n3 .&. (complement (n3 `shiftR` 1))) `xor` 0x1FF
      where
        n3 = n2 .|. (n2 `shiftR` 8)
        n2 = n1 .|. (n1 `shiftR` 4)
        n1 = n0 .|. (n0 `shiftR` 2)
        n0 = v  .|. (v  `shiftR` 1)
{-# INLINE followPrefixesFrom #-}

leftmost, rightmost :: a -> (k -> v -> a) -> Node k v -> a
leftmost  = extremity ileft
{-# INLINE leftmost #-}
rightmost = extremity iright
{-# INLINE rightmost #-}

-- | Generic function so we can easily implement 'leftmost' and 'rightmost'.
extremity :: (Node k v -> Node k v) -- ^ Either 'ileft' or 'iright'.
          -> a                      -- ^ 'Empty' continuation.
          -> (k -> v -> a)          -- ^ 'Leaf' continuation.
          -> Node k v
          -> a
extremity direct onEmpty onLeaf node = go node
  where
    go i@(Internal{}) = go $ direct i
    go (Leaf k v)     = onLeaf k v
    go _              = onEmpty
    {-# INLINE go #-}
{-# INLINE extremity #-}
