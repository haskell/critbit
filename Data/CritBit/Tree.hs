{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Data.CritBit.Tree
    where

import qualified Data.List as List
import Data.Bits
import Data.CritBit.Types.Internal
import Data.Word

empty :: CritBit k v
empty = CritBit { cbRoot = Empty }

lookup :: (CritBitKey k) => k -> CritBit k v -> Maybe v
lookup k = go . cbRoot
  where
    go i@(Internal left right _ _)
       | direction k i == 0  = go left
       | otherwise           = go right
    go (Leaf lk v) | k == lk = Just v
    go _                     = Nothing

direction :: (CritBitKey k) => k -> Node k v -> Int
direction k (Internal _ _ byte otherBits) =
    calcDirection otherBits (getByte k byte)

calcDirection :: Word8 -> Word8 -> Int
calcDirection otherBits c = (1 + fromIntegral (otherBits .|. c)) `shiftR` 8

followPrefixes :: (CritBitKey k) => k -> k -> (Int, Word8, Word8)
followPrefixes k l = go 0
  where
    go n | n == byteCount k = (n, c, c)
         | n == byteCount l = (n, b, 0)
         | b /= c           = (n, b `xor` c, c)
         | otherwise        = go (n+1)
      where b = getByte k n
            c = getByte l n

insert :: (CritBitKey k) => k -> v -> CritBit k v -> CritBit k v
insert k v (CritBit root) = CritBit . go $ root
  where
    go Empty = Leaf k v
    go i@(Internal left right _ _)
      | direction k i == 0 = go left
      | otherwise          = go right
    go (Leaf lk _)         = rewalk root
      where
        finish (Leaf _ _) | k == lk = Leaf lk v
        finish node
          | nd == 0 = Internal { ileft = node, iright = Leaf k v,
                                 ibyte = n, iotherBits = nob }
          | otherwise = Internal { ileft = Leaf k v, iright = node,
                                   ibyte = n, iotherBits = nob }
        rewalk i@(Internal left right byte otherBits)
          | byte > n                     = finish i
          | byte == n && otherBits > nob = finish i
          | direction k i == 0           = i { ileft = rewalk left }
          | otherwise                    = i { iright = rewalk right }
        rewalk i                         = finish i
        (n, bc, c) = followPrefixes k lk
        nob = let n0 = bc .|. (bc `shiftR` 1)
                  n1 = n0 .|. (n0 `shiftR` 2)
                  n2 = n1 .|. (n1 `shiftR` 4)
              in complement (n2 .&. (complement (n2 `shiftR` 1)))
        nd = calcDirection nob c

fromList :: (CritBitKey k) => [(k, v)] -> CritBit k v
fromList = List.foldl' (flip (uncurry insert)) empty

toList :: CritBit k v -> [(k, v)]
toList (CritBit root) = go root []
  where
    go Empty next              = next
    go (Internal l r _ _) next = go l (go r next)
    go (Leaf k v) next         = (k,v) : next
