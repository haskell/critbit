{-# LANGUAGE BangPatterns, RecordWildCards, ScopedTypeVariables #-}

-- |
-- Module      :  Data.CritBit.Tree
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
module Data.CritBit.Tree
    (
      null
    , singleton
    , empty
    , size
    , fromList
    , toList
    , insert
    , delete
    , lookup
    , member
    , notMember
    ) where

import Data.Bits ((.|.), (.&.), complement, shiftR, xor)
import Data.CritBit.Types.Internal (CritBitKey(..), CritBit(..), Node(..))
import Data.Word (Word16)
import Prelude hiding (lookup, null)
import qualified Data.List as List

-- | /O(1)/. Is the map empty?
--
-- > null (empty)           == True
-- > null (singleton 1 'a') == False
null :: CritBit k v -> Bool
null (CritBit Empty) = True
null _               = False

-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: CritBit k v
empty = CritBit { cbRoot = Empty }

-- | /O(log n)/. Is the key a member of the map?
--
-- > member "a" (fromList [("a", 5), ("b", 3)]) == True
-- > member "c" (fromList [("a", 5), ("b", 3)]) == False
--
-- See also 'notMember'.
member :: (CritBitKey k) => k -> CritBit k v -> Bool
member k (CritBit root) = go root
  where
    go i@(Internal left right _ _)
       | direction k i == 0  = go left
       | otherwise           = go right
    go (Leaf lk _) | k == lk = True
    go _                     = False
{-# INLINABLE member #-}

-- | /O(log n)/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True
--
-- See also 'member'.
notMember :: (CritBitKey k) => k -> CritBit k v -> Bool
notMember k m = not (member k m)
{-# INLINE notMember #-}

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Text
-- > import Prelude hiding (lookup)
-- > import Data.CritBit.Map.Lazy
-- >
-- > employeeDept, deptCountry, countryCurrency :: CritBit Text Text
-- > employeeDept = fromList [("John","Sales"), ("Bob","IT")]
-- > deptCountry = fromList [("IT","USA"), ("Sales","France")]
-- > countryCurrency = fromList [("USA", "Dollar"), ("France", "Euro")]
-- >
-- > employeeCurrency :: Text -> Maybe Text
-- > employeeCurrency name = do
-- >   dept <- lookup name employeeDept
-- >   country <- lookup dept deptCountry
-- >   lookup country countryCurrency
-- >
-- > main = do
-- >   putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >   putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: (CritBitKey k) => k -> CritBit k v -> Maybe v
lookup k (CritBit root) = go root
  where
    go i@(Internal left right _ _)
       | direction k i == 0  = go left
       | otherwise           = go right
    go (Leaf lk v) | k == lk = Just v
    go _                     = Nothing
{-# INLINABLE lookup #-}

direction :: (CritBitKey k) => k -> Node k v -> Int
direction k (Internal _ _ byte otherBits) =
    calcDirection otherBits (getByte k byte)
direction _ _ = error "Data.CritBit.Tree.direction: unpossible!"
{-# INLINE direction #-}

calcDirection :: Word16 -> Word16 -> Int
calcDirection otherBits c = (1 + fromIntegral (otherBits .|. c)) `shiftR` 9
{-# INLINE calcDirection #-}

followPrefixes :: (CritBitKey k) => k -> k -> (Int, Word16, Word16)
followPrefixes k l = go 0
  where
    go n | n == byteCount k = (n, c, c)
         | n == byteCount l = (n, b, 0)
         | b /= c           = (n, b `xor` c, c)
         | otherwise        = go (n+1)
      where b = getByte k n
            c = getByte l n
{-# INLINE followPrefixes #-}

-- | /O(log n)/. Insert a new key and value in the map.  If the key is
-- already present in the map, the associated value is replaced with
-- the supplied value. 'insert' is equivalent to @'insertWith'
-- 'const'@.
--
-- > insert "x" 5 (fromList [("a",5), ("b",3)]) == fromList [("b",3), ("x",5)]
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
                  n3 = n2 .|. (n2 `shiftR` 8)
              in (n3 .&. (complement (n3 `shiftR` 1))) `xor` 511
        nd = calcDirection nob c
    go Empty = Leaf k v
{-# INLINABLE insert #-}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete "a" (fromList [("a",5), ("b",3)]) == singleton "b" 3
-- > delete "c" (fromList [("a",5), ("b",3)]) == fromList [("a",5), ("b",3)]
-- > delete "a" empty                         == empty
delete :: (CritBitKey k) => k -> CritBit k v -> CritBit k v
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

-- | /O(n*log n)/. Build a map from a list of key\/value pairs.  If
-- the list contains more than one value for the same key, the last
-- value for the key is retained.
--
-- > fromList [] == empty
-- > fromList [("a",5), ("b",3), ("a",2)] == fromList [("a",2), ("b",3)]
fromList :: (CritBitKey k) => [(k, v)] -> CritBit k v
fromList = List.foldl' (flip (uncurry insert)) empty
{-# INLINABLE fromList #-}

-- | /O(1)/. A map with a single element.
--
-- > singleton "a" 1        == fromList [("a", 1)]
singleton :: k -> v -> CritBit k v
singleton k v = CritBit (Leaf k v)
{-# INLINE singleton #-}

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

-- | /O(n)/. The number of elements in the map.
--
-- > size empty                                  == 0
-- > size (singleton "a" 1)                      == 1
-- > size (fromList [("a",1), ("c",2), ("b",3)]) == 3
size :: CritBit k v -> Int
size (CritBit root) = go root
  where
    go (Internal l r _ _) = go l + go r
    go (Leaf _ _) = 1
    go Empty      = 0
