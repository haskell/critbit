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
    -- * Operators
    -- , (!)
    -- , (\\)

    -- * Query
      null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupGT
    -- , lookupGE

    -- * Construction
    , empty
    , singleton

    -- * Insertion
    , insert
    -- , insertWith
    -- , insertWithKey
    -- , insertLookupWithKey

    -- * Deletion
    , delete
    -- , adjust
    -- , adjustWithKey
    -- , update
    -- , updateWithKey
    -- , updateLookupWithKey
    -- , alter

    -- * Combination
    -- ** Union
    , union
    -- , unionWith
    -- , unionWithKey
    -- , unions
    -- , unionsWith
    , unionL
    , unionR

    -- ** Difference
    -- , difference
    -- , differenceWith
    -- , differenceWithKey

    -- ** Intersection
    -- , intersection
    -- , intersectionWith
    -- , intersectionWithKey

    -- * Traversal
    -- ** Map
    , map
    -- , mapWithKey
    -- , traverseWithKey
    -- , mapAccum
    -- , mapAccumWithKey
    -- , mapAccumRWithKey
    -- , mapKeys
    -- , mapKeysWith
    -- , mapKeysMonotonic

    -- * Folds
    , foldl
    , foldr
    , foldlWithKey
    , foldrWithKey

    -- ** Strict folds
    , foldl'
    , foldr'
    , foldlWithKey'
    , foldrWithKey'

    -- * Conversion
    , elems
    , keys
    -- , assocs
    -- , keysSet
    -- , fromSet

    -- ** Lists
    , toList
    , fromList
    -- , fromListWith
    -- , fromListWithKey

    -- ** Ordered lists
    -- , toAscList
    -- , toDescList
    -- , fromAscList
    -- , fromAscListWith
    -- , fromAscListWithKey
    -- , fromDistinctAscList

    -- * Filter
    -- , filter
    -- , filterWithKey
    -- , partition
    -- , partitionWithKey

    -- , mapMaybe
    -- , mapMaybeWithKey
    -- , mapEither
    -- , mapEitherWithKey

    -- , split
    -- , splitLookup

    -- * Submap
    -- , isSubmapOf
    -- , isSubmapOfBy
    -- , isProperSubmapOf
    -- , isProperSubmapOfBy

    -- -- * Min\/Max
    , findMin
    , findMax
    -- , deleteMin
    -- , deleteMax
    -- , deleteFindMin
    -- , deleteFindMax
    -- , updateMin
    -- , updateMax
    -- , updateMinWithKey
    -- , updateMaxWithKey
    -- , minView
    -- , maxView
    -- , minViewWithKey
    -- , maxViewWithKey
    ) where

import Data.CritBit.Core
import Data.CritBit.Types.Internal
import Prelude hiding (foldl, foldr, lookup, null, map)
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
-- > member "a" (fromList [("a",5), ("b",3)]) == True
-- > member "c" (fromList [("a",5), ("b",3)]) == False
--
-- See also 'notMember'.
member :: (CritBitKey k) => k -> CritBit k v -> Bool
member k m = lookupWith False (const True) k m
{-# INLINABLE member #-}

-- | /O(log n)/. Is the key not a member of the map?
--
-- > notMember "a" (fromList [("a",5), ("b",3)]) == False
-- > notMember "c" (fromList [("a",5), ("b",3)]) == True
--
-- See also 'member'.
notMember :: (CritBitKey k) => k -> CritBit k v -> Bool
notMember k m = lookupWith True (const False) k m
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
lookup k m = lookupWith Nothing Just k m
{-# INLINABLE lookup #-}

-- | /O(log n)/. Returns the value associated with the given key, or
-- the given default value if the key is not in the map.
--
-- > findWithDefault 1 "x" (fromList [("a",5), ("b",3)]) == 1
-- > findWithDefault 1 "a" (fromList [("a",5), ("b",3)]) == 5
findWithDefault :: (CritBitKey k) =>
                   v -- ^ Default value to return if lookup fails.
                -> k -> CritBit k v -> v
findWithDefault d k m = lookupWith d id k m
{-# INLINABLE findWithDefault #-}

-- | /O(log n)/. Find smallest key greater than the given one and
-- return the corresponding (key, value) pair.
--
-- > lookupGT "aa" (fromList [("a",3), ("b",5)]) == Just ("b",5)
-- > lookupGT "b"  (fromList [("a",3), ("b",5)]) == Nothing
lookupGT :: (CritBitKey k) => k -> CritBit k v -> Maybe (k, v)
lookupGT k (CritBit root) = go root
  where
    go i@(Internal left right _ _)
      | direction k i == 0 = go left
      | otherwise          = go right
    go (Leaf lk lv)        = rewalk root
      where
        finish (Leaf _ _) = case byteCompare k lk of
                              LT -> Just (lk, lv)
                              _ -> Nothing
        finish node
          | calcDirection nob c == 0 = Nothing
          | otherwise                = leftmost node
        rewalk i@(Internal left right byte otherBits)
          | byte > n                     = finish i
          | byte == n && otherBits > nob = finish i
          | direction k i == 0           = case rewalk left of
                                             Nothing -> leftmost right
                                             wat     -> wat
          | otherwise                    = rewalk right
        rewalk i                         = finish i
        (n, nob, c) = followPrefixes k lk
    go Empty = Nothing
    leftmost (Internal left _ _ _) = leftmost left
    leftmost (Leaf lmk lmv)        = Just (lmk, lmv)
    leftmost _                     = Nothing
{-# INLINABLE lookupGT #-}

byteCompare :: (CritBitKey k) => k -> k -> Ordering
byteCompare a b = go 0
  where
    go i = case ba `compare` getByte b i of
             EQ | ba /= 0   -> go (i + 1)
             wat            -> wat
      where ba = getByte a i
{-# INLINABLE byteCompare #-}

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

-- | /O(n)/. Fold the values in the map using the given
-- left-associative function, such that
-- @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- Examples:
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > foldl (+) 0 (fromList [("a",5), ("bbb",3)]) == 8
foldl :: (a -> v -> a) -> a -> CritBit k v -> a
foldl f z m = foldlWithKeyWith (\_ b -> b) (\a _ v -> f a v) z m
{-# INLINABLE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the
-- function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> CritBit k v -> a
foldl' f z m = foldlWithKeyWith seq (\a _ v -> f a v) z m
{-# INLINABLE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given
-- left-associative function, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- Examples:
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ show k ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [("a",5), ("b",3)]) == "Map: (b:3)(a:5)"
foldlWithKey :: (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKey f z m = foldlWithKeyWith (\_ b -> b) f z m
{-# INLINABLE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of
-- the function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKey' f z m = foldlWithKeyWith seq f z m
{-# INLINABLE foldlWithKey' #-}

foldlWithKeyWith :: (a -> a -> a) -> (a -> k -> v -> a) -> a -> CritBit k v -> a
foldlWithKeyWith maybeSeq f z0 (CritBit root) = go z0 root
  where
    go z (Internal left right _ _) = let z' = go z left
                                     in z' `maybeSeq` go z' right
    go z (Leaf k v)                = f z k v
    go z Empty                     = z
{-# INLINE foldlWithKeyWith #-}

-- | /O(n)/. Fold the values in the map using the given
-- right-associative function, such that
-- @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- Example:
--
-- > elems map = foldr (:) [] map
foldr :: (v -> a -> a) -> a -> CritBit k v -> a
foldr f z m = foldrWithKeyWith (\_ b -> b) (\_ v a -> f v a) z m
{-# INLINABLE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the
-- function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldr' :: (v -> a -> a) -> a -> CritBit k v -> a
foldr' f z m = foldrWithKeyWith seq (\_ v a -> f v a) z m
{-# INLINABLE foldr' #-}

-- | /O(n)/. Fold the keys and values in the map using the given
-- right-associative function, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- Examples:
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [("a",5), ("b",3)]) == "Map: (a:5)(b:3)"
foldrWithKey :: (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKey f z m = foldrWithKeyWith (\_ b -> b) f z m
{-# INLINABLE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of
-- the function is evaluated before using the result in the next
-- application. This function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKey' f z m = foldrWithKeyWith seq f z m
{-# INLINABLE foldrWithKey' #-}

foldrWithKeyWith :: (a -> a -> a) -> (k -> v -> a -> a) -> a -> CritBit k v -> a
foldrWithKeyWith maybeSeq f z0 (CritBit root) = go root z0
  where
    go (Internal left right _ _) z = let z' = go right z
                                     in z' `maybeSeq` go left z'
    go (Leaf k v) z                = f k v z
    go Empty z                     = z
{-# INLINE foldrWithKeyWith #-}

-- | /O(n)/. Return all the elements of the map in ascending order of their keys.
--
-- > elems (fromList [("b",5), ("a",3)]) == [3,5]
-- > elems empty == []
elems :: CritBit k v -> [v]
elems m = foldrWithKey f [] m 
  where f _ v vs = v : vs

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [("b",5), ("a",3)]) == ["a","b"]
-- > keys empty == []
keys :: CritBit k v -> [k]
keys m = foldrWithKey f [] m
  where f k _ ks = k : ks

unionL :: (CritBitKey k) => CritBit k v -> CritBit k v -> CritBit k v
unionL a b = foldlWithKey' (\m k v -> insert k v m) b a
{-# INLINABLE unionL #-}

unionR :: (CritBitKey k) => CritBit k v -> CritBit k v -> CritBit k v
unionR a b = foldlWithKey' (\m k v -> insert k v m) a b
{-# INLINABLE unionR #-}

union :: (CritBitKey k) => CritBit k v -> CritBit k v -> CritBit k v
union a b = unionL a b
{-# INLINE union #-}

-- | /O(n)/. Apply a function to all values.
--
-- > map show (fromList [("b",5), ("a",3)]) == fromList [("b","5"), ("a","3")]
map :: (CritBitKey k) => (v -> w) -> CritBit k v -> CritBit k w
map = fmap

-- | /O(log n)/. The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [("b",3), ("a",5)]) == ("a",5)
-- > findMin empty                            Error: empty map has no minimal element
findMin :: CritBit k v -> (k,v)
findMin (CritBit root) = go root
  where
    go (Leaf k v) = (k,v)
    go (Internal left _ _ _) = go left
    go Empty = error "CritBit.findMin: empty map has no minimal element"
{-# INLINABLE findMin #-}

-- | /O(log n)/. The maximal key of the map. Calls 'error' if the map si empty.
--
-- > findMax (fromList [("b",3), ("a",5)]) == ("b",3)
-- > findMax empty                            Error: empty map has no minimal element
findMax :: CritBit k v -> (k,v)
findMax (CritBit root) = go root
  where
    go (Leaf k v) = (k,v)
    go (Internal _ right _ _) = go right
    go Empty = error "CritBit.findMax: empty map has no maximal element"
{-# INLINABLE findMax #-}
