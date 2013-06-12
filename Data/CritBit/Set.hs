{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Data.CritBit.Set
-- Copyright   :  (c) Bryan O'Sullivan 2013
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A set type that uses crit-bit trees internally.
--
-- For every /n/ key-value pairs stored, a crit-bit tree uses /n/-1
-- internal nodes, for a total of 2/n/-1 internal nodes and leaves.
module Data.CritBit.Set
    (
    -- * Set type
    Set

    -- * Operators
    -- , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    , isSubsetOf
    , isProperSubsetOf

    -- * Construction
    , empty
    , singleton
    , insert
    , delete

    -- * Combine
    -- , union
    -- , unions
    -- , difference
    -- , intersection

    -- * Filter
    -- , S.filter
    -- , partition
    -- , split
    -- , splitMember

    -- * Map
    -- , S.map
    -- , mapMonotonic

    -- * Folds
    -- , S.foldr
    -- , S.foldl
    -- ** Strict folds
    -- , foldr'
    -- , foldl'
    -- ** Legacy folds
    -- , fold

    -- * Min\/Max
    -- , findMin
    -- , findMax
    -- , deleteMin
    -- , deleteMax
    -- , deleteFindMin
    -- , deleteFindMax
    -- , maxView
    -- , minView

    -- * Conversion

    -- ** List
    -- , elems
    , toList
    , fromList

    -- ** Ordered list
    -- , toAscList
    -- , toDescList
    -- , fromAscList
    -- , fromDistinctAscList
    ) where

import Control.DeepSeq (NFData)
import Data.CritBit.Types.Internal (CritBit(..), CritBitKey, Node(..))
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid(..))
import Prelude hiding (null)
import qualified Data.CritBit.Tree as T

-- | A set based on crit-bit trees.
newtype Set a = Set (CritBit a ())
    deriving (Eq, NFData)

instance (Show a) => Show (Set a) where
    show s = "fromList " ++ show (toList s)

instance Foldable Set where
    foldMap f (Set (CritBit n)) = foldSet f n

foldSet :: (Monoid m) => (a -> m) -> Node a () -> m
foldSet f (Internal l r _ _) = mappend (foldSet f l) (foldSet f r)
foldSet f (Leaf k _)         = f k
foldSet _ Empty              = mempty
{-# INLINABLE foldSet #-}

-- | /O(1)/. Is the set empty?
--
-- > null (empty)         == True
-- > null (singleton "a") == False
null :: Set a -> Bool
null (Set a) = T.null a

-- | /O(1)/. The empty set.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: Set a
empty = Set $ T.empty

-- | /O(1)/. A set with a single element.
--
-- > singleton "a"        == fromList ["a"]
singleton :: a -> Set a
singleton a = Set $ T.singleton a ()
{-# INLINE singleton #-}

-- | /O(n*log n)/. Build a set from a list of values.
--
-- > fromList [] == empty
-- > fromList ["a", "b", "a"] == fromList ["a", "b"]
fromList :: (CritBitKey a) => [a] -> Set a
fromList xs = Set . T.fromList . zip xs . repeat $ ()
{-# INLINABLE fromList #-}

-- | /O(n)/. Convert the set to a list of values. The list returned
-- will be sorted in lexicographically ascending order.
--
-- > toList (fromList ["b", "a"]) == ["a", "b"]
-- > toList empty == []
toList :: Set a -> [a]
toList = liftS T.keys
{-# INLINABLE toList #-}

-- | /O(n)/. The number of elements in the set.
--
-- > size empty                      == 0
-- > size (singleton "a")            == 1
-- > size (fromList ["a", "c", "b"]) == 3
size :: Set a -> Int
size = liftS T.size
{-# INLINABLE size #-}

-- | /O(k)/. Is the element in the set?
--
-- > member "a" (fromList ["a", "b"]) == True
-- > member "c" (fromList ["a", "b"]) == False
--
-- See also 'notMember'.
member :: (CritBitKey a) => a -> Set a -> Bool
member a (Set s) = T.member a s
{-# INLINABLE member #-}

-- | /O(k)/. Is the element not in the set?
--
-- > notMember "a" (fromList ["a", "b"]) == False
-- > notMember "c" (fromList ["a", "b"]) == True
--
-- See also 'member'.
notMember :: (CritBitKey a) => a -> Set a -> Bool
notMember a (Set s) = T.notMember a s
{-# INLINABLE notMember #-}

-- | /O(k)/. Find largest element smaller than the given one.
--
-- > lookupLT "b"  (fromList ["a", "b"]) == Just "a"
-- > lookupLT "aa" (fromList ["a", "b"]) == Just "a"
-- > lookupLT "a"  (fromList ["a", "b"]) == Nothing
lookupLT :: (CritBitKey a) => a -> Set a -> Maybe a
lookupLT = (fmap fst .) . liftVS T.lookupLT
{-# INLINABLE lookupLT #-}

-- | /O(k)/. Find smallest element greater than the given one.
--
-- > lookupGT "b"  (fromList ["a", "b"]) == Nothing
-- > lookupGT "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGT "a"  (fromList ["a", "b"]) == Just "b"
lookupGT :: (CritBitKey a) => a -> Set a -> Maybe a
lookupGT = (fmap fst .) . liftVS T.lookupGT
{-# INLINABLE lookupGT #-}

-- | /O(k)/. Find lagest element smaller than or equal to the given one.
--
-- > lookupGE "b"  (fromList ["a", "b"]) == Just "b"
-- > lookupGE "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGE "a"  (fromList ["a", "b"]) == Just "a"
-- > lookupGE ""   (fromList ["a", "b"]) == Nothing
lookupLE :: (CritBitKey a) => a -> Set a -> Maybe a
lookupLE = (fmap fst .) . liftVS T.lookupLE
{-# INLINABLE lookupLE #-}

-- | /O(k)/. Find smallest element greater than or equal to the given one.
--
-- > lookupGE "aa" (fromList ["a", "b"]) == Just "b"
-- > lookupGE "b"  (fromList ["a", "b"]) == Just "b"
-- > lookupGE "bb" (fromList ["a", "b"]) == Nothing
lookupGE :: (CritBitKey a) => a -> Set a -> Maybe a
lookupGE = (fmap fst .) . liftVS T.lookupGE
{-# INLINABLE lookupGE #-}

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: (CritBitKey a) => Set a -> Set a -> Bool
isSubsetOf = liftSS T.isSubmapOf
{-# INLINABLE isSubsetOf #-}

-- | /O(n+m)/. Is this a proper subset (ie. a subset but not equal)?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a proper subset of @s2@.
isProperSubsetOf :: (CritBitKey a) => Set a -> Set a -> Bool
isProperSubsetOf = liftSS T.isProperSubmapOf
{-# INLINABLE isProperSubsetOf #-}

-- | /O(k)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: (CritBitKey a) => a -> Set a -> Set a
insert = (Set .) . liftVS (flip T.insert ())
{-# INLINABLE insert #-}

-- | /O(log n)/. Delete an element from a set.
delete :: (CritBitKey a) => a -> Set a -> Set a
delete = (Set .) . liftVS T.delete
{-# INLINABLE delete #-}

-- | Lifts tree operation to set operation
liftS :: (CritBit a () -> r) -> Set a -> r
liftS f (Set s) = f s
{-# INLINE liftS #-}

-- | Lifts (value, tree) operation to (value, set) operation
liftVS :: (a -> CritBit a () -> r) -> a -> Set a -> r
liftVS = (liftS .)
{-# INLINE liftVS #-}

-- | Lifts (tree, tree) operation to (set, set) operation
liftSS :: (CritBit a () -> CritBit a () -> r) -> Set a -> Set a -> r
liftSS = liftS . (liftS .)
{-# INLINE liftSS #-}
