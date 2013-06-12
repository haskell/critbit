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
    , (\\)

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
    , union
    , unions
    , difference
    , intersection

    -- * Filter
    , filter
    , partition
    , split
    , splitMember

    -- * Map
    , map
    , mapMonotonic

    -- * Folds
    , foldr
    , foldl
    -- ** Strict folds
    , foldr'
    , foldl'

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , maxView
    , minView

    -- * Conversion

    -- ** List
    , elems
    , toList
    , fromList

    -- ** Ordered list
    -- , toAscList
    -- , toDescList
    -- , fromAscList
    -- , fromDistinctAscList
    ) where

import Control.DeepSeq (NFData)
import Control.Arrow ((***))
import Data.CritBit.Types.Internal (CritBit(..), CritBitKey, Node(..))
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid(..))
import Data.Maybe (isJust)
import Prelude hiding (null, filter, map, foldl, foldr)
import qualified Data.List as List
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

-- | Same as 'difference'.
(\\) :: CritBitKey a => Set a -> Set a -> Set a
s \\ p = difference s p
{-# INLINABLE (\\) #-}

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
{-# INLINABLE empty #-}

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

-- | /O(n)/. An alias of 'toList'. The elements of a set in ascending order.
elems :: Set a -> [a]
elems = toList

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

-- | /O(K)/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: (CritBitKey a) => Set a -> Set a -> Set a
union = (Set .) . liftSS T.union
{-# INLINABLE union #-}

-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: (CritBitKey a) => [Set a] -> Set a
unions = List.foldl' union empty
{-# INLINABLE unions #-}

-- | /O(K)/. The difference of two sets.
difference :: (CritBitKey a) => Set a -> Set a -> Set a
difference = (Set .) . liftSS T.union
{-# INLINABLE difference #-}

-- | /O(K)/. The intersection of two sets. Elements of the
-- result come from the first set.
intersection :: (CritBitKey a) => Set a -> Set a -> Set a
intersection = (Set .) . liftSS T.union
{-# INLINABLE intersection #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
--
-- > filter (> "a") (fromList ["a", "b"]) == fromList [("3","b")]
-- > filter (> "x") (fromList ["a", "b"]) == empty
-- > filter (< "a") (fromList ["a", "b"]) == empty
filter :: (a -> Bool) -> Set a -> Set a
filter = (Set .) . liftVS (T.filterWithKey . (const .))
{-# INLINABLE filter #-}

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (CritBitKey a) => (a -> Bool) -> Set a -> (Set a, Set a)
partition = ((Set *** Set) .) . liftVS (T.partitionWithKey . (const .))
{-# INLINABLE partition #-}

-- | /O(k)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split "a" (fromList ["b", "d"]) == (empty, fromList ["b", "d")])
-- > split "b" (fromList ["b", "d"]) == (empty, singleton "d")
-- > split "c" (fromList ["b", "d"]) == (singleton "b", singleton "d")
-- > split "d" (fromList ["b", "d"]) == (singleton "b", empty)
-- > split "e" (fromList ["b", "d"]) == (fromList ["b", "d"], empty)
split :: (CritBitKey a) => a -> Set a -> (Set a, Set a)
split = ((Set *** Set) .) . liftVS T.split
{-# INLINABLE split #-}

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
--
-- > splitMember "a" (fromList ["b", "d"]) == (empty, False, fromList ["b", "d"])
-- > splitMember "b" (fromList ["b", "d"]) == (empty, True, singleton "d")
-- > splitMember "c" (fromList ["b", "d"]) == (singleton "b", False, singleton "d")
-- > splitMember "d" (fromList ["b", "d"]) == (singleton "b", True, empty)
-- > splitMember "e" (fromList ["b", "d"]) == (fromList ["b", "d"], False, empty)
splitMember :: (CritBitKey a) => a -> Set a -> (Set a, Bool, Set a)
splitMember = (pack .) . liftVS T.splitLookup
  where pack (l, m, r) = (Set l, isJust m, Set r)
{-# INLINABLE splitMember #-}

-- | /O(K)/. @'map' f s@ is the set obtained by applying @f@ to each
-- element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (CritBitKey a2) => (a1 -> a2) -> Set a1 -> Set a2
map = (Set .) . liftVS T.mapKeys
{-# INLINABLE map #-}

-- | /O(n)/. The @'mapMonotonic' f s == 'map' f s@, but works only when
-- @f@ is monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
mapMonotonic :: (CritBitKey a2) => (a1 -> a2) -> Set a1 -> Set a2
mapMonotonic = error "Depends on T.mapKeysMonotonic"
--mapMonotonic = (Set .) . liftVS T.mapKeysMonotonic
{-# INLINABLE mapMonotonic #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> b -> a) -> a -> Set b -> a
foldl f = liftVS (T.foldlWithKey ((const .) . f))
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Set b -> a
foldl' f = liftVS (T.foldlWithKey' ((const .) . f))
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f = liftVS (T.foldrWithKey (const . f))
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Set a -> b
foldr' f = liftVS (T.foldrWithKey' (const . f))
{-# INLINE foldr' #-}

-- | /O(k)/. The minimal element of a set.
findMin :: Set a -> a
findMin = fst . liftS T.findMin
{-# INLINE findMin #-}

-- | /O(k)/. The maximal element of a set.
findMax :: Set a -> a
findMax = fst . liftS T.findMax
{-# INLINE findMax #-}

-- | /O(log n)/. Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: Set a -> Set a
deleteMin = Set . liftS T.deleteMin
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: Set a -> Set a
deleteMax = Set . liftS T.deleteMax
{-# INLINE deleteMax #-}

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: Set a -> (a, Set a)
deleteFindMin = (fst *** Set) . liftS T.deleteFindMin
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Set a -> (a, Set a)
deleteFindMax = (fst *** Set) . liftS T.deleteFindMax
{-# INLINE deleteFindMax #-}

-- | /O(log n)/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Set a -> Maybe (a, Set a)
minView = fmap (fst *** Set) . liftS T.minViewWithKey
{-# INLINE minView #-}

-- | /O(log n)/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Set a -> Maybe (a, Set a)
maxView = fmap (fst *** Set) . liftS T.maxViewWithKey
{-# INLINE maxView #-}

-- | Lifts tree operation to set operation
liftS :: (CritBit a () -> r) -> Set a -> r
liftS f (Set s) = f s
{-# INLINE liftS #-}

-- | Lifts (value, tree) operation to (value, set) operation
liftVS :: (t -> CritBit a () -> r) -> t -> Set a -> r
liftVS = (liftS .)
{-# INLINE liftVS #-}

-- | Lifts (tree, tree) operation to (set, set) operation
liftSS :: (CritBit a () -> CritBit a () -> r) -> Set a -> Set a -> r
liftSS = liftS . (liftS .)
{-# INLINE liftSS #-}
