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
    -- , size
    -- , member
    -- , notMember
    -- , lookupLT
    -- , lookupGT
    -- , lookupLE
    -- , lookupGE
    -- , isSubsetOf
    -- , isProperSubsetOf

    -- * Construction
    , empty
    , singleton
    -- , insert
    -- , delete

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
toList (Set a) = T.keys a
{-# INLINABLE toList #-}
