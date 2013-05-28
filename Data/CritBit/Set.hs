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
    --  Set

    -- * Operators
    -- , (\\)

    -- * Query
    -- , S.null
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
    -- , empty
    -- , singleton
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
    -- , toList
    -- , fromList

    -- ** Ordered list
    -- , toAscList
    -- , toDescList
    -- , fromAscList
    -- , fromDistinctAscList
    ) where
