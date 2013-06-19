{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Set
    where

import Properties.Common
import Data.CritBit.Map.Lazy (CritBitKey, byteCount)
import qualified Data.CritBit.Set as C
import qualified Data.Set as S

import Test.QuickCheck (Arbitrary(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.String (IsString, fromString)
import Data.List (unfoldr, sort, nub)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

kp :: (CritBitKey k) => k -> Bool
kp = even . byteCount

kf :: (CritBitKey k, Show k, IsString k) => k -> k
kf k = fromString $ show (byteCount k) ++ show k

kii :: (CritBitKey k, Show k, IsString k) => k -> Int -> Int
kii k v = byteCount k * 13 + v

fmono :: (CritBitKey k, Show k, IsString k) => k -> k
fmono k = fromString $ "test" ++ show k

propertiesFor :: (Arbitrary k, CritBitKey k, Eq k, Ord k, Show k, IsString k)
              => k -> [Test]
propertiesFor t = concat [[]
  -- * Operators
  , prop "t_diff" $ (C.\\) =**= (S.\\)

  -- * Query
  , prop "t_null" $ C.null =*= S.null
  , prop "t_size" $ C.size =*= S.size
  , prop "t_member" $ C.member =?*= S.member
  , prop "t_notMember" $ C.notMember =?*= S.notMember
#if MIN_VERSION_containers(0,5,0)
  , prop "t_lookupLT" $ C.lookupLT =?*= S.lookupLT
  , prop "t_lookupGT" $ C.lookupGT =?*= S.lookupGT
  , prop "t_lookupLE" $ C.lookupLE =?*= S.lookupLE
  , prop "t_lookupGE" $ C.lookupGE =?*= S.lookupGE
#endif
  , prop "t_isSubsetOf" $ C.isSubsetOf =**= S.isSubsetOf
  , prop "t_isProperSubsetOf" $ C.isProperSubsetOf =**= S.isProperSubsetOf

  -- * Construction
--  , prop "t_empty" $ C.empty =^= S.empty
  , prop "t_singleton" $ notEmpty $ (C.singleton =*== S.singleton) head
  , prop "t_insert" $ C.insert =?*= S.insert
  , prop "t_delete" $ C.delete =?*= S.delete

  -- * Combine
  , prop "t_union" $ C.union =**= S.union
  , prop "t_unions" $ (C.unions . map C.fromList =*==
                         S.unions . map S.fromList) fromSmall
  , prop "t_difference" $ C.difference =**= S.difference
  , prop "t_intersection" $ C.intersection =**= S.intersection

  -- * Filter
  , prop "t_filter" $ C.filter kp =*= S.filter kp
  , prop "t_partition" $ C.partition kp =*= S.partition kp
  , prop "t_split" $ C.split =?*= S.split
  , prop "t_splitMember" $ C.splitMember =?*= S.splitMember

  -- * Map
  , prop "t_map" $ C.map kf =*= S.map kf
  , prop "t_mapMonotonic" $ C.mapMonotonic fmono =*= S.mapMonotonic fmono

  -- * Folds
  , prop "t_foldr" $ C.foldr kii 0 =*= S.foldr kii 0
  , prop "t_foldl" $ C.foldl (flip kii) 0 =*= S.foldl (flip kii) 0
  -- ** Strict folds
  , prop "t_foldr'" $ C.foldr' kii 0 =*= S.foldr' kii 0
  , prop "t_foldl'" $ C.foldl' (flip kii) 0 =*= S.foldl' (flip kii) 0

  -- * Min\/Max
  , prop "t_findMin" $ notEmpty $ C.findMin =*= S.findMin
  , prop "t_findMax" $ notEmpty $ C.findMax =*= S.findMax
  , prop "t_deleteMin" $ notEmpty $ C.deleteMin =*= S.deleteMin
  , prop "t_deleteMax" $ notEmpty $ C.deleteMax =*= S.deleteMax
  , prop "t_deleteFindMin" $ notEmpty $ C.deleteFindMin =*= S.deleteFindMin
  , prop "t_deleteFindMax" $ notEmpty $ C.deleteFindMax =*= S.deleteFindMax
  , prop "t_maxView" $ notEmpty $ unfoldr C.maxView =*= unfoldr S.maxView
  , prop "t_minView" $ notEmpty $ unfoldr C.minView =*= unfoldr S.minView

  -- * Conversion
  -- ** List
  , prop "t_elems" $ C.elems =*= S.elems
  , prop "t_toList" $ C.toList =*= S.toList
  , prop "t_fromList" $ (C.fromList =*== S.fromList) id

  -- ** Ordered list
  , prop "t_toAscList" $ C.toAscList =*= S.toAscList
#if MIN_VERSION_containers(0,5,0)
  , prop "t_toDescList" $ C.toDescList =*= S.toDescList
#endif
  , prop "t_fromAscList" $ (C.fromAscList =*== S.fromAscList) sort
  , prop "t_fromDistinctAscList" $
     (C.fromDistinctAscList =*== S.fromDistinctAscList) (nub . sort)
  ]
  where
    prop name q = [testProperty name $ q $ sameAs t]

    sameAs :: (CritBitKey k, Ord k) => k -> SameAs (C.Set k) (S.Set k) [k]
    sameAs _ = SameAs C.fromList C.toList S.fromList S.toList

properties :: [Test]
properties = [
    testGroup "text" $ propertiesFor T.empty
  , testGroup "bytestring" $ propertiesFor B.empty
  ]

instance (Eq k) => Eq' (C.Set k) (S.Set k) where
   c =^= m = C.toList c =^= S.toList m

instance (Eq' a1 b1, Eq k) => Eq' (a1, C.Set k) (b1, S.Set k) where
  (a1, a2) =^= (b1, b2) = a1 =^= b1 && a2 =^= b2

-- Handy functions for fiddling with from ghci.

blist :: [B.ByteString] -> C.Set B.ByteString
blist = C.fromList

tlist :: [T.Text] -> S.Set T.Text
tlist = S.fromList
