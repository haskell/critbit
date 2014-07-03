{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Map
    where

import qualified Data.ByteString.Char8 as B
import Data.CritBit.Map.Lazy (CritBitKey, CritBit, byteCount)
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.CritBit.Set as CSet
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (unfoldr, sort, nubBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word8)
import Properties.Common
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Property ((.&&.))

--only needed for a test requiring containers >= 0.5
#if MIN_VERSION_containers(0,5,0)
import Data.Functor.Identity (Identity(..))
#endif

type V = Word8

-- * Common modifier functions

kvvf :: (CritBitKey k) => k -> V -> V -> V
kvvf k v1 v2 = toEnum (byteCount k) * 3 + v1 * 2 - v2

kvvfm :: (CritBitKey k) => k -> V -> V -> Maybe V
kvvfm k v1 v2 = if even v1 then Just (kvvf k v1 v2) else Nothing

kvf :: (CritBitKey k) => k -> V -> V
kvf k v = kvvf k v 0

kvfm :: (CritBitKey k) => k -> V -> Maybe V
kvfm k v = kvvfm k v 0

vvfm :: V -> V -> Maybe V
vvfm = kvvfm ("" :: T.Text)

vfm :: V -> Maybe V
vfm = kvfm ("" :: T.Text)

propertiesFor :: Props k
propertiesFor w = concat [[]
  -- ** Lists
  , prop sa "t_fromList" $
        (C.fromList =*== Map.fromList) id
  , prop sa "t_fromListWith" $
        (C.fromListWith (-) =*== Map.fromListWith (-)) id
  , prop sa "t_fromListWithKey" $
        (C.fromListWithKey kvvf =*== Map.fromListWithKey kvvf) id

    -- * Query
  , prop sa "t_null" $
        C.null =*= Map.null
  , prop sa "t_size" $
        C.size =*= Map.size
  , prop sa "t_member" $
        C.member =?*= Map.member
  , prop sa "t_member" $
        C.notMember =?*= Map.notMember
  , prop sa "t_lookup" $
        C.lookup =?*= Map.lookup
  , prop sa "t_findWithDefault" $
        C.findWithDefault =??*= Map.findWithDefault

#if MIN_VERSION_containers(0,5,0)
  , prop sa "t_lookupGT" $
        C.lookupGT =?*= Map.lookupGT
  , prop sa "t_lookupGE" $
        C.lookupGE =?*= Map.lookupGE
  , prop sa "t_lookupLT" $
        C.lookupLT =?*= Map.lookupLT
  , prop sa "t_lookupLE" $
        C.lookupLE =?*= Map.lookupLE
#endif

  -- * Insertion
  , pmprop sa "t_insert" $
        C.insert =??*= Map.insert
  , pmprop sa "t_insertWith" $
        C.insertWith (-) =??*= Map.insertWith (-)
  , pmprop sa "t_insertWithKey" $
        C.insertWithKey kvvf =??*= Map.insertWithKey kvvf
  , pmprop sa "t_insertLookupWithKey" $
        C.insertLookupWithKey kvvf =??*= Map.insertLookupWithKey kvvf

  -- * Deletion
  , pmprop sa "t_delete" $
        C.delete =?*= Map.delete
  , pmprop sa "t_adjust" $
        C.adjust (+3) =?*= Map.adjust (+3)
  , pmprop sa "t_adjustWithKey" $
        C.adjustWithKey kvf =?*= Map.adjustWithKey kvf
  , pmprop sa "t_update" $
        C.update vfm =?*= Map.update vfm
  , pmprop sa "t_updateWithKey" $
        C.updateWithKey kvfm =?*= Map.updateWithKey kvfm
  , pmprop sa "t_updateLookupWithKey" $
        C.updateLookupWithKey kvfm =?*= Map.updateLookupWithKey kvfm
  , prop sa "t_alter" $
        let f = Just . maybe 1 (+1)
        in C.alter f =?*= Map.alter f
  , prop sa "t_alter_delete" $
        C.alter (const Nothing) =?*= Map.alter (const Nothing)

  -- * Combination
  -- ** Union
  , prop sa "t_union" $
        C.union =**= Map.union
  , prop sa "t_unionWith" $
        C.unionWith (-) =**= Map.unionWith (-)
  , prop sa "t_unionWithKey" $
        C.unionWithKey kvvf =**= Map.unionWithKey kvvf
  , prop sa "t_unions" $
        (  C.unions . map   C.fromList =*==
         Map.unions . map Map.fromList) fromSmall
  , prop sa "t_unionsWith" $
        (  C.unionsWith (-) . map   C.fromList =*==
         Map.unionsWith (-) . map Map.fromList) fromSmall
  , prop sa "t_unionL" $
        C.unionL =**= Map.union
  , prop sa "t_unionR" $
        C.unionR =**= flip Map.union

  -- ** Difference
  , prop sa "t_difference" $
        C.difference =**= Map.difference
  , prop sa "t_differenceWith" $
        C.differenceWith vvfm =**= Map.differenceWith vvfm
  , prop sa "t_differenceWithKey" $
        C.differenceWithKey kvvfm =**= Map.differenceWithKey kvvfm

  -- ** Intersection
  , prop sa "t_intersection" $
        C.intersection =**= Map.intersection
  , prop sa "t_intersectionWith" $
        C.intersectionWith (-) =**= Map.intersectionWith (-)
  , prop sa "t_intersectionWithKey" $
        C.intersectionWithKey kvvf =**= Map.intersectionWithKey kvvf

  -- * Traversal
  -- ** Map
  , prop sa "t_map" $
        C.map (+3) =*= Map.map (+3)
  , prop sa "t_mapWithKey" $
        C.mapWithKey kvf =*= Map.mapWithKey kvf
#if MIN_VERSION_containers(0,5,0)
  , prop sa "t_traverseWithKey" $
      let f _ = Identity . show . (+3)
      in runIdentity . C.traverseWithKey f =*= runIdentity . Map.traverseWithKey f
#endif
  , prop sa "t_mapAccum" $
        let f i v = (i + 1 :: Int, show $ v + 3)
        in C.mapAccum f 0 =*= Map.mapAccum f 0
  , prop sa "t_mapAccumWithKey" $
        let f i k v = (i + byteCount k, show $ v + 3)
        in C.mapAccumWithKey f 0 =*= Map.mapAccumWithKey f 0
  , prop sa "t_mapAccumRWithKey" $
        let f i k v = (i + byteCount k, show $ v + 3)
        in C.mapAccumRWithKey f 0 =*= Map.mapAccumRWithKey f 0
  , prop sa "t_mapKeys" $
        C.mapKeys kf =*= Map.mapKeys kf
  , prop sa "t_mapKeysWith" $
        C.mapKeysWith (+) kf =*= Map.mapKeysWith (+) kf
  , prop sa "t_mapKeysMonotonic" $
        C.mapKeysMonotonic prepends =*= Map.mapKeysMonotonic prepends

  -- * Folds
  , prop sa "t_foldl" $
        C.foldl (-) 0 =*= Map.foldl (-) 0
  , prop sa "t_foldlWithKey" $
        let f i k v = i * 37 + (byteCount k) * 17 + fromIntegral v
        in C.foldlWithKey f 0 =*= Map.foldlWithKey f 0
  , prop sa "t_foldr" $
        C.foldr (-) 0 =*= Map.foldr (-) 0
  , prop sa "t_foldrWithKey" $
        let f k v i = i * 37 + (byteCount k) * 17 + fromIntegral v
        in C.foldrWithKey f 0 =*= Map.foldrWithKey f 0

  -- ** Strict folds
  , prop sa "t_foldl'" $
        C.foldl' (-) 0 =*= Map.foldl' (-) 0
  , prop sa "t_foldlWithKey'" $
        let f i k v = i * 37 + (byteCount k) * 17 + fromIntegral v
        in C.foldlWithKey' f 0 =*= Map.foldlWithKey' f 0
  , prop sa "t_foldr'" $
        C.foldr' (-) 0 =*= Map.foldr' (-) 0
  , prop sa "t_foldrWithKey'" $
        let f k v i = i * 37 + (byteCount k) * 17 + fromIntegral v
        in C.foldrWithKey' f 0 =*= Map.foldrWithKey' f 0

  -- * Conversion
  , prop sa "t_elems" $
        C.elems =*= Map.elems
  , prop sa "t_keys" $
        C.keys =*= Map.keys
  , prop sa "assocs" $
        C.assocs =*= Map.assocs
  , prop sa "t_keysSet" $
        CSet.toList . C.keysSet =*= Set.toList . Map.keysSet
#if MIN_VERSION_containers(0,5,0)
  , prop sa "t_fromSet" $
        let f = length . show
        in C.fromSet f . C.keysSet =*= Map.fromSet f . Map.keysSet
#endif

  -- ** Ordered lists
  , prop sa "t_toAscList" $
        C.toAscList =*= Map.toAscList
  , prop sa "t_toDescList" $
        C.toDescList =*= Map.toDescList
  , prop sa "t_fromAscList" $
        (C.fromAscList =*== Map.fromAscList) sort
  , prop sa "t_fromAscListWith" $
        (C.fromAscListWith (+) =*== Map.fromAscListWith (+)) sort
  , prop sa "t_fromAscListWithKey" $
        (C.fromAscListWithKey kvvf =*== Map.fromAscListWithKey kvvf) sort
  , prop sa "t_fromDistinctAscList" $
        let p = nubBy ((==) `on` fst) . sort
        in (C.fromDistinctAscList =*== Map.fromDistinctAscList) p

  -- * Filter
  , prop sa "t_filter" $
        C.filter odd =*= Map.filter odd
  , prop sa "t_filterWithKey" $
        let p k v = odd $ kvf k v
        in C.filterWithKey p =*= Map.filterWithKey p
  , prop sa "t_partition" $ C.partition odd =*= Map.partition odd
  , prop sa "t_partitionWithKey" $
       let p k v = odd $ kvf k v
       in C.partitionWithKey p =*= Map.partitionWithKey p

  , prop sa "t_mapMaybe" $
        C.mapMaybe vfm =*= Map.mapMaybe vfm
  , prop sa "t_mapMaybeWithKey" $
        C.mapMaybeWithKey kvfm =*= Map.mapMaybeWithKey kvfm
  , prop sa "t_mapEither" $
        let f v = if even v then Left (2 * v) else Right (3 * v)
        in C.mapEither f =*= Map.mapEither f
  , prop sa "t_mapEitherWithKey" $
        let f k v = if even v then Left (kvf k v) else Right (3 * v)
        in C.mapEitherWithKey f =*= Map.mapEitherWithKey f

  , pmprop sa "t_split" $
        C.split =?*= Map.split
  , pmprop sa "t_splitLookup" $
        C.splitLookup =?*= Map.splitLookup

  -- * Submap
  , prop sa "t_isSubmapOf" $
        C.isSubmapOf =**= Map.isSubmapOf
  , prop sa "t_isSubmapOfBy" $
        C.isSubmapOfBy (<=) =**= Map.isSubmapOfBy (<=)
  , prop sa "t_isProperSubmapOf" $
        C.isProperSubmapOf =**= Map.isProperSubmapOf
  , prop sa "t_isProperSubmapOfBy" $
        C.isProperSubmapOfBy (<=) =**= Map.isProperSubmapOfBy (<=)

  -- * Min\/Max
  , prop sa "t_findMin" $ notEmpty $
        C.findMin =*= Map.findMin
  , prop sa "t_findMax" $ notEmpty $
        C.findMax =*= Map.findMax
  , prop sa "t_deleteMin" $ notEmpty $
        C.deleteMin =*= Map.deleteMin
  , prop sa "t_deleteMax" $ notEmpty $
        C.deleteMax =*= Map.deleteMax
  , prop sa "t_deleteFindMin" $ notEmpty $
        C.deleteFindMin =*= Map.deleteFindMin
  , prop sa "t_deleteFindMax" $ notEmpty $
        C.deleteFindMax =*= Map.deleteFindMax
  , prop sa "t_updateMin" $
        C.updateMinWithKey kvfm =*= Map.updateMinWithKey kvfm
  , prop sa "t_updateMax" $
        C.updateMaxWithKey kvfm =*= Map.updateMaxWithKey kvfm
  , prop sa "t_updateMinWithKey" $
        C.updateMinWithKey kvfm =*= Map.updateMinWithKey kvfm
  , prop sa "t_updateMaxWithKey" $
        C.updateMaxWithKey kvfm =*= Map.updateMaxWithKey kvfm
  , prop sa "t_minView" $
        unfoldr C.minView =*= unfoldr Map.minView
  , prop sa "t_maxView" $
        unfoldr C.maxView =*= unfoldr Map.maxView
  , prop sa "t_minViewWithKey" $
        unfoldr C.minViewWithKey =*= unfoldr Map.minViewWithKey
  , prop sa "t_maxViewWithKey" $
        unfoldr C.maxViewWithKey =*= unfoldr Map.maxViewWithKey

  , prop sa "t_foldMap" $
        foldMap Sum =*= foldMap Sum
  ]
  where
    prop sa' name q = [testProperty name $ q sa']
    pmprop sa' name t = [
       testProperty (name ++ "_general") $ general
     , testProperty (name ++ "_present") $ present
     , testProperty (name ++ "_missing") $ missing
     ]
     where
       general k   kvs = t sa' kvs k
       present k v kvs = t sa' ((k, v):kvs) k
       missing k   kvs = t sa' (filter ((/= k) . fst) kvs) k

    sa = sameAs w

    sameAs :: (CritBitKey k, Ord k)
           => k -> SameAs (CritBit k V) (Map k V) [(k, V)]
    sameAs _ = SameAs C.fromList C.toList Map.fromList Map.toList

properties :: [Test]
properties = [
    testGroup "text" $ propertiesFor T.empty
  , testGroup "bytestring" $ propertiesFor B.empty
  ]

instance (Eq k, Show k, Eq v, Show v) => Eq' (CritBit k v) (Map k v) where
   c =^= m = C.toList c =^= Map.toList m

instance (Eq' a1 b1, Eq k, Show k, Eq v, Show v) => Eq' (a1, CritBit k v) (b1, Map k v) where
  (a1, a2) =^= (b1, b2) = a1 =^= b1 .&&. a2 =^= b2

-- Handy functions for fiddling with from ghci.

blist :: [B.ByteString] -> CritBit B.ByteString Word8
blist = C.fromList . flip zip [0..]

tlist :: [T.Text] -> CritBit T.Text Word8
tlist = C.fromList . flip zip [0..]

mlist :: [B.ByteString] -> Map B.ByteString Word8
mlist = Map.fromList . flip zip [0..]
