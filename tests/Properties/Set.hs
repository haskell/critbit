{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}
module Properties.Set
    where

import Control.Arrow ((***))
import Data.ByteString (ByteString)
import Data.CritBit.Map.Lazy (CritBitKey, byteCount)
import Data.Foldable (foldMap)
import Data.List (unfoldr, sort, nub)
import Data.Monoid (Sum(..), Monoid, (<>))
import Data.String (IsString)
import Data.Text (Text)
import Properties.Common
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Set as CS
import qualified Data.Set as Set
import qualified Data.Text as T

t_null :: (CritBitKey k) => k -> [k] -> Bool
t_null _ ks = CS.null (CS.fromList ks) == null ks

t_member_present :: (CritBitKey k) => k -> CS.Set k -> Bool
t_member_present k = CS.member k . CS.insert k

t_member_missing :: (CritBitKey k) => k -> CS.Set k -> Bool
t_member_missing k = not . CS.member k . CS.delete k

#if MIN_VERSION_containers(0,5,0)
t_lookupGT :: (Ord k, CritBitKey k) => k -> k -> [k] -> Bool
t_lookupGT _ k ks =
    CS.lookupGT k (CS.fromList ks) == Set.lookupGT k (Set.fromList ks)

t_lookupGE :: (Ord k, CritBitKey k) => k -> k -> [k] -> Bool
t_lookupGE _ k ks =
    CS.lookupGE k (CS.fromList ks) == Set.lookupGE k (Set.fromList ks)

t_lookupLT :: (Ord k, CritBitKey k) => k -> k -> [k] -> Bool
t_lookupLT _ k ks =
    CS.lookupLT k (CS.fromList ks) == Set.lookupLT k (Set.fromList ks)

t_lookupLE :: (Ord k, CritBitKey k) => k -> k -> [k] -> Bool
t_lookupLE _ k ks =
    CS.lookupLE k (CS.fromList ks) == Set.lookupLE k (Set.fromList ks)
#endif

-- Test that the behaviour of a CritBit function is the same as that
-- of its counterpart Map function, under some mapping of their
-- results.
isoWith :: (CritBitKey k, Ord k, Eq a) =>
           (c -> a) -> (m -> a)
        -> (CS.Set k -> c) -> (Set.Set k -> m)
        -> k -> [k] -> Bool
isoWith f g critbitf mapf _ ks =
    (f . critbitf . CS.fromList) ks == (g . mapf . Set.fromList) ks

-- Test that the behaviour of a CritBit function is the same as that
-- of its counterpart Map function.
(===) :: (CritBitKey k, Ord k) =>
         (CS.Set k -> CS.Set k) -> (Set.Set k -> Set.Set k)
      -> k -> [k] -> Bool
(===) = isoWith CS.toList Set.toList

t_fromList_toList :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_fromList_toList = id === id

t_fromList_size :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_fromList_size = isoWith CS.size Set.size id id

t_delete_present :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_delete_present k ks =
    CS.toList (CS.delete k c) == Set.toList (Set.delete k m)
  where
    c = CS.insert k $ CS.fromList ks
    m = Set.insert k $ Set.fromList ks

t_unions :: (CritBitKey k, Ord k) => k -> Small [[k]] -> Bool
t_unions _ (Small ks) =
    Set.toList (Set.unions (map Set.fromList ks)) ==
    CS.toList (CS.unions (map CS.fromList ks))

t_difference :: (CritBitKey k, Ord k) => k -> [k] -> [k] -> Bool
t_difference k ks = (CS.difference (CS.fromList ks) ===
    Set.difference (Set.fromList ks)) k

t_intersection :: (CritBitKey k, Ord k) => k -> [k] -> [k] -> Bool
t_intersection k ks = (CS.intersection (CS.fromList ks) ===
    Set.intersection (Set.fromList ks)) k

t_foldl :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_foldl = isoWith id id (CS.foldl f 0) (Set.foldl f 0)
  where f a b = a + byteCount b

t_foldl' :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_foldl' = isoWith id id (CS.foldl' f 0) (Set.foldl' f 0)
  where f a b = a + byteCount b

t_elems :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_elems = isoWith id id CS.elems Set.elems

t_map :: (CritBitKey k, Ord k, Monoid k, IsString k) => k -> [k] -> Bool
t_map = CS.map (<>"a") === Set.map (<>"a")

t_toAscList :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_toAscList = isoWith CS.toAscList Set.toAscList id id

t_toDescList :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_toDescList = isoWith CS.toDescList Set.toDescList id id

-- Check that 'toList's are equal, with input preprocessing
(====) :: Eq a =>
          (c -> CS.Set a) -> (c -> Set.Set a) -> (t -> c) -> t -> Bool
(====) f g p ks = CS.toList (f ks') == Set.toList (g ks')
  where
    ks' = p ks

t_fromAscList :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_fromAscList _ = (CS.fromAscList ==== Set.fromAscList) sort

t_fromDistinctAscList :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_fromDistinctAscList k =
    ((CS.insert k .   CS.fromDistinctAscList) ====
    (Set.insert k . Set.fromDistinctAscList))
    (nub . sort)

t_filter :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_filter = CS.filter p === Set.filter p
  where p = (> (maxBound - minBound) `div` 2) . byteCount

t_split_general :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_split_general k = isoWith (CS.toList *** CS.toList) (Set.toList *** Set.toList)
                            (CS.split k) (Set.split k) k

t_split_present :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_split_present k ks = t_split_general k (k:ks)

t_split_missing :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_split_missing k ks = t_split_general k (filter (/=k) ks)

unpack3 :: (m -> a) -> (m, b, m) -> (a, b, a)
unpack3 f (a, k, b) = (f a, k, f b)

t_submap_general :: (CritBitKey k, Ord k) =>
                    (CS.Set k -> CS.Set k -> Bool)
                 -> (Set.Set k -> Set.Set k -> Bool)
                 -> [k] -> [k] -> Bool
t_submap_general cf mf ks1 ks2 =
  cf (CS.fromList ks1) (CS.fromList ks2) ==
  mf (Set.fromList ks1) (Set.fromList ks2)

t_isSubset_ambiguous :: (CritBitKey k, Ord k) => k -> [k] -> [k] -> Bool
t_isSubset_ambiguous _ ks1 ks2 =
  t_submap_general CS.isSubsetOf Set.isSubsetOf ks1 ks2


t_isProperSubsetOf_ambiguous :: (CritBitKey k, Ord k) =>
                              k -> [k] -> [k] -> Bool
t_isProperSubsetOf_ambiguous _ ks1 ks2 =
  t_submap_general CS.isProperSubsetOf Set.isProperSubsetOf ks1 ks2

t_findMin :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_findMin k w@ks =
  null ks || isoWith id id CS.findMin Set.findMin k w

t_findMax :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_findMax k w@ks =
  null ks || isoWith id id CS.findMax Set.findMax k w

t_deleteMin :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_deleteMin = CS.deleteMin === Set.deleteMin

t_deleteMax :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_deleteMax = CS.deleteMax === Set.deleteMax

deleteFindAll :: (m -> Bool) -> (m -> (a, m)) -> m -> [a]
deleteFindAll isEmpty deleteFind m0 = unfoldr maybeDeleteFind m0
  where maybeDeleteFind m
          | isEmpty m = Nothing
          | otherwise = Just . deleteFind $ m

t_deleteFindMin :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_deleteFindMin _ ks =
    deleteFindAll CS.null CS.deleteFindMin (CS.fromList ks) ==
    deleteFindAll Set.null Set.deleteFindMin (Set.fromList ks)

t_deleteFindMax :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_deleteFindMax _ ks =
    deleteFindAll CS.null CS.deleteFindMax (CS.fromList ks) ==
    deleteFindAll Set.null Set.deleteFindMax (Set.fromList ks)

t_minView :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_minView _ ks =
  unfoldr CS.minView (CS.fromList ks) ==
  unfoldr Set.minView (Set.fromList ks)

t_maxView :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_maxView _ ks =
  unfoldr CS.maxView (CS.fromList ks) ==
  unfoldr Set.maxView (Set.fromList ks)

updateFun :: Integral v => k -> v -> Maybe v
updateFun _ v
  | v `rem` 2 == 0 = Nothing
  | otherwise = Just (v + 1)

t_insert_present :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_insert_present k =
    ((CS.insert k . CS.insert k) === (Set.insert k . Set.insert k)) k

t_insert_missing :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_insert_missing k ks = (CS.insert k === Set.insert k) k (filter (/=k) ks)

t_foldMap :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_foldMap = isoWith (foldMap f) (foldMap f) id id
  where f = Sum . byteCount

t_partition :: (CritBitKey k, Ord k) => k -> [k] -> Bool
t_partition _ ks = partCrit == partMap
  where
    fixup f (a,b) = (f a, f b)
    partCrit = fixup CS.toList . CS.partition foo . CS.fromList $ ks
    partMap  = fixup Set.toList . Set.partition foo . Set.fromList $ ks
    foo = odd . byteCount

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, Monoid k, Show k,
                  IsString k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList_toList" $ t_fromList_toList t
  , testProperty "t_fromList_size" $ t_fromList_size t
  , testProperty "t_null" $ t_null t
  , testProperty "t_member_present" $ t_member_present t
  , testProperty "t_member_missing" $ t_member_missing t
#if MIN_VERSION_containers(0,5,0)
  , testProperty "t_lookupGT" $ t_lookupGT t
  , testProperty "t_lookupGE" $ t_lookupGE t
  , testProperty "t_lookupLT" $ t_lookupLT t
  , testProperty "t_lookupLE" $ t_lookupLE t
#endif
  , testProperty "t_delete_present" $ t_delete_present t
  , testProperty "t_unions" $ t_unions t
  , testProperty "t_difference" $ t_difference t
  , testProperty "t_intersection" $ t_intersection t
  , testProperty "t_foldl" $ t_foldl t
  , testProperty "t_foldl'" $ t_foldl' t
  , testProperty "t_elems" $ t_elems t
  , testProperty "t_map" $ t_map t
  , testProperty "t_mapKeys" $ t_map t
  , testProperty "t_toAscList" $ t_toAscList t
  , testProperty "t_toDescList" $ t_toDescList t
  , testProperty "t_fromAscList" $ t_fromAscList t
  , testProperty "t_fromDistinctAscList" $ t_fromDistinctAscList t
  , testProperty "t_filter" $ t_filter t
  , testProperty "t_split_present" $ t_split_present t
  , testProperty "t_split_missing" $ t_split_missing t
  , testProperty "t_splitLookup_present" $ t_split_present t
  , testProperty "t_splitLookup_missing" $ t_split_missing t
  , testProperty "t_isProperSubsetOf_ambiguous" $
      t_isProperSubsetOf_ambiguous t
  , testProperty "t_findMin" $ t_findMin t
  , testProperty "t_findMax" $ t_findMax t
  , testProperty "t_deleteMin" $ t_deleteMin t
  , testProperty "t_deleteMax" $ t_deleteMax t
  , testProperty "t_deleteFindMin" $ t_deleteFindMin t
  , testProperty "t_deleteFindMax" $ t_deleteFindMax t
  , testProperty "t_minView" $ t_minView t
  , testProperty "t_maxView" $ t_maxView t
  , testProperty "t_insert_present" $ t_insert_present t
  , testProperty "t_insert_missing" $ t_insert_missing t
  , testProperty "t_foldMap" $ t_foldMap t
  , testProperty "t_partition" $ t_partition t
  ]

properties :: [Test]
properties = [
    testGroup "text" $ propertiesFor T.empty
  , testGroup "bytestring" $ propertiesFor B.empty
  ]

-- Handy functions for fiddling with from ghci.

blist :: [ByteString] -> CS.Set ByteString
blist = CS.fromList

tlist :: [Text] -> CS.Set Text
tlist = CS.fromList
