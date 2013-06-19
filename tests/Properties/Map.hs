{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}
module Properties.Map
    where

import Control.Arrow (second, (***))
import Data.CritBit.Map.Lazy (CritBitKey, CritBit, byteCount)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (unfoldr, sort, nubBy)
import Data.Map (Map)
import Data.Monoid (Monoid,Sum(..),mappend)
import Data.String (IsString)
import Data.Word (Word8)
import Properties.Common
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Property (Testable)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.CritBit.Set as CSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

--only needed for a test requiring containers >= 0.5
#if MIN_VERSION_containers(0,5,0)
import Data.Functor.Identity (Identity(..))
#endif

type CBProp           = (CritBitKey k, Ord k, Show k, IsString k, Monoid k)
                      => k -> KV k -> Bool
type WithKeyProp      = (CritBitKey k, Ord k, Show k, IsString k, Monoid k)
                      => k -> KV k -> k -> Bool
type WithKeyValueProp = (CritBitKey k, Ord k, Show k, IsString k, Monoid k)
                      => k -> KV k -> k -> V -> Bool
type WithMapProp      = (CritBitKey k, Ord k, Show k, IsString k, Monoid k)
                      => k -> KV k -> KV k -> Bool

newtype CB k = CB (CritBit k V)
    deriving (Show, Eq, Arbitrary)

presentMissingProperty :: (Eq k, Arbitrary k, Show k, IsString k, Testable t)
                       => String -> (k -> KV k -> k -> t) -> k -> [Test]
presentMissingProperty name t w = [
    testProperty (name ++ "_general") $ general
  , testProperty (name ++ "_present") $ present
  , testProperty (name ++ "_missing") $ missing
  ]
  where
    general k   kvs = t w kvs k
    present k v (KV kvs) = t w (KV ((k, v):kvs)) k
    missing k   (KV kvs) = t w (KV (filter ((/= k) . fst) kvs)) k

t_null :: CBProp
t_null = C.null =*= Map.null

t_size :: CBProp
t_size = C.size =*= Map.size

t_lookup :: WithKeyProp
t_lookup = C.lookup =?*= Map.lookup

#if MIN_VERSION_containers(0,5,0)
t_lookupGT :: WithKeyProp
t_lookupGT = C.lookupGT =?*= Map.lookupGT

t_lookupGE :: WithKeyProp
t_lookupGE = C.lookupGE =?*= Map.lookupGE

t_lookupLT :: WithKeyProp
t_lookupLT = C.lookupLT =?*= Map.lookupLT

t_lookupLE :: WithKeyProp
t_lookupLE = C.lookupLE =?*= Map.lookupLE
#endif

t_fromList :: CBProp
t_fromList = C.fromList . fromKV =?= Map.fromList . fromKV

t_fromListWith :: CBProp
t_fromListWith = C.fromListWith (+) . fromKV =?= Map.fromListWith (+) . fromKV

t_fromListWithKey :: CBProp
t_fromListWithKey = C.fromListWithKey f . fromKV =?=
                    Map.fromListWithKey f . fromKV
  where f key a1 a2 = toEnum (byteCount key) * 2 + a1 - a2

t_delete :: WithKeyProp
t_delete = C.delete =?*= Map.delete

t_adjust :: WithKeyProp
t_adjust = C.adjust (+3) =?*= Map.adjust (+3)

t_adjustWithKey :: WithKeyProp
t_adjustWithKey = C.adjustWithKey f =?*= Map.adjustWithKey f
  where f k v = v + fromIntegral (C.byteCount k)

t_updateLookupWithKey :: WithKeyProp
t_updateLookupWithKey = C.updateLookupWithKey f =?*= Map.updateLookupWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_update :: WithKeyProp
t_update = C.update f =?*= Map.update f
  where
    f x
      | even (fromIntegral x :: Int) = Just (x * 10)
      | otherwise                    = Nothing

t_updateWithKey :: WithKeyProp
t_updateWithKey = C.updateWithKey f =?*= Map.updateWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_mapMaybe :: CBProp
t_mapMaybe = C.mapMaybe f =*= Map.mapMaybe f
  where
    f x = if even x then Just (2 * x) else Nothing

t_mapMaybeWithKey :: CBProp
t_mapMaybeWithKey = C.mapMaybeWithKey f =*= Map.mapMaybeWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_mapEither :: CBProp
t_mapEither = (C.toList *** C.toList) . C.mapEither f =*=
              (Map.toList *** Map.toList) . Map.mapEither f
  where
    f x = if even x then Left (2 * x) else Right (3 * x)

t_mapEitherWithKey :: CBProp
t_mapEitherWithKey = (C.toList *** C.toList) . C.mapEitherWithKey f =*=
                     (Map.toList *** Map.toList) . Map.mapEitherWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Left (x + fromIntegral (C.byteCount k))
      | otherwise = Right (2 * x)

t_unionL :: WithMapProp
t_unionL = C.unionL =**= Map.union

t_unionR :: WithMapProp
t_unionR = C.unionR =**= flip Map.union

t_unionWith :: WithMapProp
t_unionWith = C.unionWith (-) =**= Map.unionWith (-)

t_unionWithKey :: WithMapProp
t_unionWithKey = C.unionWithKey f =**= Map.unionWithKey f
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 - v2

t_unions :: (CritBitKey k, Ord k) => k -> Small [KV k] -> Bool
t_unions _ (Small kvs0) =
    Map.toList (Map.unions (map Map.fromList kvs)) ==
    C.toList (C.unions (map C.fromList kvs))
  where
    kvs = map fromKV kvs0

t_unionsWith :: (CritBitKey k, Ord k) => k -> Small [KV k] -> Bool
t_unionsWith _ (Small kvs0) =
    Map.toList (Map.unionsWith (-) (map Map.fromList kvs)) ==
    C.toList (C.unionsWith (-) (map C.fromList kvs))
  where
    kvs = map fromKV kvs0

t_difference :: WithMapProp
t_difference = C.difference =**= Map.difference

t_differenceWith :: WithMapProp
t_differenceWith = C.differenceWith f =**= Map.differenceWith f
  where
    f v1 v2 = if v1 `mod` 4 == 0
              then Nothing
              else Just (v1 - v2)

t_differenceWithKey :: WithMapProp
t_differenceWithKey = C.differenceWithKey f =**= Map.differenceWithKey f
  where
    f key v1 v2 = if C.byteCount key == 2
                  then Nothing
                  else Just (fromIntegral (C.byteCount key) + v1 - v2)

t_intersection :: WithMapProp
t_intersection = C.intersection =**= Map.intersection

t_intersectionWith :: WithMapProp
t_intersectionWith = C.intersectionWith (-) =**= Map.intersectionWith (-)

t_intersectionWithKey :: WithMapProp
t_intersectionWithKey = C.intersectionWithKey f =**= Map.intersectionWithKey f
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 - v2

t_foldl :: CBProp
t_foldl = C.foldl (-) 0 =*= Map.foldl (-) 0

t_foldlWithKey :: CBProp
t_foldlWithKey = C.foldlWithKey f ([], 0) =*= Map.foldlWithKey f ([], 0)
  where
    f (l,s) k v = (k:l,s+v)

t_foldl' :: CBProp
t_foldl' = C.foldl' (-) 0 =*= Map.foldl' (-) 0

t_foldlWithKey' :: CBProp
t_foldlWithKey' = C.foldlWithKey' f ([], 0) =*= Map.foldlWithKey' f ([], 0)
  where
    f (l,s) k v = (k:l,s+v)

t_elems :: CBProp
t_elems = C.elems =*= Map.elems

t_keys :: CBProp
t_keys = C.keys =*= Map.keys

t_keysSet :: CBProp
t_keysSet = CSet.toList . C.keysSet =*= Set.toList . Map.keysSet

#if MIN_VERSION_containers(0,5,0)
t_fromSet :: CBProp
t_fromSet = (C.fromSet f . C.keysSet) =*= (Map.fromSet f . Map.keysSet)
  where f = length . show
#endif

t_map :: CBProp
t_map = C.map (+3) =*= Map.map (+3)

type M m a k v w = ((a -> k -> v -> (a, w)) -> a -> m k v -> (a, m k w))

mapAccumWithKey :: (w ~ String, v ~ V, a ~ Int, Ord k, CritBitKey k) =>
                   M CritBit a k v w -> M Map a k v w -> k -> KV k -> Bool
mapAccumWithKey critbitF mapF _ (KV kvs) = mappedC == mappedM
  where fun i _ v = (i + 1, show $ v + 3)
        mappedC = second C.toList . critbitF fun 0 $ (C.fromList kvs)
        mappedM = second Map.toList . mapF fun 0 $ (Map.fromList kvs)

prepends :: (CritBitKey k, Ord k, IsString k, Monoid k) => k -> k
prepends = mappend "test"

t_mapKeys :: CBProp
t_mapKeys = C.mapKeys prepends =*= Map.mapKeys prepends

t_mapKeysWith :: CBProp
t_mapKeysWith = C.mapKeysWith (+) prepends =*= Map.mapKeysWith (+) prepends

t_mapKeysMonotonic :: CBProp
t_mapKeysMonotonic =
  C.mapKeysMonotonic prepends =*= Map.mapKeysMonotonic prepends

t_mapAccumRWithKey :: CBProp
t_mapAccumRWithKey = mapAccumWithKey C.mapAccumRWithKey Map.mapAccumRWithKey

t_mapAccumWithKey :: CBProp
t_mapAccumWithKey = mapAccumWithKey C.mapAccumWithKey Map.mapAccumWithKey

t_toAscList :: CBProp
t_toAscList = C.toAscList =*= Map.toAscList

t_toDescList :: CBProp
t_toDescList = C.toDescList =*= Map.toDescList

-- Check that 'toList's are equal, with input preprocessing
(=*==) :: (CritBitKey k, Ord k) =>
          ([(k, V)] -> CritBit k V) -> ([(k, V)] -> Map k V)
       -> ([(k, V)] -> [(k, V)]) -> KV k -> Bool
(=*==) f g p (KV kvs) = C.toList (f kvs') == Map.toList (g kvs')
  where
    kvs' = p kvs

t_fromAscList :: CBProp
t_fromAscList _ = (C.fromAscList =*== Map.fromAscList) sort

t_fromAscListWith :: CBProp
t_fromAscListWith _ =
    (C.fromAscListWith (+) =*== Map.fromAscListWith (+)) sort

t_fromAscListWithKey :: CBProp
t_fromAscListWithKey _ =
    (C.fromAscListWithKey f =*== Map.fromAscListWithKey f) sort
  where
    f k v1 v2 = fromIntegral (C.byteCount k) + v1 + 2 * v2

t_fromDistinctAscList :: (CritBitKey k, Ord k) => k -> k -> V -> KV k -> Bool
t_fromDistinctAscList _ k v =
    ((( C.insert k v) .   C.fromDistinctAscList) =*==
    ((Map.insert k v) . Map.fromDistinctAscList))
    (nubBy ((==) `on` fst) . sort)

t_filter :: CBProp
t_filter = C.filter p =*= Map.filter p
  where p = (> (maxBound - minBound) `div` 2)

t_split :: WithKeyProp
t_split = C.split =?*= Map.split

t_splitLookup :: WithKeyProp
t_splitLookup = C.splitLookup =?*= Map.splitLookup

t_isSubmapOf :: WithMapProp
t_isSubmapOf = C.isSubmapOf =**= Map.isSubmapOf

t_isSubmapOfBy :: WithMapProp
t_isSubmapOfBy = C.isSubmapOfBy (<=) =**= Map.isSubmapOfBy (<=)

t_isProperSubmapOf :: WithMapProp
t_isProperSubmapOf = C.isProperSubmapOf =**= Map.isProperSubmapOf

t_isProperSubmapOfBy :: WithMapProp
t_isProperSubmapOfBy = C.isProperSubmapOfBy (<=) =**= Map.isProperSubmapOfBy (<=)

t_findMin :: CBProp
t_findMin k w@(KV kvs) = null kvs || (C.findMin =*= Map.findMin) k w

t_findMax :: CBProp
t_findMax k w@(KV kvs) = null kvs || (C.findMax =*= Map.findMax) k w

t_deleteMin :: CBProp
t_deleteMin = C.deleteMin =*= Map.deleteMin

t_deleteMax :: CBProp
t_deleteMax = C.deleteMax =*= Map.deleteMax

deleteFindAll :: (m -> Bool) -> (m -> (a, m)) -> m -> [a]
deleteFindAll isEmpty deleteFind m0 = unfoldr maybeDeleteFind m0
  where maybeDeleteFind m
          | isEmpty m = Nothing
          | otherwise = Just . deleteFind $ m

t_deleteFindMin :: CBProp
t_deleteFindMin _ (KV kvs) =
    deleteFindAll C.null C.deleteFindMin (C.fromList kvs) ==
    deleteFindAll Map.null Map.deleteFindMin (Map.fromList kvs)

t_deleteFindMax :: CBProp
t_deleteFindMax _ (KV kvs) =
    deleteFindAll C.null C.deleteFindMax (C.fromList kvs) ==
    deleteFindAll Map.null Map.deleteFindMax (Map.fromList kvs)

t_minView :: CBProp
t_minView = unfoldr C.minView =*= unfoldr Map.minView

t_maxView :: CBProp
t_maxView = unfoldr C.maxView =*= unfoldr Map.maxView

t_minViewWithKey :: CBProp
t_minViewWithKey = unfoldr C.minViewWithKey =*= unfoldr Map.minViewWithKey

t_maxViewWithKey :: CBProp
t_maxViewWithKey = unfoldr C.maxViewWithKey =*= unfoldr Map.maxViewWithKey

updateFun :: Integral v => k -> v -> Maybe v
updateFun _ v
  | v `rem` 2 == 0 = Nothing
  | otherwise = Just (v + 1)

t_updateMinWithKey :: CBProp
t_updateMinWithKey =
    C.updateMinWithKey updateFun =*= Map.updateMinWithKey updateFun

t_updateMaxWithKey :: CBProp
t_updateMaxWithKey =
    C.updateMaxWithKey updateFun =*= Map.updateMaxWithKey updateFun

t_insert :: WithKeyValueProp
t_insert = C.insert =??*= Map.insert

t_insertWith :: WithKeyValueProp
t_insertWith = C.insertWith (-) =??*= Map.insertWith (-)

t_insertWithKey :: WithKeyValueProp
t_insertWithKey = C.insertWithKey f =??*= Map.insertWithKey f
  where
    f key v1 v2 = fromIntegral (C.byteCount key) * v1 - v2

t_insertLookupWithKey :: WithKeyValueProp
t_insertLookupWithKey = C.insertLookupWithKey f =??*= Map.insertLookupWithKey f
  where
    f _ v1 v2 = v1 + v2

t_foldMap :: CBProp
t_foldMap = foldMap Sum =*= foldMap Sum

t_mapWithKey :: CBProp
t_mapWithKey = C.mapWithKey f =*= Map.mapWithKey f
  where f _ = show . (+3)

#if MIN_VERSION_containers(0,5,0)
t_traverseWithKey :: CBProp
t_traverseWithKey = runIdentity . C.traverseWithKey f =*=
                    runIdentity . Map.traverseWithKey f
  where f _   = Identity . show . (+3)
#endif

t_alter :: WithKeyProp
t_alter = C.alter f =?*= Map.alter f
  where
    f Nothing = Just 1
    f j       = fmap (+ 1) j

t_alter_delete :: WithKeyProp
t_alter_delete = C.alter (const Nothing) =?*= Map.alter (const Nothing)

t_partitionWithKey :: CBProp
t_partitionWithKey = C.partitionWithKey p =*= Map.partitionWithKey p
  where p k v = odd $ C.byteCount k + fromIntegral v

t_partition :: CBProp
t_partition = C.partition odd =*= Map.partition odd

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, IsString k, Monoid k, Show k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList" $ t_fromList t
  , testProperty "t_fromListWith" $ t_fromListWith t
  , testProperty "t_fromListWithKey" $ t_fromListWithKey t
  , testProperty "t_null" $ t_null t
  , testProperty "t_size" $ t_size t
#if MIN_VERSION_containers(0,5,0)
  ] ++ presentMissingProperty "t_lookupGT" t_lookupGT t ++ [
  ] ++ presentMissingProperty "t_lookupGE" t_lookupGE t ++ [
  ] ++ presentMissingProperty "t_lookupLT" t_lookupLT t ++ [
  ] ++ presentMissingProperty "t_lookupLE" t_lookupLE t ++ [
#endif
  ] ++ presentMissingProperty "t_lookup" t_lookup t ++ [
  ] ++ presentMissingProperty "t_delete" t_delete t ++ [
  ] ++ presentMissingProperty "t_adjust" t_adjust t ++ [
  ] ++ presentMissingProperty "t_adjustWithKey" t_adjustWithKey t ++ [
  ] ++ presentMissingProperty "t_update" t_update t ++ [
  ] ++ presentMissingProperty "t_updateWithKey" t_updateWithKey t ++ [
  ] ++ presentMissingProperty "t_updateLookupWithKey" t_updateLookupWithKey t ++ [
    testProperty "t_mapMaybe" $ t_mapMaybe t
  , testProperty "t_mapMaybeWithKey" $ t_mapMaybeWithKey t
  , testProperty "t_mapEither" $ t_mapEither t
  , testProperty "t_mapEitherWithKey" $ t_mapEitherWithKey t
  , testProperty "t_unionL" $ t_unionL t
  , testProperty "t_unionR" $ t_unionR t
  , testProperty "t_unionWith" $ t_unionWith t
  , testProperty "t_unionWithKey" $ t_unionWithKey t
  , testProperty "t_unions" $ t_unions t
  , testProperty "t_unionsWith" $ t_unionsWith t
  , testProperty "t_difference" $ t_difference t
  , testProperty "t_differenceWith" $ t_differenceWith t
  , testProperty "t_differenceWithKey" $ t_differenceWithKey t
  , testProperty "t_intersection" $ t_intersection t
  , testProperty "t_intersectionWith" $ t_intersectionWith t
  , testProperty "t_intersectionWithKey" $ t_intersectionWithKey t
  , testProperty "t_foldl" $ t_foldl t
  , testProperty "t_foldlWithKey" $ t_foldlWithKey t
  , testProperty "t_foldl'" $ t_foldl' t
  , testProperty "t_foldlWithKey'" $ t_foldlWithKey' t
  , testProperty "t_elems" $ t_elems t
  , testProperty "t_keys" $ t_keys t
  , testProperty "t_keysSet" $ t_keysSet t
#if MIN_VERSION_containers(0,5,0)
  , testProperty "t_fromSet" $ t_fromSet t
#endif
  , testProperty "t_map" $ t_map t
  , testProperty "t_mapWithKey" $ t_mapWithKey t
  , testProperty "t_mapKeys" $ t_mapKeys t
  , testProperty "t_mapKeysWith" $ t_mapKeysWith t
  , testProperty "t_mapKeysMonotonic" $ t_mapKeysMonotonic t
  , testProperty "t_mapAccumWithKey" $ t_mapAccumWithKey t
  , testProperty "t_mapAccumRWithKey" $ t_mapAccumRWithKey t
  , testProperty "t_toAscList" $ t_toAscList t
  , testProperty "t_toDescList" $ t_toDescList t
  , testProperty "t_fromAscList" $ t_fromAscList t
  , testProperty "t_fromAscListWith" $ t_fromAscListWith t
  , testProperty "t_fromAscListWithKey" $ t_fromAscListWithKey t
  , testProperty "t_fromDistinctAscList" $ t_fromDistinctAscList t
  , testProperty "t_insertLookupWithKey" $ t_insertLookupWithKey t
  , testProperty "t_filter" $ t_filter t
  ] ++ presentMissingProperty "t_split" t_split t ++ [
  ] ++ presentMissingProperty "t_splitLookup" t_splitLookup t ++ [
    testProperty "t_isSubmapOf" $ t_isSubmapOf t
  , testProperty "t_isSubmapOfBy" $ t_isSubmapOfBy t
  , testProperty "t_isProperSubmapOf" $ t_isProperSubmapOf t
  , testProperty "t_isProperSubmapOfBy" $ t_isProperSubmapOfBy t
  , testProperty "t_findMin" $ t_findMin t
  , testProperty "t_findMax" $ t_findMax t
  , testProperty "t_deleteMin" $ t_deleteMin t
  , testProperty "t_deleteMax" $ t_deleteMax t
  , testProperty "t_deleteFindMin" $ t_deleteFindMin t
  , testProperty "t_deleteFindMax" $ t_deleteFindMax t
  , testProperty "t_minView" $ t_minView t
  , testProperty "t_maxView" $ t_maxView t
  , testProperty "t_minViewWithKey" $ t_minViewWithKey t
  , testProperty "t_maxViewWithKey" $ t_maxViewWithKey t
  , testProperty "t_updateMinWithKey" $ t_updateMinWithKey t
  , testProperty "t_updateMaxWithKey" $ t_updateMaxWithKey t
  ] ++ presentMissingProperty "t_insert" t_insert t ++ [
  ] ++ presentMissingProperty "t_insertWith" t_insertWith t ++ [
  ] ++ presentMissingProperty "t_insertWithKey" t_insertWithKey t ++ [
    testProperty "t_insertLookupWithKey" $ t_insertLookupWithKey t
#if MIN_VERSION_containers(0,5,0)
  , testProperty "t_traverseWithKey" $ t_traverseWithKey t
#endif
  , testProperty "t_foldMap" $ t_foldMap t
  , testProperty "t_alter" $ t_alter t
  , testProperty "t_alter_delete" $ t_alter_delete t
  , testProperty "t_partition" $ t_partition t
  , testProperty "t_partitionWithKey" $ t_partitionWithKey t
  ]

properties :: [Test]
properties = [
    testGroup "text" $ propertiesFor T.empty
  , testGroup "bytestring" $ propertiesFor B.empty
  ]

infix 4 =^=, =?=, =*=, =?*=, =??*=

-- | Compares heterogeneous values
class Eq' f g where
  (=^=) :: f -> g -> Bool

instance (Eq t) => Eq' t t where
  (=^=) = (==)

instance (Eq k, Eq v) => Eq' (CritBit k v) (Map k v) where
   c =^= m = C.toList c =^= Map.toList m

instance (Eq' a1 b1, Eq k, Eq v) => Eq' (a1, CritBit k v) (b1, Map k v) where
  (a1, a2) =^= (b1, b2) = a1 =^= b1 && a2 =^= b2

instance (Eq' a1 b1, Eq' a2 b2, Eq' a3 b3) => Eq' (a1, a2, a3) (b1, b2, b3)
  where (a1, a2, a3) =^= (b1, b2, b3) = a1 =^= b1 && a2 =^= b2 && a3 =^= b3

-- | Compares functions taking one scalar
(=?=) :: (Ord k, CritBitKey k, Eq' a b)
      => (t -> a) -> (t -> b)
      -> k -> t -> Bool
f =?= g = const $ \t -> f t =^= g t

-- | Compares functions taking one map
(=*=) :: (Ord k, CritBitKey k, Eq' a b)
      => (CritBit k V -> a) -> (Map k V -> b)
      -> k -> KV k -> Bool
f =*= g = const $ \(KV kvs) -> f (C.fromList kvs) =^= g (Map.fromList kvs)

-- | Compares functions taking one scalar and one map
(=?*=) :: (Ord k, CritBitKey k, Eq' a b)
       => (t -> CritBit k V -> a) -> (t -> Map k V -> b)
       -> k -> KV k -> t -> Bool
f =?*= g = \k kvs t -> (f t =*= g t) k kvs

-- | Compares functions taking two scalars and one map
(=??*=) :: (Ord k, CritBitKey k, Eq' a b)
        => (t -> s -> CritBit k V -> a) -> (t -> s -> Map k V -> b)
        -> k -> KV k -> t -> s -> Bool
f =??*= g = \k kvs t s -> (f t s =*= g t s) k kvs

-- | Compares functions taking two maps
(=**=) :: (Ord k, CritBitKey k, Eq' a b)
        => (CritBit k V -> CritBit k V -> a) -> (Map k V -> Map k V -> b)
        -> k -> KV k -> KV k -> Bool
f =**= g = \k kvs (KV kvs2)
           -> (f (C.fromList kvs2) =*= g (Map.fromList kvs2)) k kvs

-- Handy functions for fiddling with from ghci.

blist :: [B.ByteString] -> CritBit B.ByteString Word8
blist = C.fromList . flip zip [0..]

tlist :: [T.Text] -> CritBit T.Text Word8
tlist = C.fromList . flip zip [0..]

mlist :: [B.ByteString] -> Map B.ByteString Word8
mlist = Map.fromList . flip zip [0..]
