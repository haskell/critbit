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
t_null = C.null =?= Map.null

t_lookup :: WithKeyProp
t_lookup = C.lookup =??= Map.lookup

#if MIN_VERSION_containers(0,5,0)
t_lookupGT :: (Ord k, CritBitKey k) => k -> k -> KV k -> Bool
t_lookupGT _ k (KV kvs) =
    C.lookupGT k (C.fromList kvs) == Map.lookupGT k (Map.fromList kvs)

t_lookupGE :: (Ord k, CritBitKey k) => k -> k -> KV k -> Bool
t_lookupGE _ k (KV kvs) =
    C.lookupGE k (C.fromList kvs) == Map.lookupGE k (Map.fromList kvs)

t_lookupLT :: (Ord k, CritBitKey k) => k -> k -> KV k -> Bool
t_lookupLT _ k (KV kvs) =
    C.lookupLT k (C.fromList kvs) == Map.lookupLT k (Map.fromList kvs)

t_lookupLE :: (Ord k, CritBitKey k) => k -> k -> KV k -> Bool
t_lookupLE _ k (KV kvs) =
    C.lookupLE k (C.fromList kvs) == Map.lookupLE k (Map.fromList kvs)
#endif

-- Test that the behaviour of a CritBit function is the same as that
-- of its counterpart Map function, under some mapping of their
-- results.
isoWith :: (CritBitKey k, Ord k, Eq a) =>
           (c -> a) -> (m -> a)
        -> (CritBit k V -> c) -> (Map k V -> m)
        -> k -> KV k -> Bool
isoWith f g critbitf mapf _ (KV kvs) =
    (f . critbitf . C.fromList) kvs == (g . mapf . Map.fromList) kvs

-- Test that the behaviour of a CritBit function is the same as that
-- of its counterpart Map function.
(===) :: (CritBitKey k, Ord k, Eq v) =>
         (CritBit k V -> CritBit k v) -> (Map k V -> Map k v)
      -> k -> KV k -> Bool
(===) = isoWith C.toList Map.toList

t_fromList_toList :: CBProp
t_fromList_toList = id === id

t_fromList_size :: CBProp
t_fromList_size = isoWith C.size Map.size id id

t_fromListWith_toList :: CBProp
t_fromListWith_toList _ (KV kvs) =
    Map.toList (Map.fromListWith (+) kvsDup) == C.toList (C.fromListWith (+) kvsDup)
    where kvsDup = concatMap (replicate 2) kvs

t_fromListWith_size :: CBProp
t_fromListWith_size _ (KV kvs) =
    Map.size (Map.fromListWith (+) kvsDup) == C.size (C.fromListWith (+) kvsDup)
    where kvsDup = concatMap (replicate 2) kvs

t_fromListWithKey_toList :: CBProp
t_fromListWithKey_toList _ (KV kvs) =
    Map.toList (Map.fromListWithKey f kvsDup) == C.toList (C.fromListWithKey f kvsDup)
    where kvsDup = concatMap (replicate 2) kvs
          f key a1 a2 = toEnum (byteCount key) + a1 + a2

t_fromListWithKey_size :: CBProp
t_fromListWithKey_size _ (KV kvs) =
    Map.size (Map.fromListWithKey f kvsDup) == C.size (C.fromListWithKey f kvsDup)
    where kvsDup = concatMap (replicate 2) kvs
          f key a1 a2 = toEnum (byteCount key) + a1 + a2

t_delete :: WithKeyProp
t_delete = C.delete =??= Map.delete

t_adjust :: WithKeyProp
t_adjust k kvs k0 = (C.adjust (+3) k0 === Map.adjust (+3) k0) k kvs

t_adjustWithKey :: WithKeyProp
t_adjustWithKey k kvs k0 =
    (C.adjustWithKey f k0 === Map.adjustWithKey f k0) k kvs
  where f k v = v + fromIntegral (C.byteCount k)

t_updateLookupWithKey :: WithKeyProp
t_updateLookupWithKey = C.updateLookupWithKey f =??= Map.updateLookupWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_update :: WithKeyProp
t_update = C.update f =??= Map.update f
  where
    f x
      | even (fromIntegral x :: Int) = Just (x * 10)
      | otherwise                    = Nothing

t_updateWithKey :: WithKeyProp
t_updateWithKey = C.updateWithKey f =??= Map.updateWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_mapMaybe :: CBProp
t_mapMaybe = C.mapMaybe f === Map.mapMaybe f
  where
    f x = if even x then Just (2 * x) else Nothing

t_mapMaybeWithKey :: CBProp
t_mapMaybeWithKey = C.mapMaybeWithKey f === Map.mapMaybeWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_mapEither :: CBProp
t_mapEither =
    isoWith (C.toList *** C.toList) (Map.toList *** Map.toList)
            (C.mapEither f) (Map.mapEither f)
  where
    f x = if even x then Left (2 * x) else Right (3 * x)

t_mapEitherWithKey :: CBProp
t_mapEitherWithKey =
    isoWith (C.toList *** C.toList) (Map.toList *** Map.toList)
            (C.mapEitherWithKey f) (Map.mapEitherWithKey f)
  where
    f k x
      | even (fromIntegral x :: Int) =
        Left (x + fromIntegral (C.byteCount k))
      | otherwise = Right (2 * x)

t_unionL :: WithMapProp
t_unionL k (KV kvs) =
    (C.unionL (C.fromList kvs) === Map.union (Map.fromList kvs)) k

t_unionR :: WithMapProp
t_unionR k (KV kvs) =
    (C.unionR (C.fromList kvs) === flip Map.union (Map.fromList kvs)) k

t_unionWith :: WithMapProp
t_unionWith k (KV kvs) = (C.unionWith (-) (C.fromList kvs) ===
                          Map.unionWith (-) (Map.fromList kvs)) k

t_unionWithKey :: WithMapProp
t_unionWithKey k (KV kvs) = (C.unionWithKey f (C.fromList kvs) ===
                             Map.unionWithKey f (Map.fromList kvs)) k
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
t_difference k (KV kvs) = (C.difference (C.fromList kvs) ===
    Map.difference (Map.fromList kvs)) k

t_differenceWith :: WithMapProp
t_differenceWith k (KV kvs) =
    (C.differenceWith f (C.fromList kvs) ===
        Map.differenceWith f (Map.fromList kvs)) k
  where
    f v1 v2 = if v1 `mod` 4 == 0
              then Nothing
              else Just (v1 - v2)

t_differenceWithKey :: WithMapProp
t_differenceWithKey k (KV kvs) =
    (C.differenceWithKey f (C.fromList kvs) ===
        Map.differenceWithKey f (Map.fromList kvs)) k
  where
    f key v1 v2 = if C.byteCount key == 2
                  then Nothing
                  else Just (fromIntegral (C.byteCount key) + v1 - v2)

t_intersection :: WithMapProp
t_intersection k (KV kvs) = (C.intersection (C.fromList kvs) ===
    Map.intersection (Map.fromList kvs)) k

t_intersectionWith :: WithMapProp
t_intersectionWith k (KV kvs) =
    (C.intersectionWith (-) (C.fromList kvs) ===
        Map.intersectionWith (-) (Map.fromList kvs)) k

t_intersectionWithKey :: WithMapProp
t_intersectionWithKey k (KV kvs) =
    (C.intersectionWithKey f (C.fromList kvs) ===
        Map.intersectionWithKey f (Map.fromList kvs)) k
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 - v2

t_foldl :: CBProp
t_foldl = isoWith id id (C.foldl (-) 0) (Map.foldl (-) 0)

t_foldlWithKey :: CBProp
t_foldlWithKey = isoWith id id (C.foldlWithKey f ([], 0))
                               (Map.foldlWithKey f ([], 0))
  where
    f (l,s) k v = (k:l,s+v)

t_foldl' :: CBProp
t_foldl' = isoWith id id (C.foldl' (-) 0) (Map.foldl' (-) 0)

t_foldlWithKey' :: CBProp
t_foldlWithKey' = isoWith id id (C.foldlWithKey' f ([], 0))
                                (Map.foldlWithKey' f ([], 0))
  where
    f (l,s) k v = (k:l,s+v)

t_elems :: CBProp
t_elems = isoWith id id C.elems Map.elems

t_keys :: CBProp
t_keys = isoWith id id C.keys Map.keys

t_keysSet :: CBProp
t_keysSet = isoWith CSet.toList Set.toList C.keysSet Map.keysSet

#if MIN_VERSION_containers(0,5,0)
t_fromSet :: CBProp
t_fromSet = (C.fromSet f . C.keysSet) === (Map.fromSet f . Map.keysSet)
  where f = length . show
#endif

t_map :: CBProp
t_map = C.map (+3) === Map.map (+3)

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
t_mapKeys = C.mapKeys prepends === Map.mapKeys prepends

t_mapKeysWith :: (CritBitKey k, Ord k, IsString k, Monoid k)
              => k -> KV k -> Bool
t_mapKeysWith =
  C.mapKeysWith (+) prepends === Map.mapKeysWith (+) prepends

t_mapKeysMonotonic :: (CritBitKey k, Ord k, IsString k, Monoid k)
                   => k -> KV k -> Bool
t_mapKeysMonotonic =
  C.mapKeysMonotonic prepends === Map.mapKeysMonotonic prepends

t_mapAccumRWithKey :: CBProp
t_mapAccumRWithKey = mapAccumWithKey C.mapAccumRWithKey Map.mapAccumRWithKey

t_mapAccumWithKey :: CBProp
t_mapAccumWithKey = mapAccumWithKey C.mapAccumWithKey Map.mapAccumWithKey

t_toAscList :: CBProp
t_toAscList = isoWith C.toAscList Map.toAscList id id

t_toDescList :: CBProp
t_toDescList = isoWith C.toDescList Map.toDescList id id

-- Check that 'toList's are equal, with input preprocessing
(====) :: (CritBitKey k, Ord k) =>
          ([(k, V)] -> CritBit k V) -> ([(k, V)] -> Map k V)
       -> ([(k, V)] -> [(k, V)]) -> KV k -> Bool
(====) f g p (KV kvs) = C.toList (f kvs') == Map.toList (g kvs')
  where
    kvs' = p kvs

t_fromAscList :: CBProp
t_fromAscList _ = (C.fromAscList ==== Map.fromAscList) sort

t_fromAscListWith :: CBProp
t_fromAscListWith _ =
    (C.fromAscListWith (+) ==== Map.fromAscListWith (+)) sort

t_fromAscListWithKey :: CBProp
t_fromAscListWithKey _ =
    (C.fromAscListWithKey f ==== Map.fromAscListWithKey f) sort
  where
    f k v1 v2 = fromIntegral (C.byteCount k) + v1 + 2 * v2

t_fromDistinctAscList :: (CritBitKey k, Ord k) => k -> k -> V -> KV k -> Bool
t_fromDistinctAscList _ k v =
    ((( C.insert k v) .   C.fromDistinctAscList) ====
    ((Map.insert k v) . Map.fromDistinctAscList))
    (nubBy ((==) `on` fst) . sort)

t_filter :: CBProp
t_filter = C.filter p === Map.filter p
  where p = (> (maxBound - minBound) `div` 2)

t_split :: WithKeyProp
t_split k' kvs k = isoWith (C.toList *** C.toList) (Map.toList *** Map.toList)
                    (C.split k) (Map.split k) k' kvs

t_splitLookup :: WithKeyProp
t_splitLookup = C.splitLookup =??= Map.splitLookup

t_submap_general :: (CritBitKey k, Ord k) =>
                    (CritBit k V -> CritBit k V -> Bool)
                    -> (Map k V -> Map k V -> Bool)
                    -> KV k -> KV k -> Bool
t_submap_general cf mf (KV kvs1) (KV kvs2) =
  cf (C.fromList kvs1) (C.fromList kvs2) ==
  mf (Map.fromList kvs1) (Map.fromList kvs2)

t_isSubmap_ambiguous :: WithMapProp
t_isSubmap_ambiguous _ kvs1 kvs2 =
  t_submap_general C.isSubmapOf Map.isSubmapOf kvs1 kvs2

t_isSubmapOfBy_true :: WithMapProp
t_isSubmapOfBy_true _ (KV kvs1) (KV kvs2) =
  C.isSubmapOfBy (<=) (C.fromList kvs1)
                      (C.fromList $ kvs2 ++ (fmap (second (+1)) kvs1))

t_isSubmapOfBy_ambiguous :: WithMapProp
t_isSubmapOfBy_ambiguous _ kvs1 kvs2 =
  t_submap_general (C.isSubmapOfBy (<=)) (Map.isSubmapOfBy (<=)) kvs1 kvs2

t_isProperSubmapOf_ambiguous :: WithMapProp
t_isProperSubmapOf_ambiguous _ kvs1 kvs2 =
  t_submap_general C.isProperSubmapOf Map.isProperSubmapOf kvs1 kvs2

t_isProperSubmapOfBy_ambiguous :: WithMapProp
t_isProperSubmapOfBy_ambiguous _ kvs1 kvs2 =
  t_submap_general (C.isProperSubmapOfBy (<=))
                   (Map.isProperSubmapOfBy (<=))
                   kvs1 kvs2

t_findMin :: CBProp
t_findMin k w@(KV kvs) =
  null kvs || isoWith id id C.findMin Map.findMin k w

t_findMax :: CBProp
t_findMax k w@(KV kvs) =
  null kvs || isoWith id id C.findMax Map.findMax k w

t_deleteMin :: CBProp
t_deleteMin = C.deleteMin === Map.deleteMin

t_deleteMax :: CBProp
t_deleteMax = C.deleteMax === Map.deleteMax

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
t_minView _ (KV kvs) =
  unfoldr C.minView (C.fromList kvs) ==
  unfoldr Map.minView (Map.fromList kvs)

t_maxView :: CBProp
t_maxView _ (KV kvs) =
  unfoldr C.maxView (C.fromList kvs) ==
  unfoldr Map.maxView (Map.fromList kvs)

t_minViewWithKey :: CBProp
t_minViewWithKey _ (KV kvs) =
  unfoldr C.minViewWithKey (C.fromList kvs) ==
  unfoldr Map.minViewWithKey (Map.fromList kvs)

t_maxViewWithKey :: CBProp
t_maxViewWithKey _ (KV kvs) =
  unfoldr C.maxViewWithKey (C.fromList kvs) ==
  unfoldr Map.maxViewWithKey (Map.fromList kvs)

updateFun :: Integral v => k -> v -> Maybe v
updateFun _ v
  | v `rem` 2 == 0 = Nothing
  | otherwise = Just (v + 1)

t_updateMinWithKey :: CBProp
t_updateMinWithKey =
    C.updateMinWithKey updateFun === Map.updateMinWithKey updateFun

t_updateMaxWithKey :: CBProp
t_updateMaxWithKey =
    C.updateMaxWithKey updateFun === Map.updateMaxWithKey updateFun

t_insert :: WithKeyValueProp
t_insert = C.insert =???= Map.insert

t_insertWith :: WithKeyValueProp
t_insertWith = C.insertWith (-) =???= Map.insertWith (-)

t_insertWithKey :: WithKeyValueProp
t_insertWithKey = C.insertWithKey f =???= Map.insertWithKey f
  where
    f key v1 v2 = fromIntegral (C.byteCount key) * v1 - v2

t_insertLookupWithKey :: (CritBitKey k, Ord k)
                      => k -> k -> V -> KV k -> Bool
t_insertLookupWithKey _ k v (KV kvs) = m == c
  where
    fixup tl (a,b) = (a,tl b)
    f _ v1 v2 = v1 + v2
    m = fixup Map.toList . Map.insertLookupWithKey f k v $ Map.fromList kvs
    c = fixup C.toList . C.insertLookupWithKey f k v $ C.fromList kvs

t_foldMap :: CBProp
t_foldMap = isoWith (foldMap Sum) (foldMap Sum) id id

t_mapWithKey :: CBProp
t_mapWithKey = C.mapWithKey f === Map.mapWithKey f
  where f _ = show . (+3)

#if MIN_VERSION_containers(0,5,0)
t_traverseWithKey :: CBProp
t_traverseWithKey _ (KV kvs) = mappedC == mappedM
  where fun _   = Identity . show . (+3)
        mappedC = C.toList . runIdentity . C.traverseWithKey fun $
                  (C.fromList kvs)
        mappedM = Map.toList . runIdentity . Map.traverseWithKey fun $
                  (Map.fromList kvs)
#endif

alter :: (CritBitKey k, Ord k) =>
         (Maybe Word8 -> Maybe Word8) -> k -> KV k -> Bool
alter f k = (C.alter f k === Map.alter f k) k

t_alter :: CBProp
t_alter = alter f
  where
    f Nothing = Just 1
    f j       = fmap (+ 1) j

t_alter_delete :: CBProp
t_alter_delete = alter (const Nothing)

t_partitionWithKey :: CBProp
t_partitionWithKey _ (KV kvs) = partCrit == partMap
  where
    fixup f (a,b) = (f a, f b)
    predicate k _ = odd $ C.byteCount k
    partCrit = fixup C.toList . C.partitionWithKey predicate . C.fromList $ kvs
    partMap  = fixup Map.toList . Map.partitionWithKey predicate . Map.fromList $ kvs

t_partition :: CBProp
t_partition _ (KV kvs) = partCrit == partMap
  where
    fixup f (a,b) = (f a, f b)
    partCrit = fixup C.toList . C.partition odd . C.fromList $ kvs
    partMap  = fixup Map.toList . Map.partition odd . Map.fromList $ kvs

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, IsString k, Monoid k, Show k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList_toList" $ t_fromList_toList t
  , testProperty "t_fromList_size" $ t_fromList_size t
  , testProperty "t_fromListWith_toList" $ t_fromListWith_toList t
  , testProperty "t_fromListWith_size" $ t_fromListWith_size t
  , testProperty "t_fromListWithKey_toList" $ t_fromListWithKey_toList t
  , testProperty "t_fromListWithKey_size" $ t_fromListWithKey_size t
  , testProperty "t_null" $ t_null t
#if MIN_VERSION_containers(0,5,0)
  , testProperty "t_lookupGT" $ t_lookupGT t
  , testProperty "t_lookupGE" $ t_lookupGE t
  , testProperty "t_lookupLT" $ t_lookupLT t
  , testProperty "t_lookupLE" $ t_lookupLE t
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
    testProperty "t_isSubmap_ambiguous" $ t_isSubmap_ambiguous t
  , testProperty "t_isSubmapOfBy_true" $ t_isSubmapOfBy_true t
  , testProperty "t_isSubmapOfBy_ambiguous" $ t_isSubmapOfBy_ambiguous t
  , testProperty "t_isProperSubmapOf_ambiguous" $
      t_isProperSubmapOf_ambiguous t
  , testProperty "t_isProperSubmapOfBy_ambiguous" $
      t_isProperSubmapOfBy_ambiguous t
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

infix 4 =*=, =?=, =??=, =???=
class Eq' f g where
  (=*=) :: f -> g -> Bool

instance (Eq t) => Eq' t t where
  (=*=) = (==)

instance (Eq k, Eq v) => Eq' (CritBit k v) (Map k v) where
   c =*= m = C.toList c =*= Map.toList m

instance (Eq' a1 b1, Eq' a2 b2) => Eq' (a1, a2) (b1, b2) where
  (a1, a2) =*= (b1, b2) = a1 =*= b1 && a2 =*= b2

instance (Eq' a1 b1, Eq' a2 b2, Eq' a3 b3) => Eq' (a1, a2, a3) (b1, b2, b3)
  where (a1, a2, a3) =*= (b1, b2, b3) = a1 =*= b1 && a2 =*= b2 && a3 =*= b3

-- | Compares (map -> result) functions
(=?=) :: (Ord k, CritBitKey k, Eq' a b)
      => (CritBit k V -> a) -> (Map k V -> b)
      -> k -> KV k -> Bool
f =?= g = const $ \(KV kvs) -> f (C.fromList kvs) =*= g (Map.fromList kvs)

-- | Compares (key -> map -> result) functions
(=??=) :: (Ord k, CritBitKey k, Eq' a b)
       => (t -> CritBit k V -> a) -> (t -> Map k V -> b)
       -> k -> KV k -> t -> Bool
f =??= g = \k kvs t -> (f t =?= g t) k kvs

-- | Compares (key -> value -> map -> result) functions
(=???=) :: (Ord k, CritBitKey k, Eq' a b)
        => (t -> s -> CritBit k V -> a) -> (t -> s -> Map k V -> b)
        -> k -> KV k -> t -> s -> Bool
f =???= g = \k kvs t s -> (f t s =?= g t s) k kvs

-- Handy functions for fiddling with from ghci.

blist :: [B.ByteString] -> CritBit B.ByteString Word8
blist = C.fromList . flip zip [0..]

tlist :: [T.Text] -> CritBit T.Text Word8
tlist = C.fromList . flip zip [0..]

mlist :: [B.ByteString] -> Map B.ByteString Word8
mlist = Map.fromList . flip zip [0..]
