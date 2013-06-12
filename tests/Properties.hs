{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties
    where

import Control.Applicative ((<$>))
import Control.Arrow (second, (***))
import Data.ByteString (ByteString)
import Data.CritBit.Map.Lazy (CritBitKey, CritBit, byteCount)
import Data.Foldable (foldMap)

--only needed for a test requiring containers >= 0.5
#if MIN_VERSION_containers(0,5,0)
import Data.Functor.Identity (Identity(..))
#endif

import Data.List (unfoldr, sort, nubBy)
import Data.Function (on)
import Data.Map (Map)
import Data.Monoid (Sum(..))
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Word (Word8)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Gen (Gen, resize, sized)
import Test.QuickCheck.Property (Property, Testable, forAll)
import qualified Data.ByteString as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.Map as Map
import qualified Data.Text as T

instance Arbitrary ByteString where
    arbitrary = BB.pack <$> arbitrary
    shrink    = map B.pack . shrink . B.unpack

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary
    shrink    = map T.pack . shrink . T.unpack

type V = Word8

newtype KV a = KV { fromKV :: [(a, V)] }
        deriving (Show, Eq, Ord)

instance Arbitrary a => Arbitrary (KV a) where
    arbitrary = (KV . flip zip [0..]) <$> arbitrary
    shrink = map (KV . flip zip [0..]) . shrink . map fst . fromKV

instance (CritBitKey k, Arbitrary k, Arbitrary v) =>
  Arbitrary (CritBit k v) where
    arbitrary = C.fromList <$> arbitrary
    shrink = map C.fromList . shrink . C.toList

newtype CB k = CB (CritBit k V)
    deriving (Show, Eq, Arbitrary)

-- For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll smallArbitrary

smallArbitrary :: (Arbitrary a, Show a) => Gen a
smallArbitrary = sized $ \n -> resize (smallish n) arbitrary
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs

newtype Small a = Small { fromSmall :: a }
    deriving (Eq, Ord, Show)

instance (Show a, Arbitrary a) => Arbitrary (Small a) where
    arbitrary = Small <$> smallArbitrary
    shrink = map Small . shrink . fromSmall

t_null :: (CritBitKey k) => k -> KV k -> Bool
t_null _ (KV kvs) = C.null (C.fromList kvs) == null kvs

t_lookup_present :: (CritBitKey k) => k -> k -> V -> CB k -> Bool
t_lookup_present _ k v (CB m) = C.lookup k (C.insert k v m) == Just v

t_lookup_missing :: (CritBitKey k) => k -> k -> CB k -> Bool
t_lookup_missing _ k (CB m) = C.lookup k (C.delete k m) == Nothing

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

t_fromList_toList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_toList = id === id

t_fromList_size :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_size = isoWith C.size Map.size id id

t_fromListWith_toList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromListWith_toList _ (KV kvs) =
    Map.toList (Map.fromListWith (+) kvsDup) == C.toList (C.fromListWith (+) kvsDup)
    where kvsDup = concatMap (replicate 2) kvs

t_fromListWith_size :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromListWith_size _ (KV kvs) =
    Map.size (Map.fromListWith (+) kvsDup) == C.size (C.fromListWith (+) kvsDup)
    where kvsDup = concatMap (replicate 2) kvs

t_fromListWithKey_toList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromListWithKey_toList _ (KV kvs) =
    Map.toList (Map.fromListWithKey f kvsDup) == C.toList (C.fromListWithKey f kvsDup)
    where kvsDup = concatMap (replicate 2) kvs
          f key a1 a2 = toEnum (byteCount key) + a1 + a2

t_fromListWithKey_size :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromListWithKey_size _ (KV kvs) =
    Map.size (Map.fromListWithKey f kvsDup) == C.size (C.fromListWithKey f kvsDup)
    where kvsDup = concatMap (replicate 2) kvs
          f key a1 a2 = toEnum (byteCount key) + a1 + a2

t_delete_present :: (CritBitKey k, Ord k) => k -> KV k -> k -> V -> Bool
t_delete_present _ (KV kvs) k v =
    C.toList (C.delete k c) == Map.toList (Map.delete k m)
  where
    c = C.insert k v $ C.fromList kvs
    m = Map.insert k v $ Map.fromList kvs

t_adjust_general :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_adjust_general k = (C.adjust (+3) k === Map.adjust (+3) k) k

t_adjust_present :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_adjust_present k v (KV kvs) = t_adjust_general k (KV ((k,v):kvs))

t_adjust_missing :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_adjust_missing k (KV kvs) =
    t_adjust_general k (KV $ filter ((/=k) . fst) kvs)

t_adjustWithKey_general :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_adjustWithKey_general k0 =
    (C.adjustWithKey f k0 === Map.adjustWithKey f k0) k0
  where f k v = v + fromIntegral (C.byteCount k)

t_adjustWithKey_present :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_adjustWithKey_present k v (KV kvs) =
  t_adjustWithKey_general k (KV ((k,v):kvs))

t_adjustWithKey_missing :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_adjustWithKey_missing k (KV kvs) =
  t_adjustWithKey_general k (KV $ filter ((/=k) . fst) kvs)

naiveUpdateLookupWithKey :: (CritBitKey k) => (k -> v -> Maybe v) -> k
                         -> CritBit k v -> (Maybe v, CritBit k v)
naiveUpdateLookupWithKey g k m =
  case C.lookup k m of
    Just v  -> case g k v of
      Just v' -> (Just v', C.insert k v' m)
      Nothing -> (Just v, C.delete k m)
    Nothing -> (Nothing, m)

t_updateLookupWithKey_general :: (CritBitKey k)
                              => (k -> V -> CritBit k V -> CritBit k V)
                              -> k -> V -> CB k -> Bool
t_updateLookupWithKey_general h k0 v0 (CB m0) =
    C.updateLookupWithKey f k0 m1 == naiveUpdateLookupWithKey f k0 m1
  where
    m1 = h k0 v0 m0
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_updateLookupWithKey_present :: (CritBitKey k) => k -> V -> CB k -> Bool
t_updateLookupWithKey_present =
  t_updateLookupWithKey_general C.insert

t_updateLookupWithKey_missing :: (CritBitKey k) => k -> V -> CB k -> Bool
t_updateLookupWithKey_missing =
  t_updateLookupWithKey_general (\k _v m -> C.delete k m)

t_update_general :: (CritBitKey k)
                 => (k -> V -> CritBit k V -> CritBit k V)
                 -> k -> V -> CB k -> Bool
t_update_general h k0 v0 (CB m0) = C.update f k0 m1 == naiveUpdate f k0 m1
  where
    m1 = h k0 v0 m0
    naiveUpdate g k = snd . naiveUpdateLookupWithKey (\_ v -> g v) k
    f x
      | even (fromIntegral x :: Int) = Just (x * 10)
      | otherwise                    = Nothing

t_update_present :: (CritBitKey k) => k -> V -> CB k -> Bool
t_update_present = t_update_general C.insert

t_update_missing :: (CritBitKey k) => k -> V -> CB k -> Bool
t_update_missing = t_update_general (\k _v m -> C.delete k m)

t_updateWithKey_general :: (CritBitKey k)
                        => (k -> V -> CritBit k V -> CritBit k V)
                        -> k -> V -> CB k -> Bool
t_updateWithKey_general h k0 v0 (CB m0) =
    C.updateWithKey f k0 m1 == naiveUpdateWithKey f k0 m1
  where
    m1 = h k0 v0 m0
    naiveUpdateWithKey g k = snd . naiveUpdateLookupWithKey g k
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_updateWithKey_present :: (CritBitKey k) => k -> V -> CB k -> Bool
t_updateWithKey_present = t_updateWithKey_general C.insert

t_updateWithKey_missing :: (CritBitKey k) => k -> V -> CB k -> Bool
t_updateWithKey_missing = t_updateWithKey_general (\k _v m -> C.delete k m)

t_mapMaybeWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_mapMaybeWithKey = C.mapMaybeWithKey f === Map.mapMaybeWithKey f
  where
    f k x
      | even (fromIntegral x :: Int) =
        Just (x + fromIntegral (C.byteCount k))
      | otherwise = Nothing

t_mapEitherWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_mapEitherWithKey =
    isoWith (C.toList *** C.toList) (Map.toList *** Map.toList)
            (C.mapEitherWithKey f) (Map.mapEitherWithKey f)
  where
    f k x
      | even (fromIntegral x :: Int) =
        Left (x + fromIntegral (C.byteCount k))
      | otherwise = Right (2 * x)

t_unionL :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_unionL k (KV kvs) =
    (C.unionL (C.fromList kvs) === Map.union (Map.fromList kvs)) k

t_unionR :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_unionR k (KV kvs) =
    (C.unionR (C.fromList kvs) === flip Map.union (Map.fromList kvs)) k

t_unionWith :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_unionWith k (KV kvs) = (C.unionWith (-) (C.fromList kvs) ===
                          Map.unionWith (-) (Map.fromList kvs)) k

t_unionWithKey :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
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

t_difference :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_difference k (KV kvs) = (C.difference (C.fromList kvs) ===
    Map.difference (Map.fromList kvs)) k

t_differenceWith :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_differenceWith k (KV kvs) =
    (C.differenceWith f (C.fromList kvs) ===
        Map.differenceWith f (Map.fromList kvs)) k
  where
    f v1 v2 = if v1 `mod` 4 == 0
              then Nothing
              else Just (v1 - v2)

t_differenceWithKey :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_differenceWithKey k (KV kvs) =
    (C.differenceWithKey f (C.fromList kvs) ===
        Map.differenceWithKey f (Map.fromList kvs)) k
  where
    f key v1 v2 = if C.byteCount key == 2
                  then Nothing
                  else Just (fromIntegral (C.byteCount key) + v1 - v2)

t_intersection :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_intersection k (KV kvs) = (C.intersection (C.fromList kvs) ===
    Map.intersection (Map.fromList kvs)) k

t_intersectionWith :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_intersectionWith k (KV kvs) =
    (C.intersectionWith (-) (C.fromList kvs) ===
        Map.intersectionWith (-) (Map.fromList kvs)) k

t_intersectionWithKey :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_intersectionWithKey k (KV kvs) =
    (C.intersectionWithKey f (C.fromList kvs) ===
        Map.intersectionWithKey f (Map.fromList kvs)) k
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 - v2

t_foldl :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldl = isoWith id id (C.foldl (-) 0) (Map.foldl (-) 0)

t_foldlWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldlWithKey = isoWith id id (C.foldlWithKey f ([], 0))
                               (Map.foldlWithKey f ([], 0))
  where
    f (l,s) k v = (k:l,s+v)

t_foldl' :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldl' = isoWith id id (C.foldl' (-) 0) (Map.foldl' (-) 0)

t_foldlWithKey' :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldlWithKey' = isoWith id id (C.foldlWithKey' f ([], 0))
                                (Map.foldlWithKey' f ([], 0))
  where
    f (l,s) k v = (k:l,s+v)

t_elems :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_elems = isoWith id id C.elems Map.elems

t_keys :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_keys = isoWith id id C.keys Map.keys

t_map :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_map = C.map (+3) === Map.map (+3)

type M m a k v w = ((a -> k -> v -> (a, w)) -> a -> m k v -> (a, m k w))

mapAccumWithKey :: (w ~ String, v ~ V, a ~ Int, Ord k, CritBitKey k) =>
                   M CritBit a k v w -> M Map a k v w -> k -> KV k -> Bool
mapAccumWithKey critbitF mapF _ (KV kvs) = mappedC == mappedM
  where fun i _ v = (i + 1, show $ v + 3)
        mappedC = second C.toList . critbitF fun 0 $ (C.fromList kvs)
        mappedM = second Map.toList . mapF fun 0 $ (Map.fromList kvs)

t_mapKeys :: (CritBitKey k, Ord k, IsString k, Show k) => k -> KV k -> Bool
t_mapKeys = C.mapKeys f === Map.mapKeys f
  where
    f :: (CritBitKey k, Ord k, IsString k, Show k) => k -> k
    f = fromString . (++ "test") . show

t_mapAccumRWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_mapAccumRWithKey = mapAccumWithKey C.mapAccumRWithKey Map.mapAccumRWithKey

t_mapAccumWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_mapAccumWithKey = mapAccumWithKey C.mapAccumWithKey Map.mapAccumWithKey

t_toAscList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_toAscList = isoWith C.toAscList Map.toAscList id id

t_toDescList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_toDescList = isoWith C.toDescList Map.toDescList id id

-- Check that 'toList's are equal, with input preprocessing
(====) :: (CritBitKey k, Ord k) =>
          ([(k, V)] -> CritBit k V) -> ([(k, V)] -> Map k V)
       -> ([(k, V)] -> [(k, V)]) -> KV k -> Bool
(====) f g p (KV kvs) = C.toList (f kvs') == Map.toList (g kvs')
  where
    kvs' = p kvs

t_fromAscList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromAscList _ = (C.fromAscList ==== Map.fromAscList) sort

t_fromAscListWith :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromAscListWith _ =
    (C.fromAscListWith (+) ==== Map.fromAscListWith (+)) sort

t_fromAscListWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromAscListWithKey _ =
    (C.fromAscListWithKey f ==== Map.fromAscListWithKey f) sort
  where
    f k v1 v2 = fromIntegral (C.byteCount k) + v1 + 2 * v2

t_fromDistinctAscList :: (CritBitKey k, Ord k) => k -> k -> V -> KV k -> Bool
t_fromDistinctAscList _ k v =
    ((( C.insert k v) .   C.fromDistinctAscList) ====
    ((Map.insert k v) . Map.fromDistinctAscList))
    (nubBy ((==) `on` fst) . sort)

t_filter :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_filter = C.filter p === Map.filter p
  where p = (> (maxBound - minBound) `div` 2)

t_split_general :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_split_general k = isoWith (C.toList *** C.toList) (Map.toList *** Map.toList)
                            (C.split k) (Map.split k) k

t_split_present :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_split_present k v (KV kvs) = t_split_general k (KV ((k,v):kvs))

t_split_missing :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_split_missing k (KV kvs) = t_split_general k (KV (filter ((/=k) . fst) kvs))

unpack3 :: (m -> a) -> (m, b, m) -> (a, b, a)
unpack3 f (a, k, b) = (f a, k, f b)

t_splitLookup_present :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_splitLookup_present k v (KV kvs) =
    isoWith (unpack3 C.toList) (unpack3 Map.toList)
            (C.splitLookup k) (Map.splitLookup k) k (KV ((k,v):kvs))

t_splitLookup_missing :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_splitLookup_missing k (KV kvs) =
    isoWith (unpack3 C.toList) (unpack3 Map.toList)
            (C.splitLookup k) (Map.splitLookup k) k
            (KV (filter ((/=k) . fst) kvs))

t_submap_general :: (CritBitKey k, Ord k) =>
                    (CritBit k V -> CritBit k V -> Bool)
                    -> (Map k V -> Map k V -> Bool)
                    -> KV k -> KV k -> Bool
t_submap_general cf mf (KV kvs1) (KV kvs2) =
  cf (C.fromList kvs1) (C.fromList kvs2) ==
  mf (Map.fromList kvs1) (Map.fromList kvs2)

t_isSubmap_ambiguous :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_isSubmap_ambiguous _ kvs1 kvs2 =
  t_submap_general C.isSubmapOf Map.isSubmapOf kvs1 kvs2

t_isSubmapOfBy_true :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_isSubmapOfBy_true _ (KV kvs1) (KV kvs2) =
  C.isSubmapOfBy (<=) (C.fromList kvs1)
                      (C.fromList $ kvs2 ++ (fmap (second (+1)) kvs1))

t_isSubmapOfBy_ambiguous :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_isSubmapOfBy_ambiguous _ kvs1 kvs2 =
  t_submap_general (C.isSubmapOfBy (<=)) (Map.isSubmapOfBy (<=)) kvs1 kvs2

t_isProperSubmapOf_ambiguous :: (CritBitKey k, Ord k) =>
                              k -> KV k -> KV k -> Bool
t_isProperSubmapOf_ambiguous _ kvs1 kvs2 =
  t_submap_general C.isProperSubmapOf Map.isProperSubmapOf kvs1 kvs2

t_isProperSubmapOfBy_ambiguous :: (CritBitKey k, Ord k) =>
                                  k -> KV k -> KV k -> Bool
t_isProperSubmapOfBy_ambiguous _ kvs1 kvs2 =
  t_submap_general (C.isProperSubmapOfBy (<=))
                   (Map.isProperSubmapOfBy (<=))
                   kvs1 kvs2

t_findMin :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_findMin k w@(KV kvs) =
  null kvs || isoWith id id C.findMin Map.findMin k w

t_findMax :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_findMax k w@(KV kvs) =
  null kvs || isoWith id id C.findMax Map.findMax k w

t_deleteMin :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_deleteMin = C.deleteMin === Map.deleteMin

t_deleteMax :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_deleteMax = C.deleteMax === Map.deleteMax

deleteFindAll :: (m -> Bool) -> (m -> (a, m)) -> m -> [a]
deleteFindAll isEmpty deleteFind m0 = unfoldr maybeDeleteFind m0
  where maybeDeleteFind m
          | isEmpty m = Nothing
          | otherwise = Just . deleteFind $ m

t_deleteFindMin :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_deleteFindMin _ (KV kvs) =
    deleteFindAll C.null C.deleteFindMin (C.fromList kvs) ==
    deleteFindAll Map.null Map.deleteFindMin (Map.fromList kvs)

t_deleteFindMax :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_deleteFindMax _ (KV kvs) =
    deleteFindAll C.null C.deleteFindMax (C.fromList kvs) ==
    deleteFindAll Map.null Map.deleteFindMax (Map.fromList kvs)

t_minView :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_minView _ (KV kvs) =
  unfoldr C.minView (C.fromList kvs) ==
  unfoldr Map.minView (Map.fromList kvs)

t_maxView :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_maxView _ (KV kvs) =
  unfoldr C.maxView (C.fromList kvs) ==
  unfoldr Map.maxView (Map.fromList kvs)

t_minViewWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_minViewWithKey _ (KV kvs) =
  unfoldr C.minViewWithKey (C.fromList kvs) ==
  unfoldr Map.minViewWithKey (Map.fromList kvs)

t_maxViewWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_maxViewWithKey _ (KV kvs) =
  unfoldr C.maxViewWithKey (C.fromList kvs) ==
  unfoldr Map.maxViewWithKey (Map.fromList kvs)

updateFun :: Integral v => k -> v -> Maybe v
updateFun _ v
  | v `rem` 2 == 0 = Nothing
  | otherwise = Just (v + 1)

t_updateMinWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_updateMinWithKey =
    C.updateMinWithKey updateFun === Map.updateMinWithKey updateFun

t_updateMaxWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_updateMaxWithKey =
    C.updateMaxWithKey updateFun === Map.updateMaxWithKey updateFun

t_insert_present :: (CritBitKey k, Ord k) => k -> V -> V -> KV k -> Bool
t_insert_present k v v' =
    ((C.insert k v' . C.insert k v) === (Map.insert k v' . Map.insert k v)) k

t_insert_missing :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_insert_missing k v kvs = (C.insert k v === Map.insert k v) k kvs

t_insertWith_present :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_insertWith_present k v = ((C.insertWith (+) k v . C.insert k v) ===
                            (Map.insertWith (+) k v . Map.insert k v)) k

t_insertWith_missing :: (CritBitKey k, Ord k) => k -> V -> KV k -> Bool
t_insertWith_missing k v = ((C.insertWith (+) k v . C.delete k) ===
                            (Map.insertWith (+) k v . Map.delete k)) k

t_insertWithKey_present :: (CritBitKey k, Ord k) => k -> k -> V -> KV k -> Bool
t_insertWithKey_present _ k v (KV kvs) = Map.toList m == C.toList c
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 + v2
    m = Map.insertWithKey f k v $ Map.insert k v $ Map.fromList kvs
    c =   C.insertWithKey f k v $   C.insert k v $   C.fromList kvs

t_insertWithKey_missing :: (CritBitKey k, Ord k) => k -> k -> V -> KV k -> Bool
t_insertWithKey_missing _ k v (KV kvs) = Map.toList m == C.toList c
  where
    f key v1 v2 = fromIntegral (C.byteCount key) + v1 + v2
    m = Map.insertWithKey f k v $ Map.fromList kvs
    c =   C.insertWithKey f k v $   C.fromList kvs

t_foldMap :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldMap = isoWith (foldMap Sum) (foldMap Sum) id id

t_mapWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_mapWithKey = C.mapWithKey f === Map.mapWithKey f
  where f _ = show . (+3)

#if MIN_VERSION_containers(0,5,0)
t_traverseWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
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

t_alter :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_alter = alter f
  where
    f Nothing = Just 1
    f j       = fmap (+ 1) j

t_alter_delete :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_alter_delete = alter (const Nothing)

t_partitionWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_partitionWithKey _ (KV kvs) = partCrit == partMap
  where
    fixup f (a,b) = (f a, f b)
    predicate k _ = odd $ C.byteCount k
    partCrit = fixup C.toList . C.partitionWithKey predicate . C.fromList $ kvs
    partMap  = fixup Map.toList . Map.partitionWithKey predicate . Map.fromList $ kvs

t_partition :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_partition _ (KV kvs) = partCrit == partMap
  where
    fixup f (a,b) = (f a, f b)
    partCrit = fixup C.toList . C.partition odd . C.fromList $ kvs
    partMap  = fixup Map.toList . Map.partition odd . Map.fromList $ kvs

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, Show k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList_toList" $ t_fromList_toList t
  , testProperty "t_fromList_size" $ t_fromList_size t
  , testProperty "t_fromListWith_toList" $ t_fromListWith_toList t
  , testProperty "t_fromListWith_size" $ t_fromListWith_size t
  , testProperty "t_fromListWithKey_toList" $ t_fromListWithKey_toList t
  , testProperty "t_fromListWithKey_size" $ t_fromListWithKey_size t
  , testProperty "t_null" $ t_null t
  , testProperty "t_lookup_present" $ t_lookup_present t
  , testProperty "t_lookup_missing" $ t_lookup_missing t
#if MIN_VERSION_containers(0,5,0)
  , testProperty "t_lookupGT" $ t_lookupGT t
  , testProperty "t_lookupGE" $ t_lookupGE t
  , testProperty "t_lookupLT" $ t_lookupLT t
  , testProperty "t_lookupLE" $ t_lookupLE t
#endif
  , testProperty "t_delete_present" $ t_delete_present t
  , testProperty "t_adjust_present" $ t_updateWithKey_present t
  , testProperty "t_adjust_missing" $ t_updateWithKey_missing t
  , testProperty "t_adjustWithKey_present" $ t_updateWithKey_present t
  , testProperty "t_adjustWithKey_missing" $ t_updateWithKey_missing t
  , testProperty "t_updateWithKey_present" $ t_updateWithKey_present t
  , testProperty "t_updateWithKey_missing" $ t_updateWithKey_missing t
  , testProperty "t_update_present" $ t_update_present t
  , testProperty "t_update_missing" $ t_update_missing t
  , testProperty "t_updateLookupWithKey_present" $ t_updateWithKey_present t
  , testProperty "t_updateLookupWithKey_missing" $ t_updateWithKey_missing t
  , testProperty "t_mapMaybeWithKey" $ t_mapMaybeWithKey t
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
  , testProperty "t_map" $ t_map t
  , testProperty "t_mapWithKey" $ t_mapWithKey t
  , testProperty "t_mapKeys" $ t_map t
  , testProperty "t_mapAccumWithKey"$ t_mapAccumWithKey t
  , testProperty "t_mapAccumRWithKey"$ t_mapAccumRWithKey t
  , testProperty "t_toAscList" $ t_toAscList t
  , testProperty "t_toDescList" $ t_toDescList t
  , testProperty "t_fromAscList" $ t_fromAscList t
  , testProperty "t_fromAscListWith" $ t_fromAscListWith t
  , testProperty "t_fromAscListWithKey" $ t_fromAscListWithKey t
  , testProperty "t_fromDistinctAscList" $ t_fromDistinctAscList t
  , testProperty "t_insertWithKey_present" $ t_insertWithKey_present t
  , testProperty "t_insertWithKey_missing" $ t_insertWithKey_missing t
  , testProperty "t_filter" $ t_filter t
  , testProperty "t_split_present" $ t_split_present t
  , testProperty "t_split_missing" $ t_split_missing t
  , testProperty "t_splitLookup_present" $ t_split_present t
  , testProperty "t_splitLookup_missing" $ t_split_missing t
  , testProperty "t_isSubmapOf_ambiguous" $ t_isSubmapOfBy_ambiguous t
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
  , testProperty "t_insert_present" $ t_insert_present t
  , testProperty "t_insert_missing" $ t_insert_missing t
  , testProperty "t_insertWith_present" $ t_insertWith_present t
  , testProperty "t_insertWith_missing" $ t_insertWith_missing t
  , testProperty "t_insertWithKey_present" $ t_insertWithKey_present t
  , testProperty "t_insertWithKey_missing" $ t_insertWithKey_missing t
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

-- Handy functions for fiddling with from ghci.

blist :: [ByteString] -> CritBit ByteString Word8
blist = C.fromList . flip zip [0..]

tlist :: [Text] -> CritBit Text Word8
tlist = C.fromList . flip zip [0..]

mlist :: [ByteString] -> Map ByteString Word8
mlist = Map.fromList . flip zip [0..]

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }
