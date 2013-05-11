{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties
    where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.CritBit.Map.Lazy (CritBitKey, CritBit)
import Data.Text (Text)
import Data.Word (Word8)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Property (Testable)
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

t_lookup_present :: (CritBitKey k) => k -> k -> V -> CB k -> Bool
t_lookup_present _ k v (CB m) = C.lookup k (C.insert k v m) == Just v

t_lookup_missing :: (CritBitKey k) => k -> k -> CB k -> Bool
t_lookup_missing _ k (CB m) = C.lookup k (C.delete k m) == Nothing

#ifdef MIN_VERSION_containers(0,5,0)
t_lookupGT :: (Ord k, CritBitKey k) => k -> k -> KV k -> Bool
t_lookupGT _ k (KV kvs) =
    C.lookupGT k (C.fromList kvs) == Map.lookupGT k (Map.fromList kvs)
#endif

t_fromList_toList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_toList _ (KV kvs) =
    Map.toList (Map.fromList kvs) == C.toList (C.fromList kvs)

t_fromList_size :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_size _ (KV kvs) =
    Map.size (Map.fromList kvs) == C.size (C.fromList kvs)

t_delete_present :: (CritBitKey k, Ord k) => k -> KV k -> k -> V -> Bool
t_delete_present _ (KV kvs) k v =
    C.toList (C.delete k c) == Map.toList (Map.delete k m)
  where
    c = C.insert k v $ C.fromList kvs
    m = Map.insert k v $ Map.fromList kvs

t_unionL :: (CritBitKey k, Ord k) => k -> KV k -> KV k -> Bool
t_unionL _ (KV kv0) (KV kv1) =
    Map.toList (Map.fromList kv0 `Map.union` Map.fromList kv1) ==
    C.toList (C.fromList kv0 `C.unionL` C.fromList kv1)

t_foldl :: (CritBitKey k) => k -> CritBit k V -> Bool
t_foldl _ m = C.foldl (+) 0 m == C.foldr (+) 0 m

t_foldlWithKey :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldlWithKey _ (KV kvs) =
    C.foldlWithKey f ([], 0) (C.fromList kvs) ==
    Map.foldlWithKey f ([], 0) (Map.fromList kvs)
  where
    f (l,s) k v = (k:l,s+v)

t_foldl' :: (CritBitKey k) => k -> CritBit k V -> Bool
t_foldl' _ m = C.foldl' (+) 0 m == C.foldl (+) 0 m

t_foldlWithKey' :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_foldlWithKey' _ (KV kvs) =
    C.foldlWithKey' f ([], 0) (C.fromList kvs) ==
    Map.foldlWithKey' f ([], 0) (Map.fromList kvs)
  where
    f (l,s) k v = (k:l,s+v)

t_elems :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_elems _ (KV kvs) = C.elems (C.fromList kvs) == Map.elems (Map.fromList kvs)

t_keys :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_keys _ (KV kvs) = C.keys (C.fromList kvs) == Map.keys (Map.fromList kvs)

t_map :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_map _ (KV kvs) = mappedC == mappedM
    where fun     = show . (+3)
          mappedC = C.toList . C.map fun $ (C.fromList kvs)
          mappedM = Map.toList . Map.map fun $ (Map.fromList kvs)

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, Show k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList_toList" $ t_fromList_toList t
  , testProperty "t_fromList_size" $ t_fromList_size t
  , testProperty "t_lookup_present" $ t_lookup_present t
  , testProperty "t_lookup_missing" $ t_lookup_missing t
#ifdef MIN_VERSION_containers(0,5,0)
  , testProperty "t_lookupGT" $ t_lookupGT t
#endif
  , testProperty "t_delete_present" $ t_delete_present t
  , testProperty "t_unionL" $ t_unionL t
  , testProperty "t_foldl" $ t_foldl t
  , testProperty "t_foldlWithKey" $ t_foldlWithKey t
  , testProperty "t_foldl'" $ t_foldl' t
  , testProperty "t_foldlWithKey'" $ t_foldlWithKey' t
  , testProperty "t_elems" $ t_elems t
  , testProperty "t_keys" $ t_keys t
  , testProperty "t_map" $ t_map t
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

mlist :: [ByteString] -> Map.Map ByteString Word8
mlist = Map.fromList . flip zip [0..]

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }
