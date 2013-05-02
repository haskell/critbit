{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties
    where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.CritBit.Map.Lazy (CritBitKey)
import Data.Text (Text)
import Data.Word (Word8)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
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

blist :: [ByteString] -> C.CritBit ByteString Word8
blist = C.fromList . flip zip [0..]

tlist :: [Text] -> C.CritBit Text Word8
tlist = C.fromList . flip zip [0..]

mlist :: [ByteString] -> Map.Map ByteString Word8
mlist = Map.fromList . flip zip [0..]

qc n = quickCheckWith stdArgs { maxSuccess = n }

t_fromList_lookups :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_lookups _ (KV kvs) =
    all (\(k,_) -> Map.lookup k m == C.lookup k c) kvs
  where m = Map.fromList kvs
        c = C.fromList kvs

t_fromList_toList :: (CritBitKey k, Ord k) => k -> KV k -> Bool
t_fromList_toList _ (KV kvs) =
    Map.toList (Map.fromList kvs) == C.toList (C.fromList kvs)

t_delete_present :: (CritBitKey k, Ord k) => k -> KV k -> k -> V -> Bool
t_delete_present _ (KV kvs) k v =
    C.toList (C.delete k c) == Map.toList (Map.delete k m)
  where
    c = C.insert k v $ C.fromList kvs
    m = Map.insert k v $ Map.fromList kvs

propertiesFor :: (Arbitrary k, CritBitKey k, Ord k, Show k) => k -> [Test]
propertiesFor t = [
    testProperty "t_fromList_toList" $ t_fromList_toList t
  , testProperty "t_fromList_lookups" $ t_fromList_lookups t
  , testProperty "t_delete_present" $ t_delete_present t
  ]

properties :: [Test]
properties = [
    testGroup "text" $ propertiesFor T.empty
  , testGroup "bytestring" $ propertiesFor B.empty
  ]
