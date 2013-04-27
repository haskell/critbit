module Properties
    where

import Control.Applicative
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Map as Map
import qualified Data.CritBit as C
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.Word

instance Arbitrary ByteString where
    arbitrary = do
      NonEmpty s <- arbitrary
      return (B.pack s)
    shrink = map (B.pack . fne) . shrink . NonEmpty . B.unpack
        where fne (NonEmpty x) = x

newtype KV = KV { fromKV :: [(ByteString, Word8)] }
        deriving (Show, Eq, Ord)

instance Arbitrary KV where
    arbitrary = (KV . flip zip [0..]) <$> arbitrary
    shrink = map (KV . flip zip [0..]) . shrink . map fst . fromKV

clist :: [ByteString] -> C.CritBit ByteString Word8
clist = C.fromList . flip zip [0..]

mlist :: [ByteString] -> Map.Map ByteString Word8
mlist = Map.fromList . flip zip [0..]

t_fromList_eq :: KV -> Bool
t_fromList_eq (KV kvs) =
    all (\(k,_) -> Map.lookup k m == C.lookup k c) kvs
  where m = Map.fromList kvs
        c = C.fromList kvs

properties = [
  ]
