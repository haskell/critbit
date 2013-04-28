module Properties
    where

import Control.Applicative
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Map as Map
import qualified Data.CritBit as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BB
import Data.ByteString (ByteString)
import Data.Word

-- We can't yet handle null bytes, so ensure that they can neither be
-- generated nor produced in a shrink.
instance Arbitrary ByteString where
    arbitrary = BB.pack <$> listOf1 (choose (1, 255))
    shrink    = map (B.map succ . B.pack . fne) .
                shrink . NonEmpty . B.unpack . B.map pred
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

qc n = quickCheckWith stdArgs { maxSuccess = n }

t_fromList_eq :: KV -> Bool
t_fromList_eq (KV kvs) =
    all (\(k,_) -> Map.lookup k m == C.lookup k c) kvs
  where m = Map.fromList kvs
        c = C.fromList kvs

properties = [
  ]
