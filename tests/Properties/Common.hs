{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Common
    where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.CritBit.Map.Lazy (CritBitKey, CritBit)
import Data.Text (Text)
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Gen (Gen, resize, sized)
import Test.QuickCheck.Property (Property, Testable, forAll)
import qualified Data.ByteString as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
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

-- Handy functions for fiddling with from ghci.

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }
