{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Common
    (
      V
    , KV(..)
    , Small(..)
    , unsquare
    , smallArbitrary
    , qc
    , Eq'(..)
    , (=?=)
    ) where

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
import qualified Data.CritBit.Set as CS
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

instance (CritBitKey k, Arbitrary k) =>
  Arbitrary (CS.Set k) where
    arbitrary = CS.fromList <$> arbitrary
    shrink = map CS.fromList . shrink . CS.toList

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

infix 4 =^=, =?=

-- | Compares heterogeneous values
class Eq' f g where
  (=^=) :: f -> g -> Bool

instance (Eq t) => Eq' t t where
  (=^=) = (==)

instance (Eq' a1 b1, Eq' a2 b2, Eq' a3 b3) => Eq' (a1, a2, a3) (b1, b2, b3)
  where (a1, a2, a3) =^= (b1, b2, b3) = a1 =^= b1 && a2 =^= b2 && a3 =^= b3

-- | Compares functions taking one scalar
(=?=) :: Eq' a b => (t -> a) -> (t -> b) -> k -> t -> Bool
f =?= g = const $ \t -> f t =^= g t

-- Handy functions for fiddling with from ghci.

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }

