{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Common
    (
      V
    , Small(..)
    , unsquare
    , smallArbitrary
    , qc
    , Eq'(..)
    , SameAs (..)
    , (=?=)
    , (=??=)
    , (=*=)
    , (=?*=)
    , (=??*=)
    , (=**=)
    , (=*==)
    , notEmpty
  ) where

import Control.Applicative ((<$>))
import Data.CritBit.Map.Lazy (CritBitKey, CritBit)
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Gen (Gen, resize, sized)
import Test.QuickCheck.Property (Property, Testable, forAll)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.CritBit.Set as CS
import qualified Data.Text as T

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink    = map B.pack . shrink . B.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink    = map T.pack . shrink . T.unpack

type V = Word8

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
    arbitrary = Small <$> (sized $ \n -> resize (smallish n) arbitrary)
      where
        smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs
    shrink = map Small . shrink . fromSmall

infix 4 =^=, =?=, =??=

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

-- | Compares functions taking two scalars
(=??=) :: Eq' a b => (t -> s -> a) -> (t -> s -> b) -> k -> t -> s -> Bool
f =??= g = const $ \t s -> f t s =^= g t s

infix 4 =*=, =?*=, =*==

-- | Types 'f' and 'g' have same behavior and common represenation 'r'.
data SameAs f g r = SameAs {
    toF   :: r -> f
  , fromF :: f -> r
  , toG   :: r -> g
  , fromG :: g -> r
  }

-- | Compares two functions taking one container
(=*=) :: (Eq' a b) => (f -> a) -> (g -> b)
      -> SameAs f g r -> r -> Bool
(f =*= g) sa i = f (toF sa i) =^= g (toG sa i)

-- | Compares two functions taking one scalar and one container
(=?*=) :: (Eq' a b) => (t -> f -> a) -> (t -> g -> b)
       -> SameAs f g r -> r -> t -> Bool
(f =?*= g) sa i t = (f t =*= g t) sa i

-- | Compares functions taking two scalars and one container
(=??*=) :: (Eq' a b) => (t -> s -> f -> a) -> (t -> s -> g -> b)
        -> SameAs f g r -> r -> t -> s -> Bool
(f =??*= g) sa i t s = (f t s =*= g t s) sa i

-- | Compares two functions taking two containers
(=**=) :: (Eq' a b) => (f -> f -> a) -> (g -> g -> b)
       -> SameAs f g r -> r -> r -> Bool
(f =**= g) sa i = (f (toF sa i) =*= g (toG sa i)) sa

-- | Compares two functions taking one container with preprocessing
(=*==) :: (Eq' f g) => (z -> f) -> (z -> g) -> (p -> z)
       -> SameAs f g r -> p -> Bool
(f =*== g) p _ i = f i' =^= g i'
  where i' = p i

-- | Input litst is non-empty
notEmpty :: (SameAs c1 c2 [i] -> [i] -> Bool) -> SameAs c1 c2 [i] -> [i] -> Bool
notEmpty f t items = null items || f t items

-- Handy functions for fiddling with from ghci.

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }

