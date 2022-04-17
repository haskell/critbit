{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Common
    (
      Small(..)
    , qc
    , Props
    , Eq'(..)
    , SameAs(..)
    , (=?=)
    , (=??=)
    , (=*=)
    , (=?*=)
    , (=??*=)
    , (=**=)
    , (=*==)
    , notEmpty
    , prepends
    , kf
  ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as B
import Data.CritBit.Map.Lazy (CritBitKey, byteCount)
import Data.Monoid (Monoid, mappend)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.Word
import Test.Framework (Test)
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Gen (Gen, resize, sized)
import Test.QuickCheck.Property (Property, Testable, (===), (.&&.), (.||.))

instance IsString (U.Vector Word8) where
    fromString = fromStringV

instance IsString (U.Vector Word16) where
    fromString = fromStringV

instance IsString (U.Vector Word32) where
    fromString = fromStringV

instance IsString (U.Vector Word64) where
    fromString = fromStringV

instance IsString (U.Vector Word) where
    fromString = fromStringV

instance IsString (U.Vector Char) where
    fromString = G.fromList

fromStringV :: (G.Vector v a, Integral a) => String -> v a
fromStringV = G.fromList . map (fromIntegral . fromEnum)

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink    = map B.pack . shrink . B.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink    = map T.pack . shrink . T.unpack

instance Arbitrary (U.Vector Word8) where
    arbitrary = arbitraryV
    shrink    = shrinkV

instance Arbitrary (U.Vector Word16) where
    arbitrary = arbitraryV
    shrink    = shrinkV

instance Arbitrary (U.Vector Word32) where
    arbitrary = arbitraryV
    shrink    = shrinkV

instance Arbitrary (U.Vector Word64) where
    arbitrary = arbitraryV
    shrink    = shrinkV

instance Arbitrary (U.Vector Word) where
    arbitrary = arbitraryV
    shrink    = shrinkV

instance Arbitrary (U.Vector Char) where
    arbitrary = arbitraryV
    shrink    = shrinkV

arbitraryV :: (G.Vector v a, Arbitrary a) => Gen (v a)
arbitraryV = G.fromList <$> arbitrary

shrinkV :: (G.Vector v a, Arbitrary a) => v a -> [v a]
shrinkV = map G.fromList . shrink . G.toList

newtype Small a = Small { fromSmall :: a }
    deriving (Eq, Ord, Show)

instance (Show a, Arbitrary a) => Arbitrary (Small a) where
    arbitrary = Small <$> (sized $ \n -> resize (smallish n) arbitrary)
      where
        smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs
    shrink = map Small . shrink . fromSmall

type Props k = (Arbitrary k, CritBitKey k, Ord k, IsString k, Monoid k, Show k) => k -> [Test]

infix 4 =^=, =?=, =??=

-- | Compares heterogeneous values
class (Show f, Show g) => Eq' f g where
  (=^=) :: f -> g -> Property

instance (Show t, Eq t) => Eq' t t where
  (=^=) = (===)

instance (Eq' a1 b1, Eq' a2 b2, Eq' a3 b3) => Eq' (a1, a2, a3) (b1, b2, b3)
  where (a1, a2, a3) =^= (b1, b2, b3) = a1 =^= b1 .&&. a2 =^= b2 .&&. a3 =^= b3

-- | Compares functions taking one scalar
(=?=) :: Eq' a b => (t -> a) -> (t -> b) -> k -> t -> Property
f =?= g = const $ \t -> f t =^= g t

-- | Compares functions taking two scalars
(=??=) :: Eq' a b => (t -> s -> a) -> (t -> s -> b) -> k -> t -> s -> Property
f =??= g = const $ \t s -> f t s =^= g t s

infix 4 =*=, =?*=, =*==

-- | Types 'f' and 'g' have same behavior and common representation 'r'.
data SameAs f g r = SameAs {
    toF   :: r -> f
  , fromF :: f -> r
  , toG   :: r -> g
  , fromG :: g -> r
  }

-- | Compares two functions taking one container
(=*=) :: (Eq' a b) => (f -> a) -> (g -> b)
      -> SameAs f g r -> r -> Property
(f =*= g) sa i = f (toF sa i) =^= g (toG sa i)

-- | Compares two functions taking one scalar and one container
(=?*=) :: (Eq' a b) => (t -> f -> a) -> (t -> g -> b)
       -> SameAs f g r -> r -> t -> Property
(f =?*= g) sa i t = (f t =*= g t) sa i

-- | Compares functions taking two scalars and one container
(=??*=) :: (Eq' a b) => (t -> s -> f -> a) -> (t -> s -> g -> b)
        -> SameAs f g r -> r -> t -> s -> Property
(f =??*= g) sa i t s = (f t s =*= g t s) sa i

-- | Compares two functions taking two containers
(=**=) :: (Eq' a b) => (f -> f -> a) -> (g -> g -> b)
       -> SameAs f g r -> r -> r -> Property
(f =**= g) sa i = (f (toF sa i) =*= g (toG sa i)) sa

-- | Compares two functions taking one container with preprocessing
(=*==) :: (Eq' f g) => (z -> f) -> (z -> g) -> (p -> z)
       -> SameAs f g r -> p -> Property
(f =*== g) p _ i = f i' =^= g i'
  where i' = p i

-- | Input list is non-empty
notEmpty :: (SameAs c1 c2 [i] -> [i] -> Property)
         -> SameAs c1 c2 [i] -> [i] -> Property
notEmpty f t items = null items .||. f t items

prepends :: (IsString k, Monoid k) => k -> k
prepends = mappend "test"

-- | Keys mapping function
kf :: (CritBitKey k, IsString k, Monoid k) => k -> k
kf k = fromString (show (byteCount k)) `mappend` k

-- Handy functions for fiddling with from ghci.

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }
