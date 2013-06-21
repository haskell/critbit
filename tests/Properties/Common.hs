{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Properties.Common
    (
      Small(..)
    , qc
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
  ) where

import Data.String (IsString)
import Data.Monoid (Monoid, mappend)
import Control.Applicative ((<$>))
import Test.QuickCheck (Arbitrary(..), Args(..), quickCheckWith, stdArgs)
import Test.QuickCheck.Gen (resize, sized)
import Test.QuickCheck.Property (Testable)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary
    shrink    = map B.pack . shrink . B.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink    = map T.pack . shrink . T.unpack

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

prepends :: (IsString k, Monoid k) => k -> k
prepends = mappend "test"

-- Handy functions for fiddling with from ghci.

qc :: Testable prop => Int -> prop -> IO ()
qc n = quickCheckWith stdArgs { maxSuccess = n }
