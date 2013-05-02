module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Hashable (Hashable(..), hashByteArray)
import Data.Text.Array (aBA)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Internal (Text(..))
import System.Random.MWC (asGenST, uniformR, withSystemRandom)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

instance Hashable Text where
    hash (Text arr off len) = hashByteArray (aBA arr) (off * 2) (len * 2)
    {-# INLINE hash #-}

every k = go 0
  where
    go i (x:xs)
        | i == k-1  = x : go 0 xs
        | otherwise = go (i+1) xs
    go _ _ = []

main = do
  ordKeys <- (every 5 . B.lines) <$> B.readFile "/usr/share/dict/words"
  let b_ordKVs = zip ordKeys [(0::Int)..]
      b_revKVs = reverse b_ordKVs
  b_randKVs <- withSystemRandom . asGenST $ \gen ->
               let kvVec = V.fromList b_ordKVs
               in (G.toList . G.backpermute kvVec) <$>
                  G.replicateM (G.length kvVec)
                               (uniformR (0, G.length kvVec - 1) gen)
  let t_ordKVs  = map (first decodeUtf8) b_ordKVs
      t_randKVs = map (first decodeUtf8) b_randKVs
      t_revKVs = map (first decodeUtf8) b_revKVs
  let fromList kvs = [
          bench "critbit" $ whnf C.fromList kvs
        , bench "map" $ whnf Map.fromList kvs
        , bench "hashmap" $ whnf H.fromList kvs
        ]
  defaultMain
    [ bgroup "bytestring" [
        bgroup "fromList" [
          bgroup "ordered" $ fromList b_ordKVs
        , bgroup "random" $ fromList b_randKVs
        , bgroup "reversed" $ fromList b_revKVs
        ]
      ]
    , bgroup "text" [
        bgroup "fromList" [
          bgroup "ordered" $ fromList t_ordKVs
        , bgroup "random" $ fromList t_randKVs
        , bgroup "reversed" $ fromList t_revKVs
        ]
      ]
    ]
