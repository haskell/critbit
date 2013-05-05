{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Data.Hashable (Hashable(..), hashByteArray)
import Data.Text.Array (aBA)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Internal (Text(..))
import System.Random.MWC -- (GenST, asGenST, uniform, uniformR, withSystemRandom)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Trie as Trie
import qualified Data.Vector.Generic.Mutable as M
import Control.Monad.ST

#if 0
instance Hashable Text where
    hash (Text arr off len) = hashByteArray (aBA arr) (off * 2) (len * 2)
    {-# INLINE hash #-}
#endif

every k = go 0
  where
    go i (x:xs)
        | i == k-1  = x : go 0 xs
        | otherwise = go (i+1) xs
    go _ _ = []

shuffle :: GenIO -> Double -> [Int] -> IO [Int]
shuffle gen prob xs = do
  let vec = V.fromList xs
      len = G.length vec
  v <- G.unsafeThaw vec
  let go i | i == 1 = return ()
           | otherwise = do
                   p <- uniform gen
                   when (p <= prob) $
                     M.unsafeSwap v i =<< uniformR (0, i) gen
                   go (i-1)
  go (len - 1)
  V.toList <$> G.unsafeFreeze v

chartres = do
  let xs = [0..2999]
      nxs = fromIntegral (length xs) :: Double
      go pct = do
        gen <- create
        let prob = fromIntegral pct / 100
        ys <- shuffle gen prob xs
        let mismatches = length . filter id . zipWith (/=) xs $ ys
        putStrLn $ show prob ++ " " ++ show (fromIntegral mismatches / nxs)
  mapM_ go [0..100]


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
      b_critbit = C.fromList b_ordKVs
      b_map = Map.fromList b_ordKVs
      b_hashmap = H.fromList b_ordKVs
      b_trie = Trie.fromList b_ordKVs
      fromList kvs = [
          bench "critbit" $ whnf C.fromList kvs
        , bench "map" $ whnf Map.fromList kvs
        , bench "hashmap" $ whnf H.fromList kvs
        ]
  defaultMain
    [ bgroup "bytestring" [
        bgroup "fromList" [
          bgroup "ordered" $ fromList b_ordKVs ++
                             [ bench "trie" $ whnf Trie.fromList b_ordKVs ]
        , bgroup "random" $ fromList b_randKVs ++
                            [ bench "trie" $ whnf Trie.fromList b_randKVs ]
        , bgroup "reversed" $ fromList b_revKVs ++
                              [ bench "trie" $ whnf Trie.fromList b_revKVs ]
        ]
      , bgroup "delete" $
        let k = fst . head $ b_randKVs in
        [
          bgroup "present" [
              bench "critbit" $ whnf (C.delete k) b_critbit
            , bench "map" $ whnf (Map.delete k) b_map
            , bench "hashmap" $ whnf (H.delete k) b_hashmap
            , bench "trie" $ whnf (Trie.delete k) b_trie
          ]
        , bgroup "missing" [
              bench "critbit" $ whnf (C.delete k) (C.delete k b_critbit)
            , bench "map" $ whnf (Map.delete k) (Map.delete k b_map)
            , bench "hashmap" $ whnf (H.delete k) (H.delete k b_hashmap)
            , bench "trie" $ whnf (Trie.delete k) (Trie.delete k b_trie)
          ]
        ]
      , bgroup "lookup" $
        let k = fst . head $ b_randKVs in
        [
          bgroup "present" [
              bench "critbit" $ whnf (C.lookup k) b_critbit
            , bench "map" $ whnf (Map.lookup k) b_map
            , bench "hashmap" $ whnf (H.lookup k) b_hashmap
            , bench "trie" $ whnf (Trie.lookup k) b_trie
          ]
        , bgroup "missing" [
              bench "critbit" $ whnf (C.lookup k) (C.delete k b_critbit)
            , bench "map" $ whnf (Map.lookup k) (Map.delete k b_map)
            , bench "hashmap" $ whnf (H.lookup k) (H.delete k b_hashmap)
            , bench "trie" $ whnf (Trie.lookup k) (Trie.delete k b_trie)
          ]
        ]
      , bgroup "size" [
            bench "critbit" $ whnf C.size b_critbit
          , bench "map" $ whnf Map.size b_map
          , bench "hashmap" $ whnf H.size b_hashmap
          , bench "trie" $ whnf Trie.size b_trie
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
