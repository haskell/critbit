{-# LANGUAGE CPP, Rank2Types, ScopedTypeVariables #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Exception (catch, evaluate)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Criterion.Types (Pure)
import Data.Hashable (Hashable(..), hashByteArray)
import Data.Maybe (fromMaybe)
import Data.Text.Array (aBA)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Internal (Text(..))
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioError, isDoesNotExistError)
import System.Random.MWC (GenIO, GenST, asGenST, create, uniform, uniformR)
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

#if 0
instance Hashable Text where
    hash (Text arr off len) = hashByteArray (aBA arr) (off * 2) (len * 2)
    {-# INLINE hash #-}
#endif

instance (NFData a) => NFData (Trie.Trie a) where
    rnf = rnf . Trie.toList

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
  fileName <- fromMaybe "/usr/share/dict/words" <$> lookupEnv "WORDS"
  ordKeys <- (every 5 . B.words) <$> B.readFile fileName
             `catch` \(err::IOError) -> do
               when (isDoesNotExistError err) $ do
                 hPutStrLn stderr
                    ("(point the 'WORDS' environment variable at a file " ++
                     "to use it for benchmark data)")
               ioError err
  let b_ordKVs = zip ordKeys [(0::Int)..]
      b_revKVs = reverse b_ordKVs
  b_randKVs <- do
    gen <- create
    let kvVec = V.fromList b_ordKVs
    (G.toList . G.backpermute kvVec) <$>
      G.replicateM (G.length kvVec) (uniformR (0, G.length kvVec - 1) gen)
  let t_ordKVs  = map (first decodeUtf8) b_ordKVs
      t_randKVs = map (first decodeUtf8) b_randKVs
      t_revKVs = map (first decodeUtf8) b_revKVs
      b_critbit = C.fromList b_ordKVs
      b_map = Map.fromList b_ordKVs
      b_hashmap = H.fromList b_ordKVs
      b_trie = Trie.fromList b_ordKVs
      key = fst . head $ b_randKVs
      b_critbit_1 = C.delete key b_critbit
      b_map_1 = Map.delete key b_map
      b_hashmap_1 = H.delete key b_hashmap
      b_trie_1 = Trie.delete key b_trie
      (b_randKVs_13, b_randKVs_23) = (take (l - n) b_randKVs, drop n b_randKVs)
        where
          l = length b_randKVs
          n = l `div` 3
      b_critbit_13 = C.fromList b_randKVs_13
      b_critbit_23 = C.fromList b_randKVs_23
      b_map_13 = Map.fromList b_randKVs_13
      b_map_23 = Map.fromList b_randKVs_23
      b_hashmap_13 = H.fromList b_randKVs_13
      b_hashmap_23 = H.fromList b_randKVs_23
      b_trie_13 = Trie.fromList b_randKVs_13
      b_trie_23 = Trie.fromList b_randKVs_23
      fromList kvs = [
          bench "critbit" $ whnf C.fromList kvs
        , bench "map" $ whnf Map.fromList kvs
        , bench "hashmap" $ whnf H.fromList kvs
        ]
      keyed critbit map hashmap trie =
        [
          bgroup "present" [
              bench "critbit" $ whnf (critbit key) b_critbit
            , bench "map" $ whnf (map key) b_map
            , bench "hashmap" $ whnf (hashmap key) b_hashmap
            , bench "trie" $ whnf (trie key) b_trie
          ]
        , bgroup "missing" [
              bench "critbit" $ whnf (critbit key) b_critbit_1
            , bench "map" $ whnf (map key) b_map_1
            , bench "hashmap" $ whnf (hashmap key) b_hashmap_1
            , bench "trie" $ whnf (trie key) b_trie_1
          ]
        ]
      twoMaps critbit map hashmap trie = [
          bench "critbit" $ whnf (critbit b_critbit_13) b_critbit_23
        , bench "map" $ whnf (map b_map_13) b_map_23
        , bench "hashmap" $ whnf (hashmap b_hashmap_13) b_hashmap_23
        , bench "trie" $ whnf (trie b_trie_13) b_trie_23
        ]
      function (eval :: forall a b. NFData b => (a -> b) -> a -> Pure)
               critbit map hashmap trie = [
         bench "critbit" $ eval critbit b_critbit
       , bench "map" $ eval map b_map
       , bench "hashmap" $ eval hashmap b_hashmap
       , bench "trie" $ eval trie b_trie
       ]
  evaluate $ rnf [rnf b_critbit, rnf b_critbit_1, rnf b_map, rnf b_map_1,
                  rnf b_hashmap, rnf b_hashmap_1, rnf b_trie, rnf b_trie_1,
                  rnf b_randKVs, rnf b_revKVs, rnf key,
                  rnf b_critbit_13, rnf b_critbit_23,
                  rnf b_map_13, rnf b_map_23,
                  rnf b_hashmap_13, rnf b_hashmap_23,
                  rnf b_trie_13, rnf b_trie_23]
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
      , bgroup "delete" $ keyed C.delete Map.delete H.delete Trie.delete
      , bgroup "insert" $ keyed (flip C.insert 1) (flip Map.insert 1)
                                (flip H.insert 1) (flip Trie.insert 1)
      , bgroup "lookup" $ keyed C.lookup Map.lookup H.lookup Trie.lookup
      , bgroup "lookupGT" $ [
          bench "critbit" $ whnf (C.lookupGT key) b_critbit
        , bench "map" $ whnf (Map.lookupGT key) b_map
        ]
      , bgroup "member" $ keyed C.member Map.member H.member Trie.member
      , bgroup "foldlWithKey'" $ let f a _ b = a + b
                                 in function whnf (C.foldlWithKey' f 0)
                                    (Map.foldlWithKey' f 0)
                                    (H.foldlWithKey' f 0) id
      , bgroup "foldl'" $ function whnf (C.foldl' (+) 0) (Map.foldl' (+) 0)
                          (H.foldl' (+) 0) id
      , bgroup "union" $ twoMaps C.unionR Map.union H.union Trie.unionR
      ]
    , bgroup "text" [
        bgroup "fromList" [
          bgroup "ordered" $ fromList t_ordKVs
        , bgroup "random" $ fromList t_randKVs
        , bgroup "reversed" $ fromList t_revKVs
        ]
      ]
    ]
