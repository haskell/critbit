{-# LANGUAGE CPP, Rank2Types, ScopedTypeVariables #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Criterion.Types (Pure)
import Data.Foldable (foldMap)
import Data.Functor.Identity (Identity(..))
import Data.Hashable (Hashable(..), hashByteArray)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid (Sum(..))
import Data.Text.Array (aBA)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Internal (Text(..))
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioError, isDoesNotExistError)
import System.Random.MWC (GenIO, GenST, asGenST, create, uniform, uniformR)
import qualified Control.Exception as Exc
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as TC
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L

#if 0
instance Hashable Text where
    hash (Text arr off len) = hashByteArray (aBA arr) (off * 2) (len * 2)
    {-# INLINE hash #-}
#endif

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif

instance (NFData a) => NFData (Trie.Trie a) where
    rnf = rnf . Trie.toList

forceTuple:: (a,a) -> (a,a)
forceTuple (a,b) = a `seq` b `seq` (a,b)

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

mapFKey :: (Num v, C.CritBitKey k) => k -> v -> v
mapFKey _ x = x + 1

mapAccumFKey :: (C.CritBitKey k, Num v) => Int -> k -> v -> (Int, v)
mapAccumFKey a _ v = (a + 1, v + 1)

updateFKey :: Num v => k -> v -> Maybe v
updateFKey _ v = Just $ v + 1

updateFVal :: Num v => v -> Maybe v
updateFVal v = updateFKey undefined v

main = do
  fileName <- getEnv "WORDS" `Exc.catch` \(_::IOError) ->
              return "/usr/share/dict/words"
  ordKeys <- L.sort <$> (every 5 . B.words) <$> B.readFile fileName
             `Exc.catch` \(err::IOError) -> do
               when (isDoesNotExistError err) $ do
                 hPutStrLn stderr
                    ("(point the 'WORDS' environment variable at a file " ++
                     "to use it for benchmark data)")
               ioError err
  let b_ordKVs = zip ordKeys [(0::Int)..]
      prefix = B.concat $ L.map fst b_ordKVs
      b_longKVs = map (first (B.append prefix)) b_ordKVs
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
      fromListWith kvs = [
          bench "critbit" $ whnf (C.fromListWith (\a b -> a+b)) kvs
        , bench "map" $ whnf (Map.fromListWith (\a b -> a+b)) kvs
        , bench "hashmap" $ whnf (H.fromListWith (\a b -> a+b)) kvs
        , bench "trie" $ whnf (TC.fromListWith (\a b -> a+b)) kvs
        ]
      fromListWithKey kvs = [
          bench "critbit" $ whnf (C.fromListWithKey (\k a b -> a + b)) kvs
        , bench "map" $ whnf (Map.fromListWithKey (\k a b -> a+b)) kvs
        -- , bench "hashmap" $ whnf (H.fromListWithKey (\a b -> a+b)) kvs
        -- , bench "trie" $ whnf (TC.fromListWithKey (\a b -> a+b)) kvs
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
  Exc.evaluate $ rnf [rnf b_critbit, rnf b_critbit_1, rnf b_map, rnf b_map_1,
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
      , bgroup "fromListWith" [
          bgroup "ordered" $ fromListWith b_ordKVs
        , bgroup "random" $ fromListWith b_randKVs
        , bgroup "reversed" $ fromListWith b_revKVs
        ]
      , bgroup "fromListWithKey" [
          bgroup "ordered" $ fromListWithKey b_ordKVs
        , bgroup "random" $ fromListWithKey b_randKVs
        , bgroup "reversed" $ fromListWithKey b_revKVs
        ]
      , bgroup "delete" $ keyed C.delete Map.delete H.delete Trie.delete
      , bgroup "insert" $ keyed (flip C.insert 1) (flip Map.insert 1)
                                (flip H.insert 1) (flip Trie.insert 1)
      , bgroup "insertWith" $ [
          bgroup "present" $ [
            bench "critbit" $ whnf (C.insertWith (+) key 1) b_critbit
          , bench "map" $ whnf (Map.insertWith (+) key 1) b_map
          , bench "hashmap" $ whnf (H.insertWith (+) key 1) b_hashmap
          ]
        , bgroup "missing" $ [
            bench "critbit" $ whnf (C.insertWith (+) key 1) b_critbit_1
          , bench "map" $ whnf (Map.insertWith (+) key 1) b_map_1
          , bench "hashmap" $ whnf (H.insertWith (+) key 1) b_hashmap_1
          ]
        ]
      , bgroup "insertWithKey" $ let f _ a b = a + b in [
          bgroup "present" [
            bench "critbit" $ whnf (C.insertWithKey f key 1) b_critbit
          , bench "map" $ whnf (Map.insertWithKey f key 1) b_map
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf (C.insertWithKey f key 1) b_critbit_1
          , bench "map" $ whnf (Map.insertWithKey f key 1) b_map_1
          ]
        ]
      , bgroup "adjust" $
        let f v = (v + 10) in [
          bgroup "present" [
            bench "critbit" $ whnf   (C.adjust f key) b_critbit
          , bench "map"     $ whnf (Map.adjust f key) b_map
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf   (C.adjust f key) b_critbit_1
          , bench "map"     $ whnf (Map.adjust f key) b_map_1
          ]
        ]
      , bgroup "adjustWithKey" $
        let f k v = (v + fromIntegral (C.byteCount k)) in [
          bgroup "present" [
            bench "critbit" $ whnf   (C.adjustWithKey f key) b_critbit
          , bench "map"     $ whnf (Map.adjustWithKey f key) b_map
          , bench "trie"    $ whnf  (TC.adjustWithKey f key) b_trie
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf   (C.adjustWithKey f key) b_critbit_1
          , bench "map"     $ whnf (Map.adjustWithKey f key) b_map_1
          , bench "trie"    $ whnf  (TC.adjustWithKey f key) b_trie_1
          ]
        ]
      , bgroup "updateWithKey" $
        let f k v = Just (v + fromIntegral (C.byteCount k)) in [
          bgroup "present" [
            bench "critbit" $ whnf (C.updateWithKey f key) b_critbit
          , bench "map" $ whnf (Map.updateWithKey f key) b_map
          , bench "trie" $ whnf (TC.updateWithKey f key) b_trie
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf (C.updateWithKey f key) b_critbit_1
          , bench "map" $ whnf (Map.updateWithKey f key) b_map_1
          , bench "trie" $ whnf (TC.updateWithKey f key) b_trie_1
          ]
        ]
      , bgroup "update" $
        let f = updateFVal in [
          bgroup "present" [
            bench "critbit" $ whnf (C.update f key) b_critbit
          , bench "map" $ whnf (Map.update f key) b_map
          , bench "trie" $ whnf (TC.update f key) b_trie
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf (C.update f key) b_critbit_1
          , bench "map" $ whnf (Map.update f key) b_map_1
          , bench "trie" $ whnf (TC.update f key) b_trie_1
          ]
        ]
      , bgroup "updateLookupWithKey" $
        -- The Map implementation immediately returns a tuple with lazy values,
        -- so we need to force it to evaluate the update.
        let f k v = Just (v + fromIntegral (C.byteCount k)) in [
          bgroup "present" [
            bench "critbit" $ whnf
              (snd . C.updateLookupWithKey f key) b_critbit
          , bench "map" $ whnf
              (snd . Map.updateLookupWithKey f key) b_map
          ]
        , bgroup "missing" [
            bench "critbit" $ whnf
              (snd . C.updateLookupWithKey f key) b_critbit_1
          , bench "map" $ whnf
              (snd . Map.updateLookupWithKey f key) b_map_1
          ]
        ]
      , bgroup "lookup" $ keyed C.lookup Map.lookup H.lookup Trie.lookup
#if MIN_VERSION_containers(0,5,0)
      , bgroup "lookupGT" $ [
          bench "critbit" $ whnf (C.lookupGT key) b_critbit
        , bench "map" $ whnf (Map.lookupGT key) b_map
        ]
#endif
      , bgroup "member" $ keyed C.member Map.member H.member Trie.member
      , bgroup "foldlWithKey'" $ let f a _ b = a + b
                                 in function whnf (C.foldlWithKey' f 0)
                                    (Map.foldlWithKey' f 0)
                                    (H.foldlWithKey' f 0) id
      , bgroup "foldl'" $ function whnf (C.foldl' (+) 0) (Map.foldl' (+) 0)
                          (H.foldl' (+) 0) id
      , bgroup "elems" $ function nf C.elems Map.elems H.elems Trie.elems
      , bgroup "keys" $ function nf C.keys Map.keys H.keys Trie.keys
      , bgroup "map"  $ let f = (+3)
                        in function nf (C.map f) (Map.map f) (H.map f) (fmap f)
      , bgroup "mapWithKey" $ [
          bench "critbit" $ whnf (C.mapWithKey mapFKey) b_critbit
        , bench "map" $ whnf (Map.mapWithKey mapFKey) b_map
        ]
      , bgroup "mapKeys" $ let f k = B.pack (show k ++ "test") in [
          bench "critbit" $ nf (C.mapKeys f) b_critbit
        , bench "map" $ nf (Map.mapKeys f) b_map
        ]
      , bgroup "mapAccumWithKey" $ [
          bench "critbit" $ whnf (C.mapAccumWithKey mapAccumFKey 0) b_critbit
        , bench "map" $ whnf (Map.mapAccumWithKey mapAccumFKey 0) b_map
        ]
      , bgroup "mapAccumRWithKey" $ [
          bench "critbit" $ whnf (C.mapAccumRWithKey mapAccumFKey 0) b_critbit
        , bench "map" $ whnf (Map.mapAccumRWithKey mapAccumFKey 0) b_map
        ]
      , bgroup "union" $ twoMaps C.unionR Map.union H.union Trie.unionR
      , bgroup "unionWith" [
          bench "critbit" $ whnf (C.unionWith (+) b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.unionWith (+) b_map_13) b_map_23
        ]
      , bgroup "unionWithKey" $ let f _ a b = a + b in [
          bench "critbit" $ whnf (C.unionWithKey f b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.unionWithKey f b_map_13) b_map_23
        ]
      , bgroup "unions" [
          bench "critbit" $ whnf C.unions [b_critbit_13, b_critbit_23]
        , bench "map" $ whnf Map.unions [b_map_13, b_map_23]
        ]
      , bgroup "unionsWith" [
          bench "critbit" $ whnf (C.unionsWith (+)) [b_critbit_13, b_critbit_23]
        , bench "map" $ whnf (Map.unionsWith (+)) [b_map_13, b_map_23]
        ]
      , bgroup "difference" [
          bench "critbit" $ whnf (C.difference b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.difference b_map_13) b_map_23
        , bench "hashmap" $ whnf (H.difference b_hashmap_13) b_hashmap_23
        ]
      , bgroup "differenceWith" $ let f a b = Just (a + b) in [
          bench "critbit" $ whnf (C.differenceWith f b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.differenceWith f b_map_13) b_map_23
        ]
      , bgroup "differenceWithKey" $ let f _ a b = Just(a + b) in [
          bench "critbit" $ whnf (C.differenceWithKey f b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.differenceWithKey f b_map_13) b_map_23
        ]
      , bgroup "intersection" [
          bench "critbit" $ whnf (C.intersection b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.intersection b_map_13) b_map_23
        , bench "hashmap" $ whnf (H.intersection b_hashmap_13) b_hashmap_23
        ]
      , bgroup "intersectionWith" [
          bench "critbit" $ whnf (C.intersectionWith (+) b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.intersectionWith (+) b_map_13) b_map_23
        , bench "hashmap" $ whnf (H.intersectionWith (+) b_hashmap_13) b_hashmap_23
        ]
      , bgroup "intersectionWithKey" $ let f _ a b = a + b in [
          bench "critbit" $ whnf (C.intersectionWithKey f b_critbit_13) b_critbit_23
        , bench "map" $ whnf (Map.intersectionWithKey f b_map_13) b_map_23
        ]
      , bgroup "toAscList" $ function nf C.toAscList Map.toAscList id id
      , bgroup "toDescList" $ function nf C.toDescList Map.toDescList id id
      , bgroup "fromAscList_short" [
          bench "critbit" $ nf   C.fromAscList b_ordKVs
        , bench "map"     $ nf Map.fromAscList b_ordKVs
        ]
      , bgroup "fromAscList_long" [
          bench "critbit" $ nf   C.fromAscList b_longKVs
        , bench "map"     $ nf Map.fromAscList b_longKVs
        ]
      , bgroup "fromAscListWith" [
          bench "critbit" $ nf (  C.fromAscListWith (+)) b_ordKVs
        , bench "map"     $ nf (Map.fromAscListWith (+)) b_ordKVs
        ]
      , bgroup "fromAscListWithKey" [
          bench "critbit" $ nf (  C.fromAscListWithKey (const (+))) b_ordKVs
        , bench "map"     $ nf (Map.fromAscListWithKey (const (+))) b_ordKVs
        ]
      , bgroup "fromAscDistinctList_short" [
          bench "critbit" $ nf (  C.fromDistinctAscList) b_ordKVs
        , bench "map"     $ nf (Map.fromDistinctAscList) b_ordKVs
        ]
      , bgroup "fromAscDistinctList_long" [
          bench "critbit" $ nf (  C.fromDistinctAscList) b_longKVs
        , bench "map"     $ nf (Map.fromDistinctAscList) b_longKVs
        ]
      , bgroup "filter" $ let p  = (< 128)
                              p' = \e -> if p e then Just e else Nothing
                          in  function nf (C.filter p) (Map.filter p)
                                          (H.filter p) (Trie.filterMap p')
      , bgroup "mapMaybeWithKey" $
        let f k v | even (fromIntegral v :: Int) =
                    Just (v + fromIntegral (C.byteCount k))
                  | otherwise = Nothing
        in [
          bench "critbit" $ whnf (C.mapMaybeWithKey f) b_critbit
        , bench "map" $ whnf (Map.mapMaybeWithKey f) b_map
        ]
      , bgroup "mapEitherWithKey" $
        let f k v | even (fromIntegral v :: Int) =
                    Left (v + fromIntegral (C.byteCount k))
                  | otherwise = Right (2 * v)
        in [
          bench "critbit" $ nf (C.mapEitherWithKey f) b_critbit
        , bench "map" $ nf (Map.mapEitherWithKey f) b_map
        ]
      , bgroup "split" $ [
          bench "critbit" $ whnf (forceTuple . C.split key) b_critbit
        , bench "map" $ whnf (forceTuple . Map.split key) b_map
        ]
      , bgroup "splitLookup" $
        let forceTriple (a,_,b) = a `seq` b `seq` (a,b)
        in [
          bench "critbit" $ whnf (forceTriple . C.splitLookup key) b_critbit
        , bench "map" $ whnf (forceTriple . Map.splitLookup key) b_map
        ]
      , bgroup "findMin" $ [
          bench "critbit" $ whnf (C.findMin) b_critbit
        , bench "map" $ whnf (Map.findMin) b_map
        ]
      , bgroup "findMax" $ [
          bench "critbit" $ whnf (C.findMax) b_critbit
        , bench "map" $ whnf (Map.findMax) b_map
        ]
      , bgroup "deleteMin" $ [
          bench "critbit" $ whnf (C.deleteMin) b_critbit
        , bench "map" $ whnf (Map.deleteMin) b_map
        ]
      , bgroup "deleteMax" $ [
          bench "critbit" $ whnf (C.deleteMax) b_critbit
        , bench "map" $ whnf (Map.deleteMax) b_map
       ]
      , bgroup "deleteFindMin" $ [
          bench "critbit" $ whnf (snd . C.deleteFindMin) b_critbit
        , bench "map" $ whnf (snd . Map.deleteFindMin) b_map
        ]
      , bgroup "deleteFindMax" $ [
          bench "critbit" $ whnf (snd . C.deleteFindMax) b_critbit
        , bench "map" $ whnf (snd . Map.deleteFindMax) b_map
        ]
      , bgroup "minView" $ [
          bench "critbit" $ whnf (snd . fromJust . C.minView) b_critbit
        , bench "map" $ whnf (snd . fromJust . Map.minView) b_map
        ]
      , bgroup "maxView" $ [
          bench "critbit" $ whnf (snd . fromJust . C.maxView) b_critbit
        , bench "map" $ whnf (snd . fromJust . Map.maxView) b_map
        ]
      , bgroup "minViewWithKey" $ [
          bench "critbit" $ whnf (snd . fromJust . C.minViewWithKey) b_critbit
        , bench "map" $ whnf (snd . fromJust . Map.minViewWithKey) b_map
        ]
      , bgroup "maxViewWithKey" $ [
          bench "critbit" $ whnf (snd . fromJust . C.minViewWithKey) b_critbit
        , bench "map" $ whnf (snd . fromJust . Map.minViewWithKey) b_map
        ]
      , bgroup "updateMin" $ [
          bench "critbit" $ whnf (C.updateMin updateFVal) b_critbit
        , bench "map" $ whnf (Map.updateMin updateFVal) b_map
        ]
      , bgroup "updateMax" $ [
          bench "critbit" $ whnf (C.updateMax updateFVal) b_critbit
        , bench "map" $ whnf (Map.updateMax updateFVal) b_map
        ]
      , bgroup "traverseWithKey" $ let f _ = Identity . (+3) in [
          bench "critbit" $ nf (runIdentity . C.traverseWithKey f) b_critbit
#if MIN_VERSION_containers(0,5,0)
        , bench "map" $ nf (runIdentity . Map.traverseWithKey f) b_map
#endif
        , bench "hashmap" $ nf (runIdentity . H.traverseWithKey f) b_hashmap
        , bench "trie" $ nf (fmap f) b_trie
        ]
      , bgroup "updateMinWithKey" $ [
          bench "critbit" $ whnf (C.updateMinWithKey updateFKey) b_critbit
        , bench "map" $ whnf (Map.updateMinWithKey updateFKey) b_map
        ]
      , bgroup "updateMaxWithKey" $ [
          bench "critbit" $ whnf (C.updateMaxWithKey updateFKey) b_critbit
        , bench "map" $ whnf (Map.updateMaxWithKey updateFKey) b_map
        ]
      , bgroup "foldMap" $ [
          bench "critbit" $ let c_foldmap :: (C.CritBitKey k, Num v)
                                          => C.CritBit k v
                                          -> Sum v
                                c_foldmap = foldMap Sum
                            in whnf c_foldmap b_critbit
        , bench "map" $ let m_foldmap :: (Eq k, Num v)
                                      => Map.Map k v
                                      -> Sum v
                            m_foldmap = foldMap Sum
                        in whnf m_foldmap b_map
        ]
      , bgroup "alter" $ let altF (Just v) =
                                  if odd v
                                    then Just (v+1)
                                    else Nothing
                             altF Nothing  = Just 1
                          in [
          bench "critbit" $  whnf (C.alter altF key) b_critbit
        , bench "map" $ whnf (Map.alter altF key) b_map
        ]
     , bgroup "partitionWithKey" $ let predicate k _ = odd $ C.byteCount k
                                       forceTuple (a,b) = a `seq` b `seq` (a,b)
                                   in [
          bench "critbit" $ whnf (forceTuple . C.partitionWithKey predicate) b_critbit
        , bench "map" $ whnf (forceTuple . Map.partitionWithKey predicate) b_map
     ]
     , bgroup "partition" $ [
          bench "critbit" $ whnf (forceTuple . C.partition odd) b_critbit
        , bench "map" $ whnf (forceTuple . Map.partition odd) b_map
     ]
    ]
    , bgroup "text" [
        bgroup "fromList" [
          bgroup "ordered" $ fromList t_ordKVs
        , bgroup "random" $ fromList t_randKVs
        , bgroup "reversed" $ fromList t_revKVs
        ]
      , bgroup "fromListWith" [
          bgroup "ordered" $ fromListWith b_ordKVs
        , bgroup "random" $ fromListWith b_randKVs
        , bgroup "reversed" $ fromListWith b_revKVs
        ]
      , bgroup "fromListWithKey" [
          bgroup "ordered" $ fromListWithKey b_ordKVs
        , bgroup "random" $ fromListWithKey b_randKVs
        , bgroup "reversed" $ fromListWithKey b_revKVs
        ]
      ]
    ]
