module Main (main) where

import Control.Applicative ((<$>))
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config (defaultConfig)
import Criterion.Main (bench, bgroup, defaultMainWith, whnf)
import System.Random.MWC (asGenST, uniformR, withSystemRandom)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

every k = go 0
  where
    go i (x:xs)
        | i == k-1  = x : go 0 xs
        | otherwise = go (i+1) xs
    go _ _ = []

main = do
  ordKeys <- (every 5 . B.lines) <$> B.readFile "/usr/share/dict/words"
  let ordKVs = zip ordKeys [(0::Int)..]
      revKVs = reverse ordKVs
  randKVs <- withSystemRandom . asGenST $ \gen ->
             let kvVec = V.fromList ordKVs
             in (G.toList . G.backpermute kvVec) <$>
                G.replicateM (G.length kvVec)
                             (uniformR (0, G.length kvVec - 1) gen)
  let fromList kvs = [
          bench "critbit" $ whnf C.fromList kvs
        , bench "map" $ whnf Map.fromList kvs
        , bench "hashmap" $ whnf H.fromList kvs
        ]
  defaultMainWith
    defaultConfig
    (liftIO . evaluate $ rnf [ordKVs, randKVs, revKVs])
    [ bgroup "fromList" [
        bgroup "ordered" $ fromList ordKVs
      , bgroup "random" $ fromList randKVs
      , bgroup "reversed" $ fromList revKVs
      ]
    ]
