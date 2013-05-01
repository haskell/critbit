module Main (main) where

import Control.Applicative ((<$>))
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config (defaultConfig)
import Criterion.Main (bench, bgroup, defaultMainWith, whnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.CritBit.Map.Lazy as C
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as Map

main = do
  keys <- B.lines <$> B.readFile "/usr/share/dict/words"
  let kvs = zip keys [(0::Int)..]
  defaultMainWith
    defaultConfig
    (liftIO . evaluate $ rnf [kvs])
    [ bgroup "critbit" [
        bench "fromList" $ whnf C.fromList kvs
      ]
    , bgroup "map" [
        bench "fromList" $ whnf Map.fromList kvs
      ]
    , bgroup "hashmap" [
        bench "fromList" $ whnf H.fromList kvs
      ]
    ]
