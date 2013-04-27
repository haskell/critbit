module Main (main) where

import Test.Framework (defaultMain, testGroup)
import Properties (properties)

main = defaultMain [
         testGroup "properties" properties
       ]
