{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Framework (defaultMain, testGroup)

#if MIN_VERSION_base(4,5,0)
import Properties (properties)
#else
-- Don't run any tests on GHC < 7.4, but *do* generate an empty output
-- file that a continuous build system can consume so that it won't
-- crash. Thanks for being so inflexible, Jenkins!
properties = []
#endif

main :: IO ()
main = defaultMain [
         testGroup "properties" properties
       ]
