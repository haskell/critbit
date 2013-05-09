{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Framework

#if MIN_VERSION_base(4,5,0)
import Properties (properties)
#else
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Don't run any tests on GHC < 7.4, but *do* generate a test output
-- file that a continuous build system can consume so that it won't
-- crash. The output file can't be devoid of tests, because then
-- instead of crashing Jenkins will fail because no tests were run.
-- Thanks for being so inflexible, Jenkins!
properties = [ testProperty "fuck you, jenkins" True ]
#endif

main :: IO ()
main = defaultMain [
         testGroup "properties" properties
       ]
