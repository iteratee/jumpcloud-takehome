module Main where

import           Test.HUnitPlus
import qualified TestMultiThread
import qualified TestSingleThread

main :: IO ()
main = createMain [TestSingleThread.suite, TestMultiThread.suite]
