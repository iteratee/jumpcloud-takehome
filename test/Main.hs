module Main where

import qualified Control.Logging  as Log

import           Test.HUnitPlus
import qualified TestMultiThread
import qualified TestSingleThread

main :: IO ()
main =
  Log.withStderrLogging $ do
    Log.setLogLevel Log.LevelWarn
    createMain [TestSingleThread.suite, TestMultiThread.suite]
