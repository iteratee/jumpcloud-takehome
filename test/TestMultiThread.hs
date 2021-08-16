{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}

module TestMultiThread
  ( suite
  ) where

import           Control.Concurrent          (myThreadId, threadCapability)
import           Control.Concurrent.Async    (forConcurrently_)
import           Control.Concurrent.STM.TVar
import qualified Control.Logging             as Log
import           Control.Monad               (forM_)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as Text (toStrict)
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Text.Lazy.Builder.Int  as Builder (decimal)
import           ExerciseTracker
import           System.Random.Shuffle

import           Test.HUnitPlus

testMultiThread :: Test
testMultiThread =
  "testMultiThread" ~:
  (do mapVar <- emptyActivityMap
      let keys = map Text.singleton ['A' .. 'z']
          pairs = [(key, duration) | key <- keys, duration <- [10,20 .. 1000]]
      shufPairs <- shuffleM pairs
      forConcurrently_
        shufPairs
        (\(key, duration) -> do
           (cap, _) <- myThreadId >>= threadCapability
           let msgBuilder =
                 Builder.fromText "Running on cap: " <> Builder.decimal cap
           Log.debug (Text.toStrict $ Builder.toLazyText $ msgBuilder)
           addAction
             ActivityRecord {activityName = key, activityDuration = duration}
             mapVar)
      curMap <- readTVarIO mapVar
      forM_
        keys
        (\key ->
           case Map.lookup key curMap of
             Nothing -> assertFailure "Error, key should be in the final map."
             Just logVar -> do
               ActivityLog {..} <- readTVarIO logVar
               assertEqual "Count should be 100" 100 activityCount
               assertEqual "Duration should be 50500" 50500 activityDuration))

suite :: TestSuite
suite = testSuite "Multi Thread Test Suite" [testMultiThread]
