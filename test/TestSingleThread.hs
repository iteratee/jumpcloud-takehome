{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeApplications         #-}

module TestSingleThread
  ( suite
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception           (AssertionFailed (..), throwIO)
import           Data.Aeson                  (encode, eitherDecodeStrict')
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           ExerciseTracker

import           Test.HUnitPlus

testParseRecord :: Test
testParseRecord =
  "TestParseRecord" ~:
  (assertEqual
     "Expected parse of ActivityRecord"
     (Right ActivityRecord {activityName = "jump", activityDuration = 200})
     (eitherDecodeStrict' "{\"action\":\"jump\", \"time\":200}"))

testParseFail :: Test
testParseFail =
  "TestParseFail" ~:
  (assertThrowsExact
     (ParseError "Error in $: key \"time\" not found")
     (do mapVar <- emptyActivityMap
         addLogJson "{\"action\":\"jump\", \"tim\":200}" mapVar))

testIncrementsCountSingle :: TVar ActivityMap -> Text -> IO ()
testIncrementsCountSingle activityMapVar key = do
  curMap <- readTVarIO activityMapVar
  oldCount <-
    case Map.lookup key curMap of
      Nothing     -> return 0
      Just logVar -> fmap activityCount (readTVarIO logVar)
  addLog
    ActivityRecord {activityName = key, activityDuration = 10.0}
    activityMapVar
  newMap <- readTVarIO activityMapVar
  newCount <-
    case Map.lookup key newMap of
      Nothing ->
        throwIO (AssertionFailed "Error, key should be in the new map.")
      Just logVar -> fmap activityCount (readTVarIO logVar)
  assertEqual "Expected that count is incremented by 1" (oldCount + 1) newCount

testIncrementsCount :: Test
testIncrementsCount =
  "TestIncrementsCount" ~:
  (do mapVar <- emptyActivityMap
      let keys = take 20 $ cycle ["a", "b", "c", "d", "e"]
      mapM_ (\key -> testIncrementsCountSingle mapVar key) keys)

testIncrementsDurationSingle :: TVar ActivityMap -> Text -> Double -> IO ()
testIncrementsDurationSingle activityMapVar key duration = do
  curMap <- readTVarIO activityMapVar
  oldDuration <-
    case Map.lookup key curMap of
      Nothing -> return 0
      Just logVar ->
        fmap (\ActivityLog {..} -> activityDuration) (readTVarIO logVar)
  addLog
    ActivityRecord {activityName = key, activityDuration = duration}
    activityMapVar
  newMap <- readTVarIO activityMapVar
  newDuration <-
    case Map.lookup key newMap of
      Nothing ->
        throwIO (AssertionFailed "Error, key should be in the new map.")
      Just logVar ->
        fmap (\ActivityLog {..} -> activityDuration) (readTVarIO logVar)
  assertEqual
    "Expected that newDuration = oldDuration + duration"
    (oldDuration + duration)
    newDuration

testIncrementsDuration :: Test
testIncrementsDuration =
  "TestIncrementsDuration" ~:
  (do mapVar <- emptyActivityMap
      let pairs =
            [ (key, duration)
            | key <- ["a", "b", "c", "d", "e"]
            , duration <- [10, 20, 30, 40, 50]
            ]
      mapM_
        (\(key, duration) -> testIncrementsDurationSingle mapVar key duration)
        pairs)

testSerialization :: Test
testSerialization =
  "TestSerialization" ~:
  (assertEqual
    "Should serialize to example JSON"
    "{\"action\":\"jump\",\"avg\":150.0}"
    (toStrict $ encode $ ActivityAverage { activityName = "jump", activityAverage = 150 }))

suite :: TestSuite
suite =
  testSuite
    "Single Thread Test Suite"
    [ testParseRecord
    , testParseFail
    , testIncrementsCount
    , testIncrementsDuration
    , testSerialization
    ]
