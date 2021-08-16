{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

-- | Exercise Tracker
--   This module allows you to track the average duration of exercise
--   activities. The three main things that you need in order to meet the
--   problem definition are 'emptyActivityMap', 'addActionJson' and
--   'getStatsJson'. For the non problem interface, you can you 'addAction'
--   and 'getStats'
--
--
module ExerciseTracker
  ( ActivityLog(..)
  , ActivityRecord(..)
  , ActivityAverage(..)
  , ActivityMap
  , ParseError(..)
  , addAction
  , addActionJson
  , emptyActivityMap
  , getStats
  , getStatsJson
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.STM
import           Data.Aeson                  (eitherDecodeStrict', encode)
import           Data.Aeson.Types
import           Data.ByteString             (ByteString)
import           Data.ByteString.Lazy        (toStrict)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Data.Word

-- | Data to compute an average duration
data ActivityLog = ActivityLog
  { activityCount    :: !Word64
    -- ^ Number of sessions recorded, used to compute the average
  , activityDuration :: !Double
    -- ^ Total duration of all sessions
  } deriving (Eq, Ord, Show)

-- | An empty ActivityLog has a duration of 0 and a count of 0
--   An empty log is temporarily placed in the map before updating
emptyActivityLog :: ActivityLog
emptyActivityLog = ActivityLog {activityCount = 0, activityDuration = 0}

-- | Record of a given activity session
--   Used to update an 'ActivityLog' inside an 'ActivityMap'
data ActivityRecord = ActivityRecord
  { activityName     :: !Text
    -- ^ Name of the activity
  , activityDuration :: !Double
    -- ^ Duration of the single activity session
  } deriving (Eq, Ord, Show)

-- Conversion instance from JSON to 'ActivityRecord'
instance FromJSON ActivityRecord where
  parseJSON =
    withObject "ActivityRecord" $ \obj ->
      ActivityRecord <$> obj .: "action" <*> obj .: "time"

-- | Result record showing the average duration of an activity
data ActivityAverage = ActivityAverage
  { activityName    :: !Text
    -- ^ Name of the activity
  , activityAverage :: !Double
    -- ^ Average duration of all sessions for this activity
  } deriving (Eq, Ord, Show)

-- | Exception thrown when invalid JSON is supplied for an 'ActivityRecord'
data ParseError =
  ParseError String
  deriving (Eq, Ord, Show)

instance Exception ParseError

-- Conversion instance from ActivityAverage to JSON
instance ToJSON ActivityAverage where
  toJSON ActivityAverage {..} =
    object ["action" .= activityName, "avg" .= activityAverage]
  toEncoding ActivityAverage {..} =
    pairs ("action" .= activityName <> "avg" .= activityAverage)

-- | A Map from 'Text' activity names to 'ActivityLog's that can be used to find
--   the current average duration for varioous activities
type ActivityMap = Map Text (TVar ActivityLog)

-- | Perfom JSON parsing from a 'ByteString' and then call 'addAction'.
--   Throws 'ParseError' if the JSON doesn't parse
addActionJson :: ByteString -> TVar ActivityMap -> IO ()
addActionJson str activityMapVar =
  case eitherDecodeStrict' @ActivityRecord str of
    Left errMsg  -> throwIO $ ParseError errMsg
    Right record -> addAction record activityMapVar

-- | Add a single 'ActivityRecord' into a 'TVar' containing an 'ActivityMap'
--   This function is thread safe, but 'getStats' may observe an activity of 0 duration
--   and 0 count
addAction :: ActivityRecord -> TVar ActivityMap -> IO ()
addAction ActivityRecord {activityName, activityDuration = newDuration} activityMapVar = do
  logVar <- atomically ensureKey
  atomically $ incrementLog logVar
    -- Ensure that the key is in the map and return the value there.
    -- If a value is not found, a new TVar is allocated, and the map is updated
    -- before returning
  where
    ensureKey :: STM (TVar ActivityLog)
    ensureKey = do
      curMap <- readTVar activityMapVar
      case Map.lookup activityName curMap of
        Just logVar -> return logVar
        Nothing -> do
          newVar <- newTVar emptyActivityLog
          let curMap' = Map.insert activityName newVar curMap
          writeTVar activityMapVar curMap'
          return newVar
    -- Take the TVar ActivityLog returned by ensureKey and increment it appropriately
    -- These two functions are run in two different transactions to reduce contention.
    -- This means it is possible to observe the empty activity log
    incrementLog :: TVar ActivityLog -> STM ()
    incrementLog logVar = do
      ActivityLog {activityCount, activityDuration = oldDuration} <-
        readTVar logVar
      writeTVar
        logVar
        ActivityLog
          { activityCount = activityCount + 1
          , activityDuration = oldDuration + newDuration
          }

-- | Wrapper for 'getStats' that produces a JSON 'ByteString'
getStatsJson :: TVar ActivityMap -> IO ByteString
getStatsJson = fmap (toStrict . encode) . getStats

-- | Get average duration for all activities
--   The list of activities is read in a single transaction.
--   The value for each activity is read in a single transaction.
--   The result is that while we get a consistent average for each activity,
--   It may not be consistent across activities
getStats :: TVar ActivityMap -> IO [ActivityAverage]
getStats mapVar = do
  curMap <- readTVarIO mapVar
  fmap catMaybes $ mapM (uncurry getAverage) (Map.toList curMap)
  where
    getAverage :: Text -> TVar ActivityLog -> IO (Maybe ActivityAverage)
    getAverage key logVar = do
      ActivityLog {..} <- readTVarIO logVar
      if activityCount == 0
        then return Nothing
        else return $
             Just
               ActivityAverage
                 { activityName = key
                 , activityAverage =
                     activityDuration / fromIntegral activityCount
                 }

-- | Build an empty activity map
--   Call in order to be able to use 'addAction' or 'getStats'
emptyActivityMap :: IO (TVar ActivityMap)
emptyActivityMap = newTVarIO Map.empty
