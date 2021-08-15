{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module ExerciseTracker
  ( ActivityLog(..)
  , ActivityRecord(..)
  , ActivityAverage(..)
  , ActivityMap
  , ParseError(..)
  , addLog
  , addLogJson
  , emptyActivityMap
  , readStats
  , readStatsJson
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.STM
import           Data.Aeson                   (eitherDecodeStrict', encode)
import           Data.Aeson.Types
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import           Data.Word

data ActivityLog = ActivityLog
  { activityCount    :: !Word64
  , activityDuration :: !Double
  } deriving (Eq, Ord, Show)

emptyActivityLog :: ActivityLog
emptyActivityLog = ActivityLog {activityCount = 0, activityDuration = 0}

data ActivityRecord = ActivityRecord
  { activityName     :: !Text
  , activityDuration :: !Double
  } deriving (Eq, Ord, Show)

instance FromJSON ActivityRecord where
  parseJSON =
    withObject "ActivityRecord" $ \obj ->
      ActivityRecord <$> obj .: "action" <*> obj .: "time"

data ActivityAverage = ActivityAverage
  { activityName    :: !Text
  , activityAverage :: !Double
  } deriving (Eq, Ord, Show)

data ParseError = ParseError String
  deriving (Eq, Ord, Show)

instance Exception ParseError

instance ToJSON ActivityAverage where
  toJSON ActivityAverage {..} =
    object ["action" .= activityName, "avg" .= activityAverage]
  toEncoding ActivityAverage {..} =
    pairs ("action" .= activityName <> "avg" .= activityAverage)

type ActivityMap = Map Text (TVar ActivityLog)

addLogJson :: ByteString -> TVar ActivityMap -> IO ()
addLogJson str activityMapVar =
  case eitherDecodeStrict' @ActivityRecord str of
    Left errMsg -> throwIO $ ParseError errMsg
    Right record -> addLog record activityMapVar

addLog :: ActivityRecord -> TVar ActivityMap -> IO ()
addLog ActivityRecord {activityName, activityDuration = newDuration} activityMapVar = do
  activityMap <- atomically ensureKey
  atomically $ incrementKey activityMap
  where
    ensureKey :: STM ActivityMap
    ensureKey = do
      curMap <- readTVar activityMapVar
      if activityName `Map.member` curMap
        then return curMap
        else do
          newVal <- newTVar emptyActivityLog
          let curMap' = Map.insert activityName newVal curMap
          writeTVar activityMapVar curMap'
          return curMap'
    incrementKey :: ActivityMap -> STM ()
    incrementKey activityMap =
      case Map.lookup activityName activityMap of
        Nothing -> return ()
        Just activityLogVar -> do
          ActivityLog {activityCount, activityDuration = oldDuration} <-
            readTVar activityLogVar
          writeTVar
            activityLogVar
            ActivityLog
              { activityCount = activityCount + 1
              , activityDuration = oldDuration + newDuration
              }

readStatsJson :: TVar ActivityMap -> IO ByteString
readStatsJson = fmap (toStrict . encode) . readStats

readStats :: TVar ActivityMap -> IO [ActivityAverage]
readStats mapVar = do
  curMap <- readTVarIO mapVar
  mapM (uncurry getAverage) (Map.toList curMap)
  where
    getAverage :: Text -> TVar ActivityLog -> IO ActivityAverage
    getAverage key logVar = do
      ActivityLog {..} <- readTVarIO logVar
      return
        ActivityAverage
          { activityName = key
          , activityAverage = activityDuration / fromIntegral activityCount
          }

emptyActivityMap :: IO (TVar ActivityMap)
emptyActivityMap = newTVarIO Map.empty
