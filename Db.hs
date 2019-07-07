{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db
  ( -- * Basic types
    TestEnv(..)
  , TestName(..)
  , BranchName(..)
  , CommitSha(..)
    -- * Fetch metric results
  , getCommitResults
  , Result(..)
    -- * Fetching test environments
  , getTestEnvs
    -- * Fetching commits
  , Commit(..)
  , fetchCommitsWithPrefix
  ) where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text (Text)
import           Data.Aeson hiding (Result)
import           Control.Monad.IO.Class
import           Reflex.Dom

newtype TestEnv = TestEnv { getTestEnv :: Int }
                deriving (Eq, Ord, Show, Read, FromJSON, ToJSON)
newtype TestName = TestName { getTestName :: Text }
                 deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype BranchName = BranchName { getBranchName :: Text }
                   deriving (Eq, Ord, Show, FromJSON, ToJSON)
newtype CommitSha = CommitSha { getCommitSha :: Text }
                  deriving (Eq, Ord, Show, FromJSON, ToJSON)

type C t m = (MonadIO m, MonadIO (Performable m), PerformEvent t m, HasJSContext (Performable m), TriggerEvent t m)

data Result = Result { testName :: !TestName
                     , resultValue :: !Double
                     }
            deriving (Eq, Ord, Show)

instance FromJSON Result where
  parseJSON = withObject "result" $ \o ->
    Result <$> o .: "test_name"
           <*> o .: "result_value"

rootUrl :: Text
rootUrl = "http://home.smart-cactus.org:8889"

getCommitResults :: (Reflex t, C t m)
                 => Event t (CommitSha, TestEnv)
                 -> m (Event t (Maybe [Result]))
getCommitResults = getAndDecode . fmap reqUrl
  where
    reqUrl (sha, testEnv) =
        rootUrl <> "/results_view"
        <> "?commit_sha=eq." <> getCommitSha sha
        <> "&test_env_id=eq." <> T.pack (show $ getTestEnv testEnv)
        <> "&limit=100"

getTestEnvs :: (Reflex t, C t m)
            => Event t ()
            -> m (Event t (Maybe (M.Map TestEnv Text)))
getTestEnvs ev = do
    fmap (fmap (M.fromList . map f)) <$> getAndDecode (req <$ ev)
  where
    f (TestEnvRow a b) = (a,b)
    req = rootUrl <> "/test_envs"

data TestEnvRow = TestEnvRow TestEnv Text

instance FromJSON TestEnvRow where
  parseJSON = withObject "test environment" $ \o ->
    TestEnvRow <$> o .: "test_env_id"
               <*> o .: "test_env_name"

data Commit = Commit { commitSha   :: CommitSha
                     , commitTitle :: Maybe Text
                     , commitDate  :: Maybe Text
                     , commitResultsCount :: Int
                     }
            deriving (Eq, Show)

instance FromJSON Commit where
  parseJSON = withObject "commit" $ \o ->
    Commit <$> o .: "commit_sha"
           <*> o .: "commit_title"
           <*> o .: "commit_date"
           <*> o .: "result_count"

fetchCommitsWithPrefix :: (Reflex t, C t m)
                       => Event t (TestEnv, Text)
                       -> m (Event t (Maybe [Commit]))
fetchCommitsWithPrefix = getAndDecode . fmap reqUrl
  where
    reqUrl (env, prefix) =
      rootUrl <> "/commit_metric_counts"
        <> "?test_env_id=eq." <> T.pack (show $ getTestEnv env) <> "&"
        <> "commit_sha=like." <> prefix <> "*"
