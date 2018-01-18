{-# LANGUAGE OverloadedStrings #-}
module Trustee.Config where

import Control.Exception (handle)
import Data.Aeson.Compat (FromJSON (..), decode, withObject, (.!=), (.:?))
import System.Directory  (XdgDirectory (..), getXdgDirectory)
import System.FilePath   ((</>))

import qualified Data.ByteString.Lazy as LBS

data Config = Config
    { cfgThreads   :: !Int
    , cfgCabalJobs :: !Int
    , cfgGhcJobs   :: !Int
    }
  deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \obj -> Config
        <$> obj .:? "threads"    .!= cfgThreads defaultConfig
        <*> obj .:? "cabal-jobs" .!= cfgCabalJobs defaultConfig
        <*> obj .:? "ghc-jobs"   .!= cfgGhcJobs defaultConfig

defaultConfig :: Config
defaultConfig = Config
    { cfgThreads   = 4
    , cfgCabalJobs = 2
    , cfgGhcJobs   = 1
    }

readConfig :: IO Config
readConfig = handle withDefaultConfig $ do
    cfgDir <- getXdgDirectory XdgConfig "trustee"
    let cfgFile = cfgDir </> "config"
    contents <- LBS.readFile cfgFile
    decode contents
  where
    withDefaultConfig :: IOError -> IO Config
    withDefaultConfig _ = return defaultConfig
