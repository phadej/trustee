{-# LANGUAGE OverloadedStrings #-}
module Trustee.Config where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.!=), (.:?))
import Peura
import Prelude (userError)

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

readConfig :: Peu r Config
readConfig = handle withDefaultConfig $ do
    let cfgFile' :: Path XdgConfig
        cfgFile' = root </> fromUnrootedFilePath "trustee" </> fromUnrootedFilePath "config"
    cfgFile <- makeAbsolute (FsPath cfgFile')
    contents <- readLazyByteString cfgFile
    either (throwM . userError) return $ eitherDecode contents
  where
    withDefaultConfig :: IOException -> Peu r Config
    withDefaultConfig _ = return defaultConfig
