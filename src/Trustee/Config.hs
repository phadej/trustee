module Trustee.Config where

data Config = Config
    { cfgThreads :: !Int
    , cfgGhcJobs :: !Int
    }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cfgThreads = 3
    , cfgGhcJobs = 1
    }
