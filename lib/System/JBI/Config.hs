{- |
   Module      : System.JBI.Config
   Description : Run-time configuration settings
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Config where

--------------------------------------------------------------------------------

newtype Config = Config
  { debugMode :: Bool
  } deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { debugMode = False
  }
