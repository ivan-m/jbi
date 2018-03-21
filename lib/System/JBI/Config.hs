{- |
   Module      : System.JBI.Config
   Description : Run-time configuration settings
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Config where

--------------------------------------------------------------------------------

data Config = Config
  { debugMode  :: !Bool
  , nixChannel :: !(Maybe String)
  } deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { debugMode  = False
  , nixChannel = Nothing
  }
