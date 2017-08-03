{-# LANGUAGE DefaultSignatures, FlexibleContexts, KindSignatures,
             MultiParamTypeClasses #-}

{- |
   Module      : JBI.Commands.Common
   Description : How to handle build tools
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.BuildTool where

import JBI.Commands.Tool
import JBI.Tagged

import Data.String (IsString(..))
import System.Exit (ExitCode)

--------------------------------------------------------------------------------

class (Tool bt) => BuildTool bt where

  -- | Try and determine the root directory for this project.
  commandProjectRoot :: Tagged bt CommandPath -> IO (Maybe (Tagged bt ProjectRoot))

  hasBuildArtifacts :: Tagged bt ProjectRoot -> IO Bool

  -- | Ensure's that 'hasBuildArtifacts' is 'True' afterwards;
  --   i.e. forces this build tool.
  --
  --   The intent for this is \"No build tool is currently being used
  --   (i.e. 'hasBuildArtifacts' is 'False' for all) so start using
  --   the one chosen.\" This will not do the equivalent of @stack
  --   init@ and create project configuration.
  --
  --   Some manual fiddling is allowed after this.
  commandPrepare :: GlobalEnv -> Tagged bt CommandPath -> Tagged bt ProjectRoot
                    -> IO ExitCode

  commandTargets :: Tagged bt CommandPath -> IO [Tagged bt ProjectTarget]

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandBuild :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                  -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandRepl :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                 -> IO ExitCode

  -- | Remove /all/ build artifacts of using this build tool (that is,
  --   afterwards 'hasBuildArtifacts' should return 'False').
  --
  --   Assumes 'commandProjectRoot' is 'Just'.
  commandClean :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandTest :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandBench :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

--------------------------------------------------------------------------------

data Command = Command
  { name      :: !String
  , installed :: !(Maybe Installed)
  } deriving (Eq, Ord, Show, Read)

-- | Empty for now, but denote non-specific configurations.
data GlobalEnv = GlobalEnv

newtype ProjectRoot = ProjectRoot { rootPath :: FilePath }
  deriving (Eq, Ord, Show, Read)

instance IsString ProjectRoot where
  fromString = ProjectRoot

-- | TODO: determine if this is a library, executable, test or benchmark component.
newtype ProjectTarget = ProjectTarget { target :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString ProjectTarget where
  fromString = ProjectTarget

--------------------------------------------------------------------------------
