{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

{- |
   Module      : JBI.Commands.Common
   Description : How to handle build tools
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.BuildTool where

import JBI.Commands.Tool
import JBI.Environment
import JBI.Tagged

import Control.Applicative (liftA2)
import Control.Monad       (forM)
import Data.Maybe          (isJust)
import Data.String         (IsString(..))
import System.Exit         (ExitCode)

--------------------------------------------------------------------------------

class (Tool bt) => BuildTool bt where

  -- | Make sure there's nothing in the environment preventing us from
  --   using this tool.
  --
  --   For example, a minimum version, need another tool installed, etc.
  canUseCommand :: GlobalEnv -> Installed bt -> IO Bool
  canUseCommand _ _ = return True

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
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandPrepare :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandTargets :: Tagged bt CommandPath -> IO [Tagged bt ProjectTarget]

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandBuild :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                  -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandRepl :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                 -> IO ExitCode

  -- | Remove /all/ build artifacts of using this build tool (that is,
  --   afterwards 'hasBuildArtifacts' should return 'False').
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandClean :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandTest :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandBench :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Run an external command within this environment.
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandExec :: GlobalEnv -> Tagged bt CommandPath -> String -> Args -> IO ExitCode

  -- | Run an executable component within this environment (building
  --   it first if required).
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandRun :: GlobalEnv -> Tagged bt CommandPath -> Tagged bt ProjectTarget
                -> Args -> IO ExitCode

  -- | Update index of available packages.
  commandUpdate :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

-- | This class exists because of:
--
--   a) Distinguish the different Cabal variants
--
--   b) Be able to use a wrapper GADT that takes a @proxy bt@ and can
--      be an instance of 'BuildTool' but not this.
class (BuildTool bt) => NamedTool bt where
  prettyName :: proxy bt -> String
  prettyName = nameOfCommand . proxy commandName

data ToolInformation bt = ToolInformation
  { tool        :: !String
  , information :: !(Maybe (BuildUsage bt))
  } deriving (Eq, Show, Read)

commandToolInformation :: (NamedTool bt)
                          => GlobalEnv -> proxy bt
                          -> IO (ToolInformation bt)
commandToolInformation env pr =
  ToolInformation (prettyName pr) <$> commandBuildUsage env

data BuildUsage bt = BuildUsage
  { installation :: !(Installed bt)
  , usable       :: !Bool
  , project      :: !(Maybe (BuildProject bt))
  } deriving (Eq, Show, Read)

data BuildProject bt = BuildProject
  { projectRoot      :: !(Tagged bt ProjectRoot)
  , artifactsPresent :: !Bool
  } deriving (Eq, Show, Read)

-- | A 'Nothing' indicates that this tool cannot be used for this
--   project (i.e. needs configuration).
commandBuildUsage :: (BuildTool bt)
                     => GlobalEnv
                     -> IO (Maybe (BuildUsage bt))
commandBuildUsage env = do
  mInst <- commandInformation
  forM mInst $ \inst ->
    BuildUsage inst <$> canUseCommand env inst
                    <*> commandBuildProject (path inst)


commandBuildProject :: (BuildTool bt) => Tagged bt CommandPath
                       -> IO (Maybe (BuildProject bt))
commandBuildProject cmd = do
  mroot <- commandProjectRoot cmd
  forM mroot $ \root ->
    BuildProject root <$> hasBuildArtifacts root

canUseBuildTool :: Maybe (BuildUsage bt) -> Bool
canUseBuildTool = maybe False (liftA2 (&&) usable (isJust . project))

--------------------------------------------------------------------------------

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