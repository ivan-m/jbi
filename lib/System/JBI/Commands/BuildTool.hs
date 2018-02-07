{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts,
             MultiParamTypeClasses #-}

{- |
   Module      : System.JBI.Commands.Common
   Description : How to handle build tools
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Commands.BuildTool where

import System.JBI.Commands.Tool
import System.JBI.Environment
import System.JBI.Tagged

import Control.Applicative (liftA2)
import Control.Exception   (SomeException(SomeException), handle)
import Control.Monad       (filterM, forM)
import Data.Aeson          (ToJSON(toJSON))
import Data.List           (span)
import Data.Maybe          (isJust)
import Data.String         (IsString(..))
import GHC.Generics        (Generic)
import System.Directory    (doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit         (ExitCode)
import System.FilePath     (dropTrailingPathSeparator, isDrive, takeDirectory,
                            (</>))

--------------------------------------------------------------------------------

class (Tool bt) => BuildTool bt where

  -- | Make sure there's nothing in the environment preventing us from
  --   using this tool.
  --
  --   For example, a minimum version, need another tool installed, etc.
  --
  --   @since 0.2.0.0
  canUseCommand :: Env -> Tagged bt CommandPath -> IO Bool
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
  commandPrepare :: Env -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandTargets :: Config -> Tagged bt CommandPath -> IO [Tagged bt ProjectTarget]

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandBuild :: Env -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                  -> IO ExitCode

  -- | Launch a @ghci@ session within the current project.
  --
  --   Takes a list of interpreter arguments.
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandRepl :: Env -> Tagged bt CommandPath -> Tagged bt Args
                 -> Maybe (Tagged bt ProjectTarget) -> IO ExitCode

  -- | Remove /all/ build artifacts of using this build tool (that is,
  --   afterwards 'hasBuildArtifacts' should return 'False').
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandClean :: Env -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandTest :: Env -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandBench :: Env -> Tagged bt CommandPath -> IO ExitCode

  -- | Run an external command within this environment.
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandExec :: Env -> Tagged bt CommandPath -> String -> Args -> IO ExitCode

  -- | Run an executable component within this environment (building
  --   it first if required).
  --
  --   Assumes 'canUseBuildTool'.  Should be run within 'ProjectRoot'.
  commandRun :: Env -> Tagged bt CommandPath -> Tagged bt ProjectTarget
                -> Args -> IO ExitCode

  -- | Update index of available packages.
  commandUpdate :: Env -> Tagged bt CommandPath -> IO ExitCode

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
  } deriving (Eq, Show, Read, Generic, ToJSON)

commandToolInformation :: (NamedTool bt) => Env -> proxy bt
                          -> IO (ToolInformation bt)
commandToolInformation env pr =
  ToolInformation (prettyName pr) <$> commandBuildUsage env

data BuildUsage bt = BuildUsage
  { installation :: !(Installed bt)
  , usable       :: !Bool
  , project      :: !(Maybe (BuildProject bt))
  } deriving (Eq, Show, Read, Generic, ToJSON)

data BuildProject bt = BuildProject
  { projectRoot      :: !(Tagged bt ProjectRoot)
  , artifactsPresent :: !Bool
  } deriving (Eq, Show, Read, Generic, ToJSON)

-- | A 'Nothing' indicates that this tool cannot be used for this
--   project (i.e. needs configuration).
commandBuildUsage :: (BuildTool bt) => Env
                     -> IO (Maybe (BuildUsage bt))
commandBuildUsage env = do
  mInst <- commandInformation (envConfig env)
  forM mInst $ \inst ->
    BuildUsage inst <$> canUseCommand env (path inst)
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

instance ToJSON ProjectRoot where
  toJSON = toJSON . rootPath

-- | TODO: determine if this is a library, executable, test or benchmark component.
newtype ProjectTarget = ProjectTarget { projectTarget :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString ProjectTarget where
  fromString = ProjectTarget

componentName :: Tagged bt ProjectTarget -> String
componentName = safeLast . splitOn ':' . stripTag

safeLast :: [[a]] -> [a]
safeLast []  = []
safeLast ass = last ass

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep = go
  where
    go [] = []
    go as = case span (/= sep) as of
              (seg, [])    -> seg : []
              (seg, _:as') -> seg : go as'

--------------------------------------------------------------------------------

-- | If an exception occurs, return 'Nothing'
tryIO :: IO (Maybe a) -> IO (Maybe a)
tryIO = handle (\SomeException{} -> return Nothing)

-- | Recurse up until you find a directory containing a file that
--   matches the predicate, returning that directory.
recurseUpFindFile :: (FilePath -> Bool) -> IO (Maybe FilePath)
recurseUpFindFile p = tryIO $ go . dropTrailingPathSeparator =<< getCurrentDirectory
  where
    go dir = do cntns  <- listDirectory dir
                files <- filterM (doesFileExist . (dir </>)) cntns
                if any p files
                   then return (Just dir)
                   -- We do the base case check here so we can
                   -- actually check the top level directory.
                   else if isDrive dir
                           then return Nothing
                           else go (takeDirectory dir)
