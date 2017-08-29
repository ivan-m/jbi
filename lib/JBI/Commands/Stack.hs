{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Stack
   Description : Stack commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Stack (Stack) where

import JBI.Commands.BuildTool
import JBI.Commands.Tool
import JBI.Environment
import JBI.Tagged

import Data.Maybe       (maybeToList)
import System.Directory (doesDirectoryExist)
import System.Exit      (ExitCode)
import System.FilePath  ((</>))

--------------------------------------------------------------------------------

data Stack

instance Tool Stack where
  commandName = "stack"

instance BuildTool Stack where

  commandProjectRoot = withTaggedF go
    where
      go :: FilePath -> IO (Maybe FilePath)
      go _ = recurseUpFindFile (== "stack.yaml")

  hasBuildArtifacts dir = doesDirectoryExist (stripTag dir </> ".stack-work")

  commandPrepare env cmd = commandArg "setup" env cmd
                           .&&. commandArgs ["build", "--dry-run"] env cmd

  commandTargets = withTaggedF go
    where
      go cmd = maybe [] lines <$> tryRunOutput cmd ["ide", "targets"]

  commandBuild = commandArgTarget "build"

  commandRepl = commandArgTarget "ghci"

  commandClean = commandArgs ["clean", "--full"]

  commandTest = commandArg "test"

  commandBench = commandArg "bench"

  commandExec env cmd prog progArgs = commandArgs args env cmd
    where
      args = "exec" : prog : "--" : progArgs

  -- Stack doesn't have a "run" equivalent; see under \"component\"
  -- here:
  -- https://docs.haskellstack.org/en/stable/build_command/#target-syntax
  commandRun env cmd prog progArgs =
    commandBuild env cmd (Just prog)
    .&&. commandExec env cmd (componentName prog) progArgs

  commandUpdate = commandArg "update"

instance NamedTool Stack

commandArgTarget :: String -> GlobalEnv -> Tagged Stack CommandPath
                    -> Maybe (Tagged Stack ProjectTarget) -> IO ExitCode
commandArgTarget arg env cmd mt = commandArgs args env cmd
  where
    args = arg : maybeToList (fmap stripTag mt)

commandArg :: String -> GlobalEnv -> Tagged Stack CommandPath
              -> IO ExitCode
commandArg arg = commandArgs [arg]

commandArgs :: Args -> GlobalEnv -> Tagged Stack CommandPath
               -> IO ExitCode
commandArgs args _env cmd = tryRun cmd args
