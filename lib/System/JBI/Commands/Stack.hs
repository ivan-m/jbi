{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : System.JBI.Commands.Stack
   Description : Stack commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Commands.Stack (Stack) where

import System.JBI.Commands.BuildTool
import System.JBI.Commands.Nix       (nixShell)
import System.JBI.Commands.Tool
import System.JBI.Environment
import System.JBI.Tagged

import Control.Applicative (liftA2)
import Data.Char           (isSpace)
import Data.Maybe          (isJust, maybeToList)
import System.Directory    (doesDirectoryExist)
import System.Exit         (ExitCode)
import System.FilePath     ((</>))

--------------------------------------------------------------------------------

data Stack

instance Tool Stack where
  commandName = "stack"

instance BuildTool Stack where

  commandProjectRoot = withTaggedF go
    where
      go :: FilePath -> IO (Maybe FilePath)
      go _ = recurseUpFindFile (== stackYaml)

  hasBuildArtifacts dir = doesDirectoryExist (stripTag dir </> ".stack-work")

  commandPrepare env cmd = commandArg "setup" env cmd
                           .&&. commandArgs ["build", "--dry-run"] env cmd

  commandTargets = withTaggedF go
    where
      go cmd = maybe [] validTargets <$> tryRunOutput cmd ["ide", "targets"]

      validTargets = filter isTarget . lines

      isTarget = liftA2 (&&) (not . null) (all (not . isSpace))

  commandBuild = commandArgsTarget ["build", "--test", "--no-run-tests", "--bench", "--no-run-benchmarks"]

  commandRepl env cmd rargs = commandArgsTarget stackArgs env cmd
    where
      stackArgs = [ "ghci"
                  , "--ghci-options"
                  , ghcArgs
                  , "--test"
                  , "--bench"
                  , "--no-load"
                  ]

      ghcArgs = unwords (stripTag rargs :: Args)

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

stackYaml :: String
stackYaml = "stack.yaml"

commandArgsTarget :: Args -> ToolEnv -> Tagged Stack CommandPath
                     -> Maybe (Tagged Stack ProjectTarget) -> IO ExitCode
commandArgsTarget args env cmd mt = commandArgs args' env cmd
  where
    args' = args ++ maybeToList (fmap stripTag mt)

commandArg :: String -> ToolEnv -> Tagged Stack CommandPath
              -> IO ExitCode
commandArg arg = commandArgs [arg]

commandArgs :: Args -> ToolEnv -> Tagged Stack CommandPath
               -> IO ExitCode
commandArgs args env cmd = tryRun cmd args'
  where
    hasNix = isJust (nixShell (nix env))

    -- Take advantage of the fact that we're running in the same
    -- directory as the stack.yaml file so that stack doesn't have to
    -- bother looking for it.
    args' = addNix (["--stack-yaml", stackYaml] ++ args)

    addNix
      | hasNix    = ("--nix":)
      | otherwise = id
