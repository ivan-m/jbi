{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Stack
   Description : Stack commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Stack where

import JBI.Commands.BuildTool
import JBI.Commands.Tool
import JBI.Environment.Global
import JBI.Tagged

import Data.List        (isPrefixOf, span)
import Data.Maybe       (maybeToList)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit      (ExitCode)
import System.FilePath  (dropTrailingPathSeparator, normalise, (</>))

--------------------------------------------------------------------------------

data Stack

instance Tool Stack where
  commandName = "stack"

instance BuildTool Stack where

  commandProjectRoot = withTaggedF go
    where
      go cmd = do mProjRoot <- tryRunLine cmd ["path", "--project-root"]
                  case cleanse <$> mProjRoot of
                    Nothing -> return Nothing
                    Just root -> do
                      currDir <- cleanse <$> getCurrentDirectory
                      return $ if root `isPrefixOf` currDir
                                  then Just root
                                  else Nothing

      -- Just in case; this is a poor-man's version of
      -- canonicalizePath by "knowing" that they're both directories.
      cleanse = normalise . dropTrailingPathSeparator

  hasBuildArtifacts dir = doesDirectoryExist (stripTag dir </> ".stack-work")

  commandPrepare env cmd _pr = commandArgs ["build", "--dry-run"] env cmd

  commandTargets = withTaggedF go
    where
      go cmd = maybe [] lines <$> tryRunOutput cmd ["ide", "targets"]

  commandBuild = commandArgTarget "build"

  commandRepl = commandArgTarget "ghci"

  commandClean = commandArgs ["clean", "--full"]

  commandTest = commandArg "test"

  commandBench = commandArg "bench"

  commandUpdate = commandArg "update"

  commandExec env cmd prog progArgs = commandArgs args env cmd
    where
      args = "exec" : prog : "--" : progArgs

  -- Stack doesn't have a "run" equivalent; see under \"component\"
  -- here:
  -- https://docs.haskellstack.org/en/stable/build_command/#target-syntax
  commandRun env cmd prog progArgs =
    commandBuild env cmd (Just prog)
    .&&. commandExec env cmd (componentName prog) progArgs

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
