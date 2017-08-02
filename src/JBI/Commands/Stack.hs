{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Stack
   Description : Stack commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Stack where

import JBI.Commands.Common

import Data.List        (isPrefixOf)
import System.Directory (getCurrentDirectory)
import System.FilePath  (dropTrailingPathSeparator, normalise)

--------------------------------------------------------------------------------

data Stack

instance BuildTool Stack where
  commandName = "stack"

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

  commandTargets = withTaggedF go
    where
      go cmd = maybe [] lines <$> tryRunOutput cmd ["ide", "targets"]
