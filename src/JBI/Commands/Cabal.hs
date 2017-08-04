{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Cabal
   Description : cabal-install support
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Cabal where

import JBI.Commands.BuildTool
import JBI.Commands.Tool
import JBI.Environment.Global
import JBI.Tagged

import Control.Exception (SomeException(SomeException), handle)
import Control.Monad     (filterM)
import System.Directory  (doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath   (dropTrailingPathSeparator, isDrive, takeDirectory,
                          takeExtension, (</>))

--------------------------------------------------------------------------------

data Cabal mode

instance Tool (Cabal mode) where
  commandName = "cabal"

class CabalMode mode where
  cabalProjectRoot :: Tagged (Cabal mode) CommandPath
                      -> IO (Maybe (Tagged (Cabal mode) ProjectRoot))
  cabalProjectRoot = withTaggedF go
    where
      -- Type signature needed to make withTaggedF happy, though we
      -- don't actually use the command itself for this.
      go :: FilePath -> IO (Maybe FilePath)
      go _ = recurseUpFindFile ((== ".cabal") . takeExtension)

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
