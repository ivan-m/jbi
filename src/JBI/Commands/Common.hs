{- |
   Module      : JBI.Commands.Common
   Description : Common datatypes for commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Common where

import Control.Applicative          (liftA2)
import Data.Char                    (isDigit)
import Data.Version                 (Version, parseVersion)
import System.Directory             (findExecutable)
import System.Exit                  (ExitCode(ExitSuccess))
import System.Process               (readProcessWithExitCode)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

--------------------------------------------------------------------------------

data Command = Command
  { name      :: !String
  , installed :: !(Maybe Installed)
  } deriving (Eq, Ord, Show, Read)

data Installed = Installed
  { path    :: !FilePath
  , version :: !(Maybe Version)
  } deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

-- | Create a 'Command' by assuming the version is easily obtainable.
defaultCommand :: String -> IO Command
defaultCommand cmd = Command cmd <$> defaultInstalled cmd

defaultInstalled :: String -> IO (Maybe Installed)
defaultInstalled cmd = do
  mPth <- findExecutable cmd
  case mPth of
    Nothing  -> return Nothing
    Just pth -> Just . Installed pth <$> tryFindVersion pth

-- | Attempt to find the version of the provided command, by assuming
--   it's contained in the first line of the output of @command
--   --version@.
tryFindVersion :: FilePath -> IO (Maybe Version)
tryFindVersion cmd = do
  vOut <- readProcessWithExitCode cmd ["--version"] ""
  case vOut of
    (ExitSuccess, ver, "") ->
      case readP_to_S (parseVersion <* eof) (findVersion ver) of
        [(v,"")] -> return (Just v)
        _        -> return Nothing
    _            -> return Nothing
  where
    findVersion str = takeWhile (liftA2 (||) isDigit (=='.')) (dropWhile (not . isDigit) str)
