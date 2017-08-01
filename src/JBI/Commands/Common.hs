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
import Data.String                  (IsString(..))
import Data.Version                 (Version, parseVersion)
import System.Directory             (findExecutable)
import System.Exit                  (ExitCode(ExitSuccess))
import System.Process               (readProcessWithExitCode)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

--------------------------------------------------------------------------------

newtype Proxied t a = Proxied { proxiedValue :: a }
  deriving (Eq, Ord, Show, Read)

instance (IsString a) => IsString (Proxied t a) where
  fromString = Proxied . fromString

newtype CommandName = CommandName { nameOfCommand :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString CommandName where
  fromString = CommandName

newtype CommandPath = CommandPath { pathToCommand :: FilePath }
  deriving (Eq, Ord, Show, Read)

instance IsString CommandPath where
  fromString = CommandPath

class BuildTool bt where
  commandName :: Proxied bt CommandName

  commandVersion :: Proxied bt CommandPath -> IO (Proxied bt (Maybe Version))
  commandVersion = fmap Proxied
                   . tryFindVersion
                   . pathToCommand . proxiedValue

commandPath :: Proxied bt CommandName -> IO (Proxied bt (Maybe CommandPath))
commandPath = fmap (Proxied . fmap CommandPath)
              . findExecutable
              . nameOfCommand . proxiedValue

data Command = Command
  { name      :: !String
  , installed :: !(Maybe Installed)
  } deriving (Eq, Ord, Show, Read)

data Installed = Installed
  { path    :: !FilePath
  , version :: !(Maybe Version)
  } deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

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
