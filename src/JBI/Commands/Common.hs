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
import Data.Coerce                  (Coercible, coerce)
import Data.String                  (IsString(..))
import Data.Tagged
import Data.Version                 (Version, parseVersion)
import System.Directory             (findExecutable)
import System.Exit                  (ExitCode(ExitSuccess))
import System.Process               (readProcessWithExitCode)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

--------------------------------------------------------------------------------

newtype CommandName = CommandName { nameOfCommand :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString CommandName where
  fromString = CommandName

newtype CommandPath = CommandPath { pathToCommand :: FilePath }
  deriving (Eq, Ord, Show, Read)

instance IsString CommandPath where
  fromString = CommandPath

-- | Strip off type safety, run the function, put type safety back on.
withTaggedF :: (Coercible a a', Coercible b b', Functor f)
               => (a' -> f b') -> Tagged t a -> f (Tagged t b)
withTaggedF f = fmap coerce . f . coerce

class BuildTool bt where
  commandName :: Tagged bt CommandName

  commandVersion :: Tagged bt CommandPath -> IO (Tagged bt (Maybe Version))
  commandVersion = withTaggedF tryFindVersion

commandPath :: Tagged bt CommandName -> IO (Tagged bt (Maybe CommandPath))
commandPath = withTaggedF findExecutable

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
