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
import Data.Maybe                   (listToMaybe)
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

newtype ProjectRoot = ProjectRoot { rootPath :: FilePath }
  deriving (Eq, Ord, Show, Read)

instance IsString ProjectRoot where
  fromString = ProjectRoot

-- | Strip off type safety, run the function, put type safety back on.
withTaggedF :: (Coercible a a', Coercible b b', Functor f)
               => (a' -> f (Maybe b')) -> Tagged t a -> f (Maybe (Tagged t b))
withTaggedF f = fmap coerce . f . coerce

tagMaybe :: Tagged t (Maybe a) -> Maybe (Tagged t a)
tagMaybe = coerce

maybeTag :: Maybe (Tagged t a) -> Tagged t (Maybe a)
maybeTag = coerce

class BuildTool bt where
  commandName :: Tagged bt CommandName

  commandVersion :: Tagged bt CommandPath -> IO (Maybe (Tagged bt Version))
  commandVersion = withTaggedF tryFindVersion

  -- | Try and determine the root directory for this project.
  commandProjectRoot :: Tagged bt CommandPath -> IO (Maybe (Tagged bt FilePath))

commandPath :: (BuildTool bt) => IO (Maybe (Tagged bt CommandPath))
commandPath = withTaggedF findExecutable commandName

commandInformation :: (BuildTool bt) => IO (Maybe (Tagged bt Installed))
commandInformation = commandPath >>= mapM getVersion
  where
    getVersion :: (BuildTool bt') => Tagged bt' CommandPath -> IO (Tagged bt' Installed)
    getVersion tcp = liftA2 Installed tcp  . maybeTag <$> commandVersion tcp

data Command = Command
  { name      :: !String
  , installed :: !(Maybe Installed)
  } deriving (Eq, Ord, Show, Read)

data Installed = Installed
  { path    :: !CommandPath
  , version :: !(Maybe Version)
               -- ^ Try and determine the version.  Only a factor in
               --   case any features are version-specific.
  } deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

-- | Attempt to find the version of the provided command, by assuming
--   it's contained in the first line of the output of @command
--   --version@.
tryFindVersion :: FilePath -> IO (Maybe Version)
tryFindVersion cmd =
  fmap (>>= parseVer) (tryRun cmd ["--version"])
  where
    findVersion str = takeWhile (liftA2 (||) isDigit (=='.')) (dropWhile (not . isDigit) str)

    parseVer ver = case readP_to_S (parseVersion <* eof) (findVersion ver) of
                     [(v,"")] -> Just v
                     _        -> Nothing

-- | Only return the stdout if the process was successful and had no stderr.
tryRun :: FilePath -> [String] -> IO (Maybe String)
tryRun cmd args = do
  res <- readProcessWithExitCode cmd args ""
  return $ case res of
             (ExitSuccess, out, "") -> Just out
             _                      -> Nothing

-- | As with 'tryRun' but only return the first line (if any).
tryRunLine :: FilePath -> [String] -> IO (Maybe String)
tryRunLine cmd = fmap (>>= listToMaybe . lines) . tryRun cmd
