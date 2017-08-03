{-# LANGUAGE DefaultSignatures, FlexibleContexts, KindSignatures,
             MultiParamTypeClasses #-}

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
import System.Process               (CreateProcess(..), StdStream(Inherit),
                                     proc, readProcessWithExitCode,
                                     waitForProcess, withCreateProcess)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

--------------------------------------------------------------------------------

class BuildTool bt where
  commandName :: Tagged bt CommandName

  commandVersion :: Tagged bt CommandPath -> IO (Maybe (Tagged bt Version))
  commandVersion = withTaggedF tryFindVersion

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
  commandPrepare :: GlobalEnv -> Tagged bt CommandPath -> Tagged bt ProjectRoot
                    -> IO ExitCode

  commandTargets :: Tagged bt CommandPath -> IO [Tagged bt ProjectTarget]

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandBuild :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                  -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandRepl :: GlobalEnv -> Tagged bt CommandPath -> Maybe (Tagged bt ProjectTarget)
                 -> IO ExitCode

  -- | Remove /all/ build artifacts of using this build tool (that is,
  --   afterwards 'hasBuildArtifacts' should return 'False').
  --
  --   Assumes 'commandProjectRoot' is 'Just'.
  commandClean :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandTest :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

  -- | Assumes 'commandProjectRoot' is 'Just'.
  commandBench :: GlobalEnv -> Tagged bt CommandPath -> IO ExitCode

commandPath :: (BuildTool bt) => IO (Maybe (Tagged bt CommandPath))
commandPath = withTaggedF findExecutable commandName

commandInformation :: (BuildTool bt) => IO (Maybe (Tagged bt Installed))
commandInformation = commandPath >>= mapM getVersion
  where
    getVersion :: (BuildTool bt') => Tagged bt' CommandPath -> IO (Tagged bt' Installed)
    getVersion tcp = liftA2 Installed tcp  . tagOuter <$> commandVersion tcp

--------------------------------------------------------------------------------

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

-- | Empty for now, but denote non-specific configurations.
data GlobalEnv = GlobalEnv

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

-- | TODO: determine if this is a library, executable, test or benchmark component.
newtype ProjectTarget = ProjectTarget { target :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString ProjectTarget where
  fromString = ProjectTarget

--------------------------------------------------------------------------------
-- Tagged support

class WithTagged (g :: * -> *) where

  -- | Strip off type safety, run the function, put type safety back on.
  withTaggedF :: (Coercible a a', Coercible b b', Functor f)
                 => (a' -> f (g b')) -> Tagged t a -> f (g (Tagged t b))
  default withTaggedF :: (Coercible a a', Coercible b b', Functor f
                         , Coercible (g b') (g (Tagged t b)))
                         => (a' -> f (g b')) -> Tagged t a -> f (g (Tagged t b))
  withTaggedF f = fmap coerce . f . coerce

  tagInner :: Tagged t (g a) -> g (Tagged t a)
  default tagInner :: (Coercible (Tagged t (g a)) (g (Tagged t a)))
                      => Tagged t (g a) -> g (Tagged t a)
  tagInner = coerce

  tagOuter :: g (Tagged t a) -> Tagged t (g a)
  default tagOuter :: (Coercible (g (Tagged t a)) (Tagged t (g a)))
                      => g (Tagged t a) -> Tagged t (g a)
  tagOuter = coerce

instance WithTagged Maybe
instance WithTagged []

-- | Remove the tag along with (potentially) any newtype wrappers
--   added on.
stripTag :: (Coercible a a') => Tagged t a -> a'
stripTag = coerce

--------------------------------------------------------------------------------

-- | Attempt to find the version of the provided command, by assuming
--   it's contained in the first line of the output of @command
--   --version@.
tryFindVersion :: FilePath -> IO (Maybe Version)
tryFindVersion cmd =
  fmap (>>= parseVer) (tryRunOutput cmd ["--version"])
  where
    findVersion str = takeWhile (liftA2 (||) isDigit (=='.')) (dropWhile (not . isDigit) str)

    parseVer ver = case readP_to_S (parseVersion <* eof) (findVersion ver) of
                     [(v,"")] -> Just v
                     _        -> Nothing

type Args = [String]

-- | Only return the stdout if the process was successful and had no stderr.
tryRunOutput :: FilePath -> Args -> IO (Maybe String)
tryRunOutput cmd args = do
  res <- readProcessWithExitCode cmd args ""
  return $ case res of
             (ExitSuccess, out, "") -> Just out
             _                      -> Nothing

-- | As with 'tryRunOutput' but only return the first line (if any).
tryRunLine :: FilePath -> Args -> IO (Maybe String)
tryRunLine cmd = fmap (>>= listToMaybe . lines) . tryRunOutput cmd

-- | Returns success of call.
tryRun :: FilePath -> Args -> IO ExitCode
tryRun cmd args = withCreateProcess cp $ \_ _ _ ph ->
                    waitForProcess ph
  where
    cp = (proc cmd args) { std_in  = Inherit
                         , std_out = Inherit
                         , std_err = Inherit
                         }

-- | Equivalent to chaining all the calls with @&&@ in bash, etc.
--
--   Argument order to make it easier to feed it into a 'Tagged'-based
--   pipeline.
tryRunAll :: [Args] -> FilePath -> IO ExitCode
tryRunAll argss cmd = allSuccess $ map (tryRun cmd) argss

(.&&.) :: (Monad m) => m ExitCode -> m ExitCode -> m ExitCode
m1 .&&. m2 = do ec1 <- m1
                case ec1 of
                  ExitSuccess -> m2
                  _           -> return ec1

infixr 3 .&&.

allSuccess :: (Monad m, Foldable t) => t (m ExitCode) -> m ExitCode
allSuccess = foldr (.&&.) (return ExitSuccess)

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM f (b:bs) = f b >>= (\bv -> if bv then allM f bs else return False)
