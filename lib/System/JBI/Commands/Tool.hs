{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

{- |
   Module      : System.JBI.Commands.Tool
   Description : Common tooling commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Commands.Tool where

import System.JBI.Config
import System.JBI.Tagged

import Control.Applicative          (liftA2)
import Control.Monad                (when)
import Data.Aeson                   (ToJSON(toJSON))
import Data.Char                    (isDigit)
import Data.Maybe                   (listToMaybe)
import Data.String                  (IsString(..))
import Data.Version                 (Version, parseVersion)
import GHC.Generics                 (Generic)
import System.Directory             (findExecutable)
import System.Exit                  (ExitCode(ExitSuccess))
import System.IO                    (IOMode(WriteMode), hPutStrLn, stderr,
                                     withFile)
import System.Process               (CreateProcess(..),
                                     StdStream(Inherit, UseHandle), proc,
                                     readProcessWithExitCode,
                                     showCommandForUser, waitForProcess,
                                     withCreateProcess)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

--------------------------------------------------------------------------------

class Tool t where
  commandName :: Tagged t CommandName

  commandVersion :: Config -> Tagged t CommandPath -> IO (Maybe (Tagged t Version))
  commandVersion = withTaggedF . tryFindVersion

commandPath :: (Tool t) => IO (Maybe (Tagged t CommandPath))
commandPath = withTaggedF findExecutable commandName

commandInformation :: (Tool t) => Config -> IO (Maybe (Installed t))
commandInformation cfg = commandPath >>= mapM getVersion
  where
    getVersion :: (Tool t') => Tagged t' CommandPath -> IO (Installed t')
    getVersion tcp = Installed tcp <$> commandVersion cfg tcp

data GHC

instance Tool GHC where
  commandName = "ghc"

--------------------------------------------------------------------------------

newtype CommandName = CommandName { nameOfCommand :: String }
  deriving (Eq, Ord, Show, Read)

instance IsString CommandName where
  fromString = CommandName

newtype CommandPath = CommandPath { pathToCommand :: FilePath }
  deriving (Eq, Ord, Show, Read)

instance ToJSON CommandPath where
  toJSON = toJSON . pathToCommand

instance IsString CommandPath where
  fromString = CommandPath

data Installed t = Installed
  { path    :: !(Tagged t CommandPath)
  , version :: !(Maybe (Tagged t Version))
               -- ^ Try and determine the version.  Only a factor in
               --   case any features are version-specific.
  } deriving (Eq, Ord, Show, Read, Generic, ToJSON)

--------------------------------------------------------------------------------

-- | Attempt to find the version of the provided command, by assuming
--   it's contained in the first line of the output of @command
--   --version@.
tryFindVersion :: Config -> FilePath -> IO (Maybe Version)
tryFindVersion = tryFindVersionBy findVersion
  where
    findVersion str = takeVersion (dropWhile (not . isDigit) str)

-- | If we're at the start of a Version, take all of it.
takeVersion :: String -> String
takeVersion = takeWhile (liftA2 (||) isDigit (=='.'))

tryFindVersionBy :: (String -> String) -> Config -> FilePath -> IO (Maybe Version)
tryFindVersionBy findVersion cfg cmd =
  fmap (>>= parseVer) (tryRunOutput cfg cmd ["--version"])
  where
    parseVer ver = case readP_to_S (parseVersion <* eof) (findVersion ver) of
                     [(v,"")] -> Just v
                     _        -> Nothing

type Args = [String]

-- | Only return the stdout if the process was successful and had no stderr.
tryRunOutput :: Config -> FilePath -> Args -> IO (Maybe String)
tryRunOutput cfg cmd args = do
  printDebug cfg cmd args
  res <- readProcessWithExitCode cmd args ""
  return $ case res of
             (ExitSuccess, out, "" ) -> Just out
             -- Some tools (e.g. Stack) put output to stderr
             (ExitSuccess, "",  err) -> Just err
             _                       -> Nothing

-- | As with 'tryRunOutput' but only return the first line (if any).
tryRunLine :: Config -> FilePath -> Args -> IO (Maybe String)
tryRunLine cfg cmd = fmap (>>= listToMaybe . lines) . tryRunOutput cfg cmd

-- | Returns success of call.
tryRun :: Config -> Tagged t CommandPath -> Args -> IO ExitCode
tryRun cfg cmd args = do
  printDebug cfg cmd' args
  withCreateProcess cp $ \_ _ _ ph ->
    waitForProcess ph
  where
    cmd' = stripTag cmd

    cp = (proc cmd' args) { std_in  = Inherit
                          , std_out = Inherit
                          , std_err = Inherit
                          }

-- | Print the error message if it isn't successful.
tryRunErr :: String -> IO ExitCode -> IO ExitCode
tryRunErr msg act = do
  res <- act
  if res == ExitSuccess
     then return res
     else res <$ hPutStrLn stderr msg

tryRunToFile :: Config -> FilePath -> Tagged t CommandPath -> Args -> IO ExitCode
tryRunToFile cfg file cmd args = do
  printDebug cfg cmd' args
  withFile file WriteMode $ \h ->
    withCreateProcess (cp h) $ \_ _ _ ph ->
      waitForProcess ph
  where
    cmd' = stripTag cmd

    cp h = (proc cmd' args) { std_in  = Inherit
                            , std_out = UseHandle h
                            , std_err = Inherit
                            }

printDebug :: Config -> FilePath -> Args -> IO ()
printDebug cfg cmd args =
  when (debugMode cfg) (hPutStrLn stderr (makeBox ("Running: " ++ cmdStr)))
  where
    cmdStr = showCommandForUser cmd args

(.&&.) :: (Monad m) => m ExitCode -> m ExitCode -> m ExitCode
m1 .&&. m2 = do ec1 <- m1
                case ec1 of
                  ExitSuccess -> m2
                  _           -> return ec1

infixr 3 .&&.

(.||.) :: (Monad m) => m ExitCode -> m ExitCode -> m ExitCode
m1 .||. m2 = do ec1 <- m1
                case ec1 of
                  ExitSuccess -> return ec1
                  _           -> m2

infixr 2 .||.

tryCommand :: String -> IO ExitCode -> IO ExitCode -> IO ExitCode
tryCommand msg tryWith run = run .||. tryAgain
  where
    tryAgain = do
      putStrLn (makeBox msg)
      tryWith .&&. run

makeBox :: String -> String
makeBox msg = unlines [ border
                      , "* " ++ msg ++ " *"
                      , border
                      ]
  where
    msgLen = length msg
    boxLen = msgLen + 4 -- asterisk + space on either side

    border = replicate boxLen '*'

allSuccess :: (Monad m, Foldable t) => t (m ExitCode) -> m ExitCode
allSuccess = foldr (.&&.) (return ExitSuccess)

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM f (b:bs) = f b >>= (\bv -> if bv then allM f bs else return False)
