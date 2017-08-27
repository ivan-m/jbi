{-# LANGUAGE CPP, OverloadedStrings #-}

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

import Control.Applicative (liftA2, (<*>))
import Control.Exception   (SomeException(SomeException), handle)
import Control.Monad       (filterM)
import Data.Maybe          (isJust)
import Data.Proxy          (Proxy(Proxy))
import System.Directory    (doesDirectoryExist, doesFileExist,
                            getCurrentDirectory, listDirectory)
import System.Exit         (ExitCode)
import System.FilePath     (dropTrailingPathSeparator, isDrive, takeDirectory,
                            takeExtension, (</>))

import qualified Distribution.Package                  as CPkg
import           Distribution.PackageDescription       (GenericPackageDescription,
                                                        condBenchmarks,
                                                        condExecutables,
                                                        condLibrary,
                                                        condTestSuites)
import qualified Distribution.PackageDescription.Parse as CParse
import           Distribution.Verbosity                (silent)

#if MIN_VERSION_Cabal (2,0,0)
import Distribution.Types.UnqualComponentName (UnqualComponentName,
                                               unUnqualComponentName)
#endif

--------------------------------------------------------------------------------

data Cabal mode

instance Tool (Cabal mode) where
  commandName = "cabal"

instance (CabalMode mode) => BuildTool (Cabal mode) where
  canUseCommand = canUseMode

  commandProjectRoot = cabalProjectRoot

  hasBuildArtifacts = hasModeArtifacts

  commandPrepare = cabalPrepare

  commandTargets = cabalTargets

  commandBuild env cmd = cabalTry env cmd . cabalBuild env cmd

  commandRepl env cmd = cabalTry env cmd . cabalRepl env cmd

  commandClean = cabalClean

  commandTest = liftA2 (<*>) cabalTry cabalTest

  commandBench = liftA2 (<*>) cabalTry cabalBench

  commandExec = cabalExec

  commandRun env cmd = (cabalTry env cmd .) . cabalRun env cmd

  commandUpdate = cabalUpdate

cabalTry :: (CabalMode mode) => GlobalEnv -> Tagged (Cabal mode) CommandPath
            -> IO ExitCode -> IO ExitCode
cabalTry env cmd = tryCommand "Command failed, trying to re-configure"
                              (cabalConfigure env cmd)

instance (CabalMode mode) => NamedTool (Cabal mode) where
  prettyName p = "cabal+" ++ modeName (getMode p)

getMode :: proxy (Cabal mode) -> Proxy mode
getMode _ = Proxy

class CabalMode mode where
  modeName :: proxy mode -> String

  canUseMode :: GlobalEnv -> Installed (Cabal mode) -> IO Bool

  cabalProjectRoot :: Tagged (Cabal mode) CommandPath
                      -> IO (Maybe (Tagged (Cabal mode) ProjectRoot))
  cabalProjectRoot = withTaggedF go
    where
      -- Type signature needed to make withTaggedF happy, though we
      -- don't actually use the command itself for this.
      go :: FilePath -> IO (Maybe FilePath)
      go _ = recurseUpFindFile isCabalFile

  hasModeArtifacts :: Tagged (Cabal mode) ProjectRoot -> IO Bool

  cabalPrepare :: GlobalEnv -> Tagged (Cabal mode) CommandPath
                  -> Tagged (Cabal mode) ProjectRoot -> IO ExitCode

  cabalTargets :: Tagged (Cabal mode) CommandPath
                  -> Tagged (Cabal mode) ProjectRoot
                  -> IO [Tagged (Cabal mode) ProjectTarget]
  cabalTargets = const (withTaggedF cabalFileComponents)

  -- | This is an additional function than found in 'BuildTool'.  May
  --   include installing dependencies.
  cabalConfigure :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalBuild :: GlobalEnv -> Tagged (Cabal mode) CommandPath
                  -> Maybe (Tagged (Cabal mode) ProjectTarget) -> IO ExitCode

  cabalRepl :: GlobalEnv -> Tagged (Cabal mode) CommandPath
               -> Maybe (Tagged (Cabal mode) ProjectTarget) -> IO ExitCode

  cabalClean :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalTest :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalBench :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalExec :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> String -> Args -> IO ExitCode

  cabalRun :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> Tagged (Cabal mode) ProjectTarget
              -> Args -> IO ExitCode

  cabalUpdate :: GlobalEnv -> Tagged (Cabal mode) CommandPath -> IO ExitCode

-- | Not made part of default call in case cabal-new does something
--   different.
hasCabalDist :: Tagged (Cabal mode) ProjectRoot -> IO Bool
hasCabalDist pr = doesDirectoryExist (stripTag pr </> "dist")

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

isCabalFile :: FilePath -> Bool
isCabalFile = ((== ".cabal") . takeExtension)

--------------------------------------------------------------------------------
-- The Cabal library likes to really keep changing things...

cabalFileComponents :: FilePath -> IO [String]
cabalFileComponents root = do
  cntns <- map (root </>) <$> listDirectory root
  files <- filterM doesFileExist cntns
  let cabalFiles = filter isCabalFile files
  case cabalFiles of
    []    -> return []
    (c:_) -> getComponents <$> parseCabalFile c

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile =
#if MIN_VERSION_Cabal(2,0,0)
  CParse.readGenericPackageDescription
#else
  CParse.readPackageDescription
#endif
    silent

type ComponentName =
#if MIN_VERSION_Cabal (2,0,0)
  UnqualComponentName
#else
  String
#endif

rawComponentName :: ComponentName -> String
rawComponentName =
#if MIN_VERSION_Cabal (2,0,0)
  unUnqualComponentName
#else
  id
#endif

packageName :: GenericPackageDescription -> String
packageName =
#if MIN_VERSION_Cabal (2,0,0)
  CPkg.unPackageName
#else
  (\(CPkg.PackageName nm) -> nm)
#endif
  . CPkg.packageName

getComponents :: GenericPackageDescription -> [String]
getComponents gpd = concat
                      [ getLib
                      , getType condExecutables "exe"
                      , getType condTestSuites  "test"
                      , getType condBenchmarks  "bench"
                      ]
  where
    pkgName = packageName gpd

    getLib
      | isJust (condLibrary gpd) = ["lib:" ++ pkgName]
      | otherwise                = []

    getType f typ = map (\cmp -> typ ++ ':' : rawComponentName (fst cmp)) (f gpd)
