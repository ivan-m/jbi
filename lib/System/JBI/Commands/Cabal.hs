{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
   Module      : System.JBI.Commands.Cabal
   Description : cabal-install support
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Commands.Cabal
  ( Cabal
  , CabalMode
  , Sandbox
  , Nix
  ) where

import System.JBI.Commands.BuildTool
import System.JBI.Commands.Nix
import System.JBI.Commands.Tool
import System.JBI.Environment
import System.JBI.Tagged

import Control.Applicative (liftA2, (<*>))
import Control.Monad       (filterM)
import Data.Bool           (bool)
import Data.Maybe          (isJust, maybeToList)
import Data.Proxy          (Proxy(Proxy))
import Data.Version        (Version, makeVersion)
import System.Directory    (doesFileExist, getCurrentDirectory, listDirectory,
                            removeFile)
import System.Exit         (ExitCode, die, exitSuccess)
import System.FilePath     (takeExtension, (</>))
import System.IO.Error     (ioError, isDoesNotExistError, tryIOError)

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

  commandRepl env cmd rargs = cabalTry env cmd . cabalRepl env cmd rargs

  commandClean = cabalClean

  commandTest = liftA2 (<*>) cabalTry cabalTest

  commandBench = liftA2 (<*>) cabalTry cabalBench

  commandExec = cabalExec

  commandRun env cmd = (cabalTry env cmd .) . cabalRun env cmd

  commandUpdate = cabalUpdate

cabalTry :: (CabalMode mode) => Env -> Tagged (Cabal mode) CommandPath
            -> IO ExitCode -> IO ExitCode
cabalTry env cmd = tryCommand "Command failed, trying to re-configure"
                              (cabalConfigure env cmd)

instance (CabalMode mode) => NamedTool (Cabal mode) where
  prettyName p = "cabal+" ++ modeName (getMode p)

getMode :: proxy (Cabal mode) -> Proxy mode
getMode _ = Proxy

class CabalMode mode where
  modeName :: proxy mode -> String

  -- | Optional minimal version of @cabal@ required.  Used to provide
  --   default instance of @canUseMode@.
  --
  --   @since 0.2.0.0
  needsMinCabal :: Maybe (Tagged (Cabal mode) Version)
  needsMinCabal = Nothing

  -- | @since 0.2.0.0
  canUseMode :: Env -> Tagged (Cabal mode) CommandPath -> IO Bool
  canUseMode env cp = case needsMinCabal of
                        Nothing -> return hasGHC
                        Just mv -> maybe hasGHC (mv <=)
                                   <$> commandVersion (envConfig env) cp
    where
      hasGHC = isJust (ghc (envTools env))

  cabalProjectRoot :: Tagged (Cabal mode) CommandPath
                      -> IO (Maybe (Tagged (Cabal mode) ProjectRoot))
  cabalProjectRoot = withTaggedF go
    where
      -- Type signature needed to make withTaggedF happy, though we
      -- don't actually use the command itself for this.
      go :: FilePath -> IO (Maybe FilePath)
      go _ = recurseUpFindFile isCabalFile

  hasModeArtifacts :: Tagged (Cabal mode) ProjectRoot -> IO Bool

  cabalPrepare :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalTargets :: Config -> Tagged (Cabal mode) CommandPath
                  -> IO [Tagged (Cabal mode) ProjectTarget]
  cabalTargets _ = withTaggedF go
    where
      -- Make withTaggedF happy
      go :: FilePath -> IO [String]
      go _ = cabalFileComponents

  -- | This is an additional function than found in 'BuildTool'.  May
  --   include installing dependencies.
  cabalConfigure :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalBuild :: Env -> Tagged (Cabal mode) CommandPath
                -> Maybe (Tagged (Cabal mode) ProjectTarget) -> IO ExitCode
  cabalBuild = commandArgTarget "build"

  cabalRepl :: Env -> Tagged (Cabal mode) CommandPath
               -> Tagged (Cabal mode) Args
               -> Maybe (Tagged (Cabal mode) ProjectTarget)
               -> IO ExitCode
  cabalRepl env cmd rargs = commandArgsTarget ("repl" : ghcArgs) env cmd
    where
      ghcArgs = ["--ghc-options", unwords (stripTag rargs :: Args)]

  cabalClean :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode

  cabalTest :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode
  cabalTest = commandArg "test"

  cabalBench :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode
  cabalBench = commandArg "bench"

  cabalExec :: Env -> Tagged (Cabal mode) CommandPath -> String -> Args -> IO ExitCode
  cabalExec env cmd prog progArgs = commandArgs args env cmd
    where
      args = "exec" : prog : "--" : progArgs

  cabalRun :: Env -> Tagged (Cabal mode) CommandPath -> Tagged (Cabal mode) ProjectTarget
              -> Args -> IO ExitCode
  cabalRun env cmd prog progArgs = commandArgs args env cmd
    where
      args = "run" : componentName (stripTag prog) : "--" : progArgs

  cabalUpdate :: Env -> Tagged (Cabal mode) CommandPath -> IO ExitCode
  cabalUpdate = commandArg "update"

--------------------------------------------------------------------------------

data Sandbox

instance CabalMode Sandbox where
  modeName _ = "sandbox"

  needsMinCabal = Just (tag (makeVersion [1,18]))

  hasModeArtifacts pr = doesFileExist (stripTag pr </> "cabal.sandbox.config")

  cabalPrepare = commandArgs ["sandbox", "init"]

  cabalConfigure env cmd = tryConfigure
    where
      install = commandArgs ["install", "--only-dependencies"
                            , "--enable-tests", "--enable-benchmarks"]
                            env cmd

      tryInstall = tryCommand "Installation failed; updating index."
                              (cabalUpdate env cmd)
                              install

      tryConfigure = tryCommand "Configuring failed; checking dependencies"
                                tryInstall
                                configure

      configure = commandArgs ["configure", "--enable-tests", "--enable-benchmarks"]
                              env cmd

  -- Note: we don't treat "dist" as part of the tool artifacts, but it
  -- doesn't make sense without the sandbox so remove it as well.
  cabalClean env cmd = commandArg "clean" env cmd
                       .&&. commandArgs ["sandbox", "delete"] env cmd

--------------------------------------------------------------------------------

data Nix

instance CabalMode Nix where
  modeName _ = "nix"

  -- We don't test for nix-instantiate here, as it's just used if it
  -- can be used.
  canUseMode env _ = return (has nixShell && has cabal2Nix)
    where
      has :: (NixSupport -> Maybe (Installed a)) -> Bool
      has f = isJust (f (nix (envTools env)))

  hasModeArtifacts pr = or <$> mapM (doesFileExist . (stripTag pr </>))
                                    ["shell.nix", "default.nix"]

  -- Note that commandPrepare is meant to be run within ProjectRoot
  cabalPrepare env _ = case path <$> cabal2Nix (nix (envTools env)) of
                         Nothing  -> die "cabal2Nix required"
                         Just c2n -> tryRunToFile (envConfig env) "shell.nix" c2n ["--shell", "."]

  -- It is tempting to want to run cabal2nix again here just in case,
  -- but people might have customised variants (different
  -- haskellPackages set, etc.).
  --
  -- Instead, people need to run @jbi prepare@ if the .cabal file
  -- changes.
  cabalConfigure env _ = case path <$> nixShell nixEnv of
                           Nothing -> die "nix-shell required"
                           Just ns -> do
                             -- We now evaluate canBench twice, which isn't ideal.
                             --
                             -- Should also warn if it's False.
                             args  <- extraArgs
                             cArgs <- cabalArgs
                             tryRunErr
                               "Configuration failed; you may need to manually enable 'withBenchmarkDepends' or 'doBenchmark' in your shell.nix file."
                               (tryRun cfg ns (args ++ ["--run", cArgs]))
    where
      extraArgs = bool [] ["--arg", "doBenchmark", "true"] <$> canBench

      nixEnv = nix (envTools env)
      cfg = envConfig env

      canBench =
        case path <$> nixInstantiate nixEnv of
          Nothing -> return False
          Just ni -> do
            res <- tryRunLine cfg (stripTag ni) ["--eval", "--expr", "with import <nixpkgs> {}; haskell.lib ? doBenchmark"]
            return $ case res of
                       Just "true" -> maybe False (>= c2nBenchSupport) (cabal2Nix nixEnv >>= version)
                       _           -> False

      c2nBenchSupport :: Tagged Cabal2Nix Version
      c2nBenchSupport = tag (makeVersion [2,6])

      cabalArgs = unwords . (["cabal", "configure", "--enable-tests"] ++) . bnchArgs <$> canBench
        where
          bnchArgs canB
            | canB      = ["--enable-benchmarks"]
            | otherwise = []

  cabalClean env cmd = commandArg "clean" env cmd
                       .&&. rmFile "shell.nix"
                       .&&. rmFile "default.nix"
    where
      rmFile file = do
        rmStatus <- tryIOError (removeFile file)
        case rmStatus of
          -- We're guessing as to which file is the one being used
          -- here, so an error because a file doesn't exist is OK;
          -- anything else is serious and should be re-thrown.
          Left err | not (isDoesNotExistError err) -> ioError err
          _        -> exitSuccess

--------------------------------------------------------------------------------

isCabalFile :: FilePath -> Bool
isCabalFile = (== ".cabal") . takeExtension

--------------------------------------------------------------------------------
-- The Cabal library likes to really keep changing things...

cabalFileComponents :: IO [String]
cabalFileComponents = do
  dir   <- getCurrentDirectory
  cntns <- map (dir </>) <$> listDirectory dir
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

--------------------------------------------------------------------------------

commandArgsTarget :: Args -> Env -> Tagged (Cabal mode) CommandPath
                     -> Maybe (Tagged (Cabal mode) ProjectTarget) -> IO ExitCode
commandArgsTarget args env cmd mt = commandArgs args' env cmd
  where
    args' = args ++ maybeToList (fmap stripTag mt)

commandArgTarget :: String -> Env -> Tagged (Cabal mode) CommandPath
                    -> Maybe (Tagged (Cabal mode) ProjectTarget) -> IO ExitCode
commandArgTarget = commandArgsTarget . (:[])

commandArg :: String -> Env -> Tagged (Cabal mode) CommandPath
              -> IO ExitCode
commandArg arg = commandArgs [arg]

commandArgs :: Args -> Env -> Tagged (Cabal mode) CommandPath
               -> IO ExitCode
commandArgs args env cmd = tryRun (envConfig env) cmd args
