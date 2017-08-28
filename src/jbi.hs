{- |
   Module      : Main
   Description : Just Build It!
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import JBI
import JBI.Commands.BuildTool (ProjectTarget(..))
import JBI.Commands.Tool      (Args)
import Paths_jbi              (version)

import Data.Monoid         (mconcat, (<>))
import Data.Version        (showVersion)
import Options.Applicative
import System.Exit         (ExitCode(ExitSuccess), die, exitWith)

--------------------------------------------------------------------------------

main :: IO ()
main = execParser parser >>= runCommand defaultTools

--------------------------------------------------------------------------------

data Command = Prepare
             | Targets
             | Build (Maybe ProjectTarget)
             | REPL  (Maybe ProjectTarget)
             | Clean
             | Test
             | Bench
             | Exec String Args
             | Run ProjectTarget Args
             | Update
  deriving (Eq, Show, Read)

parser :: ParserInfo Command
parser = info (helper <*> parseCommand) $
     header versionInfo
  <> fullDesc
  <> footer "No arguments is equivalent to running `build`"

versionInfo :: String
versionInfo = "jbi " ++ showVersion version ++ " - Just Build It and Hack On!"

parseCommand :: Parser Command
parseCommand = (subparser . mconcat $
  [ command "prepare" (info (pure Prepare)
                            (progDesc "Initialise the tool; usually not needed, \
                                      \but will (re-)generate the `shell.nix` for `cabal+nix`."))
  , command "targets" (info (pure Targets)
                            (progDesc "Print available targets"))
  , command "build"   (info (Build <$> optional parseTarget)
                            (progDesc "Build the project (optionally a specified target)."))
  , command "repl"    (info (REPL <$> optional parseTarget)
                            (progDesc "Start a REPL (optionally for a specified target)."))
  , command "clean"   (info (pure Clean)
                            (progDesc "Remove all build artifacts."))
  , command "test"    (info (pure Test)
                            (progDesc "Run test suite(s)."))
  , command "bench"   (info (pure Bench)
                            (progDesc "Run benchmark(s)."))
  , command "exec"    (info (Exec <$> parseExecutable <*> parseArgs)
                            (progDesc "Run the specified executable within the build environment."))
  , command "run"     (info (Run <$> parseTarget <*> parseArgs)
                            (progDesc "Run the specified executable target within the build environment."))
  , command "update"  (info (pure Update)
                            (progDesc "Update the package index (usually not needed)."))
  ])
  <|> pure (Build Nothing)

parseTarget :: Parser ProjectTarget
parseTarget = strArgument (   metavar "TARGET"
                           <> help "Project target (see the `targets` command)"
                          )

parseExecutable :: Parser String
parseExecutable = strArgument (   metavar "COMMAND"
                               <> help "A command/executable"
                              )

parseArgs :: Parser Args
parseArgs = many (argument str (   metavar "ARG"
                                <> help "Optional arguments to pass through to the command"
                               ))

--------------------------------------------------------------------------------

runCommand :: [WrappedTool proxy] -> Command -> IO ()
runCommand tools cmd = do
  ec <- tooled $
    case cmd of
      Prepare       -> prepare
      Targets       -> printTargets
      Build mt      -> build mt
      REPL mt       -> repl mt
      Clean         -> clean
      Test          -> test
      Bench         -> bench
      Exec exe args -> exec exe args
      Run exe args  -> run exe args
      Update        -> update
  exitWith ec
  where
    tooled f = withTool (die "No possible tool found.") f tools

    printTargets _ wv = do tgts <- targets wv
                           mapM_ (putStrLn . projectTarget) tgts
                           return ExitSuccess
