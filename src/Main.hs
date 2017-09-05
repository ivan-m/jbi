{- |
   Module      : Main
   Description : Just Build It!
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import JBI
import JBI.Commands.BuildTool (ProjectTarget(..), rootPath)
import JBI.Commands.Tool      (Args)
import Paths_jbi              (version)

import Control.Applicative      (many, optional, (<*>), (<|>))
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.List                (intercalate)
import Data.Monoid              (mconcat, (<>))
import Data.Text.Lazy           (unpack)
import Data.Text.Lazy.Builder   (toLazyText)
import Data.Version             (showVersion)
import Options.Applicative      (Parser, ParserInfo, argument, command,
                                 execParser, footer, fullDesc, header, help,
                                 helper, hsubparser, info, metavar, progDesc,
                                 str, strArgument)
import System.Exit              (ExitCode(ExitSuccess), die, exitWith)

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
             | Info InfoType
  deriving (Eq, Show, Read)

data InfoType = AvailableTools
              | ChosenTool
              | ProjectDir
              | Detailed
              deriving (Eq, Show, Read)

parser :: ParserInfo Command
parser = info (helper <*> parseCommand) $
     header versionInfo
  <> fullDesc
  <> footer "No arguments is equivalent to running `build`"

versionInfo :: String
versionInfo = "jbi " ++ showVersion version ++ " - Just Build It and Hack On!"

parseCommand :: Parser Command
parseCommand = (hsubparser . mconcat $
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
  , command "info"    (info (Info <$> parseInfo)
                            (progDesc "Build tool information; useful for debugging."))
  ])
  <|> pure (Build Nothing)

parseTarget :: Parser ProjectTarget
parseTarget = argument (ProjectTarget <$> str) (   metavar "TARGET"
                                                <> help "Project target (see the `targets` command)"
                                               )

parseExecutable :: Parser String
parseExecutable = strArgument (   metavar "COMMAND"
                               <> help "A command/executable"
                              )

parseArgs :: Parser Args
parseArgs = many (strArgument (   metavar "ARG"
                               <> help "Optional arguments to pass through to the command"
                              ))

parseInfo :: Parser InfoType
parseInfo = hsubparser . mconcat $
  [ command "tools"   (info (pure AvailableTools)
                            (progDesc "Print all known build tools."))
  , command "chosen"  (info (pure ChosenTool)
                            (progDesc "Print the build tool chosen."))
  , command "project" (info (pure ProjectDir)
                            (progDesc "Path to the project directory."))
  , command "details" (info (pure Detailed)
                            (progDesc "Print detailed information about build tools."))
  ]

--------------------------------------------------------------------------------

runCommand :: [WrappedTool proxy] -> Command -> IO ()
runCommand tools cmd = do
  ec <- case cmd of
          Prepare       -> tooled prepare
          Build mt      -> tooled (build mt)
          REPL mt       -> tooled (repl mt)
          Clean         -> tooled clean
          Test          -> tooled test
          Bench         -> tooled bench
          Exec exe args -> tooled (exec exe args)
          Run exe args  -> tooled (run exe args)
          Update        -> tooled update
          Targets       -> printSuccess printTargets
          Info it       -> printSuccess (printInfo it)
  exitWith ec
  where
    tooled :: (GlobalEnv -> WrappedTool Valid -> IO a) -> IO a
    tooled f = withTool toolFail f tools

    toolFail :: IO a
    toolFail = die "No possible tool found."

    printSuccess act = do out <- act
                          putStrLn out
                          return ExitSuccess

    printTargets = tooled ((fmap (multiLine . map projectTarget) .) . targets)

    withChosen f = do env <- globalEnv
                      mTool <- chooseTool env tools
                      maybe toolFail (return . f) mTool

    jsonStr = unpack . toLazyText . encodePrettyToTextBuilder

    printInfo AvailableTools = return . multiLine . map toolName $ tools
    printInfo ChosenTool     = withChosen toolName
    printInfo ProjectDir     = withChosen (rootPath . infoProjectDir)
    printInfo Detailed       = jsonStr <$> getInformation tools

-- Unlike unlines, this doesn't add a trailing newline.
multiLine :: [String] -> String
multiLine = intercalate "\n"
