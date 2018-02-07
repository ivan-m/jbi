{- |
   Module      : Main
   Description : Just Build It!
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import Paths_jbi                     (version)
import System.JBI
import System.JBI.Commands.BuildTool (ProjectTarget(..), rootPath)
import System.JBI.Commands.Tool      (Args)

import Control.Applicative      (many, optional, (<*>), (<|>))
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.List                (intercalate)
import Data.Monoid              (mconcat, (<>))
import Data.Text.Lazy           (unpack)
import Data.Text.Lazy.Builder   (toLazyText)
import Data.Version             (showVersion)
import Options.Applicative      (Parser, ParserInfo, argument, command,
                                 eitherReader, execParser, flag', footer,
                                 fullDesc, header, help, helper, hsubparser,
                                 info, long, metavar, option, progDesc, short,
                                 str, strArgument, switch)
import System.Exit              (ExitCode(ExitSuccess), die, exitWith)

import Text.ParserCombinators.ReadP (ReadP, char, eof, get, munch1, readP_to_S,
                                     satisfy, skipSpaces)

--------------------------------------------------------------------------------

main :: IO ()
main = execParser parser >>= runAction defaultTools

data Action = Action
  { actConfig  :: !Config
  , actCommand :: !Command
  } deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

data Command = Prepare
             | Targets
             | Build (Maybe ProjectTarget)
             | REPL  Args (Maybe ProjectTarget)
             | Clean
             | Test
             | Bench
             | Exec String Args
             | Run ProjectTarget Args
             | Update
             | Info InfoType
             | Version
  deriving (Eq, Show, Read)

data InfoType = AvailableTools
              | ChosenTool
              | ProjectDir
              | Detailed
              deriving (Eq, Show, Read)

parser :: ParserInfo Action
parser = info (helper <*> prs) $
     header versionInfo
  <> fullDesc
  <> footer "No arguments is equivalent to running `build`"
  where
    prs = Action <$> parseConfig <*> parseCommand

versionInfo :: String
versionInfo = "jbi " ++ showVersion version ++ " - Just Build It and Hack On!"

parseConfig :: Parser Config
parseConfig = Config <$> parseDebug
  where
    parseDebug = switch (   long "debug"
                         <> help "Print debugging information whilst running."
                        )

parseCommand :: Parser Command
parseCommand = (hsubparser . mconcat $
  [ command "prepare" (info (pure Prepare)
                            (progDesc "Initialise the tool; usually not needed, \
                                      \but will (re-)generate the `shell.nix` for `cabal+nix`."))
  , command "targets" (info (pure Targets)
                            (progDesc "Print available targets"))
  , command "build"   (info (Build <$> optional parseTarget)
                            (progDesc "Build the project (optionally a specified target)."))
  , command "repl"    (info (REPL <$> parseReplArgs <*> optional parseTarget)
                            (progDesc "Start a REPL.  Passing in a target is optional, but \
                                      \highly recommended when multiple targets are available."))
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
  <|> flag' Version (long "version" <> short 'V')
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

parseReplArgs :: Parser Args
parseReplArgs = concat <$> many (option readArgs (   long "repl-opts"
                                                  <> metavar "ARGS"
                                                  <> help "Optional arguments to pass through to the REPL."
                                                 ))
  where
    readArgs = eitherReader $ \inp ->
      case readP_to_S parseSubArgs inp of
        [(args,"")] -> Right args
        []          -> Left "No ARGS parseable"
        _           -> Left "Ambiguous parse of ARGS"

-- Based upon Data.Attoparsec.Args.argsParser from stack
--
-- Using ReadP to avoid bringing in a parsing library for this one
-- small task.
parseSubArgs :: ReadP Args
parseSubArgs = many (skipSpaces *> (quoted <|> unquoted) <* skipSpaces)
               <* (eof <|> fail "Unterminated string")
  where
    -- munch1 is greedier than (many1 . satisfy)
    unquoted = munch1 (not . flip elem ['"', ' '])
    quoted = char '"' *> string <* char '"'
    string = many (escaped <|> nonquote)
    escaped = char '\\' *> get
    nonquote = satisfy (/='"')

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

runAction :: [WrappedTool proxy] -> Action -> IO ()
runAction tools act = do
  ec <- case actCommand act of
          Prepare       -> tooled prepare
          Build mt      -> tooled (build mt)
          REPL args mt  -> tooled (repl args mt)
          Clean         -> tooled clean
          Test          -> tooled test
          Bench         -> tooled bench
          Exec exe args -> tooled (exec exe args)
          Run exe args  -> tooled (run exe args)
          Update        -> tooled update
          Targets       -> printSuccess printTargets
          Info it       -> printSuccess (printInfo it)
          Version       -> putStrLn versionInfo >> returnSuccess
  exitWith ec
  where
    tooled :: (Env -> WrappedTool Valid -> IO a) -> IO a
    tooled f = withTool cfg toolFail f tools

    toolFail :: IO a
    toolFail = die "No possible tool found."

    returnSuccess = return ExitSuccess

    printSuccess ma = do out <- ma
                         putStrLn out
                         returnSuccess

    printTargets = tooled ((fmap (multiLine . map projectTarget) .) . targets)

    withChosen f = do env <- getEnvironment cfg
                      mTool <- chooseTool env tools
                      maybe toolFail (return . f) mTool

    jsonStr = unpack . toLazyText . encodePrettyToTextBuilder

    printInfo AvailableTools = return . multiLine . map toolName $ tools
    printInfo ChosenTool     = withChosen toolName
    printInfo ProjectDir     = withChosen (rootPath . infoProjectDir)
    printInfo Detailed       = jsonStr <$> getInformation cfg tools

    cfg = actConfig act

-- Unlike unlines, this doesn't add a trailing newline.
multiLine :: [String] -> String
multiLine = intercalate "\n"
