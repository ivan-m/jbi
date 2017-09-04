{-# LANGUAGE FlexibleInstances, GADTs, OverloadedStrings, RankNTypes,
             StandaloneDeriving #-}

{- |
   Module      : JBI.Commands
   Description : Running a specific build tool
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands
  ( WrappedTool (..)
  , Valid
  , toolName
  , toolInformation
  , checkValidity
  , alreadyUsed
    -- * Commands
  , prepare
  , targets
  , build
  , repl
  , clean
  , test
  , bench
  , exec
  , run
  , update
  ) where

import JBI.Commands.BuildTool
import JBI.Commands.Tool
import JBI.Environment
import JBI.Tagged

import Control.Monad    (forM)
import Data.Aeson       (ToJSON(toJSON))
import Data.Function    (on)
import Data.Proxy       (Proxy(..))
import System.Directory (withCurrentDirectory)
import System.Exit      (ExitCode(ExitSuccess), die)

--------------------------------------------------------------------------------

data WrappedTool proxy where
  Wrapped :: (NamedTool bt) => proxy bt -> WrappedTool proxy

-- | Not made polymorphic as there might be extra data contained
--   within.
instance Eq (WrappedTool Proxy) where
  (==) = (==) `on` toolName

-- | Not really a valid instance as it doesn't produce Haskell code.
instance Show (WrappedTool Proxy) where
  show = toolName

deriving instance Show (WrappedTool ToolInformation)
deriving instance Show (WrappedTool Valid)

instance ToJSON (WrappedTool ToolInformation) where
  toJSON = withWrapped toJSON

withWrapped :: (forall bt. (NamedTool bt) => proxy bt -> res)
               -> WrappedTool proxy -> res
withWrapped f (Wrapped bt) = f bt

toolName :: WrappedTool proxy -> String
toolName = withWrapped prettyName

toolInformation :: GlobalEnv -> WrappedTool proxy -> IO (WrappedTool ToolInformation)
toolInformation env (Wrapped pr) = Wrapped <$> commandToolInformation env pr

--------------------------------------------------------------------------------

data Valid bt = Valid
  { command      :: !(Installed bt)
  , projectDir   :: !(Tagged bt ProjectRoot)
  , hasArtifacts :: !Bool
    -- ^ Only to be used with 'ensurePrepared', 'prepare', 'unprepared'.
  } deriving (Eq, Ord, Show, Read)

alreadyUsed :: WrappedTool Valid -> Bool
alreadyUsed = withWrapped hasArtifacts

-- This is pretty ugly; one way to clean it up would be to use MaybeT.
checkValidity :: GlobalEnv -> WrappedTool proxy -> IO (Maybe (WrappedTool Valid))
checkValidity env (Wrapped p) = fmap Wrapped <$> check p
  where
    check :: (BuildTool bt) => proxy' bt -> IO (Maybe (Valid bt))
    check _ = do
      mInst <- commandInformation
      case mInst of
        Nothing   -> return Nothing
        Just inst -> do
          usbl <- canUseCommand env inst
          if not usbl
             then return Nothing
             else do mroot <- commandProjectRoot (path inst)
                     forM mroot $ \root ->
                       Valid inst root <$> hasBuildArtifacts root

runInProject :: (forall bt. (BuildTool bt) => Tagged bt CommandPath -> IO res)
                -> WrappedTool Valid -> IO res
runInProject f (Wrapped val) = withCurrentDirectory (stripTag (projectDir val))
                                                    (f (path (command val)))

prepareWrapped :: GlobalEnv -> WrappedTool Valid -> IO (WrappedTool Valid)
prepareWrapped env wt@(Wrapped val) = do
  ec <- runInProject (commandPrepare env) wt
  case ec of
    ExitSuccess -> do
      hasArt <- canUseCommand env (command val)
      if hasArt
         then return (Wrapped (val { hasArtifacts = True }))
         else die "Preparation failed"
    _           -> die "Could not prepare"

runPrepared :: (forall bt. (BuildTool bt) => GlobalEnv -> Tagged bt CommandPath -> IO res)
               -> GlobalEnv -> WrappedTool Valid -> IO res
runPrepared f env wv = do
  wv' <- if not (alreadyUsed wv)
            then prepareWrapped env wv
            else return wv
  runInProject (f env) wv'

--------------------------------------------------------------------------------
-- This mimics the actual command-level portion of BuildTool

prepare :: GlobalEnv -> WrappedTool Valid -> IO ExitCode
prepare env wv = prepareWrapped env wv >> return ExitSuccess
-- Explicitly prepare.

targets :: GlobalEnv -> WrappedTool Valid -> IO [ProjectTarget]
targets = runPrepared (const (fmap stripTags . commandTargets))

build :: Maybe ProjectTarget -> GlobalEnv -> WrappedTool Valid -> IO ExitCode
build targ = runPrepared (\env cp -> commandBuild env cp (tagInner (tag targ)))

repl :: Maybe ProjectTarget -> GlobalEnv -> WrappedTool Valid -> IO ExitCode
repl targ = runPrepared (\env cp -> commandRepl env cp (tagInner (tag targ)))

clean :: GlobalEnv -> WrappedTool Valid -> IO ExitCode
clean = runPrepared commandClean

test :: GlobalEnv -> WrappedTool Valid -> IO ExitCode
test = runPrepared commandTest

bench :: GlobalEnv -> WrappedTool Valid -> IO ExitCode
bench = runPrepared commandBench

exec :: String -> Args -> GlobalEnv -> WrappedTool Valid -> IO ExitCode
exec cmd args = runPrepared (\env cp -> commandExec env cp cmd args)

run :: ProjectTarget -> Args -> GlobalEnv -> WrappedTool Valid -> IO ExitCode
run targ args = runPrepared (\env cp -> commandRun env cp (tag targ) args)

update :: GlobalEnv -> WrappedTool Valid -> IO ExitCode
update = runPrepared commandUpdate
