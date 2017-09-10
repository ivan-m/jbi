{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

{- |
   Module      : System.JBI
   Description : Just Build It
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI
  ( WrappedTool
  , Valid
  , defaultTools
  , withTool
  , chooseTool
  , toolName
  , infoProjectDir
    -- * System state\/environment
  , GlobalEnv(..)
  , globalEnv
    -- * Information\/Diagnostics
  , Information (..)
  , getInformation
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

import System.JBI.Commands
import System.JBI.Commands.BuildTool (ToolInformation)
import System.JBI.Commands.Cabal
import System.JBI.Commands.Stack
import System.JBI.Environment

import Control.Applicative ((<|>))
import Data.Aeson          (ToJSON)
import Data.List           (find)
import Data.Maybe          (catMaybes, listToMaybe)
import Data.Proxy          (Proxy(..))
import GHC.Generics        (Generic)

import qualified Control.Monad.Parallel as P

--------------------------------------------------------------------------------

defaultTools :: [WrappedTool Proxy]
defaultTools = [ Wrapped (Proxy :: Proxy Stack)
               , Wrapped (Proxy :: Proxy (Cabal Nix))
               , Wrapped (Proxy :: Proxy (Cabal Sandbox))
               ]

withTool :: IO res -> (GlobalEnv -> WrappedTool Valid -> IO res)
            -> [WrappedTool proxy] -> IO res
withTool onFailure f tools = do
  env <- globalEnv
  mtool <- chooseTool env tools
  maybe onFailure (f env) mtool

chooseTool :: GlobalEnv -> [WrappedTool proxy] -> IO (Maybe (WrappedTool Valid))
chooseTool env tools = do
  valid <- catMaybes <$> P.mapM (checkValidity env) tools
  return (find alreadyUsed valid <|> listToMaybe valid)

data Information = Information
  { environment :: !GlobalEnv
  , toolDetails :: ![WrappedTool ToolInformation]
  } deriving (Show, Generic, ToJSON)

getInformation :: [WrappedTool proxy] -> IO Information
getInformation tools = do
  env <- globalEnv
  Information env <$> P.mapM (toolInformation env) tools
