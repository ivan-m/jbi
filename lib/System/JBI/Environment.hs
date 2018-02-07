{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

{- |
   Module      : System.JBI.Environment
   Description : Build tool agnostic environment
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is used by build tools to help them determine how they should
   run (as opposed to configuration environment which is their actual
   working directories, etc.).

 -}
module System.JBI.Environment where

import System.JBI.Commands.Nix
import System.JBI.Commands.Tool

import Data.Aeson   (ToJSON)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data ToolEnv = ToolEnv
  { nix :: NixSupport
  , ghc :: Maybe (Installed GHC)
  } deriving (Eq, Show, Read, Generic, ToJSON)

toolEnv :: IO ToolEnv
toolEnv = ToolEnv <$> findNixSupport <*> commandInformation
