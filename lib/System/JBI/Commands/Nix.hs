{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

{- |
   Module      : System.JBI.Commands.Nix
   Description : Nix tooling support
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module System.JBI.Commands.Nix where

import System.JBI.Commands.Tool
import System.JBI.Tagged

import Data.Aeson   (ToJSON)
import Data.Char    (isSpace)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data NixSupport = NixSupport
  { nixShell       :: !(Maybe (Installed NixShell))
  , cabal2Nix      :: !(Maybe (Installed Cabal2Nix))
  , nixInstantiate :: !(Maybe (Installed NixInstantiate))
  } deriving (Eq, Ord, Show, Read, Generic, ToJSON)

findNixSupport :: IO NixSupport
findNixSupport = NixSupport <$> commandInformation
                            <*> commandInformation
                            <*> commandInformation

data NixShell

instance Tool NixShell where
  commandName = "nix-shell"

data Cabal2Nix

instance Tool Cabal2Nix where
  commandName = "cabal2nix"

  commandVersion = withTaggedF (tryFindVersionBy getVer)
    where
      -- There's a digit in the command name, so the naive approach
      -- doesn't work.
      getVer = takeVersion . drop 1 . dropWhile (not . isSpace)

data NixInstantiate

instance Tool NixInstantiate where
  commandName = "nix-instantiate"
