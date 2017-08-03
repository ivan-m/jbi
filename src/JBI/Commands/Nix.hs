{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : JBI.Commands.Nix
   Description : Nix tooling support
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Nix where

import JBI.Commands.Tool
import JBI.Tagged

import Control.Applicative (liftA2)
import Data.Char           (isSpace)

--------------------------------------------------------------------------------

data NixSupport = NixSupport
  { nixShell  :: !(Maybe (Installed NixShell))
  , cabal2Nix :: !(Maybe (Installed Cabal2Nix))
  } deriving (Eq, Ord, Show, Read)

findNixSupport :: IO NixSupport
findNixSupport = liftA2 NixSupport commandInformation
                                   commandInformation

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
