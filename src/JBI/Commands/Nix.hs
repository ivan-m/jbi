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

import Data.Char  (isSpace)
import Data.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------

data NixSupport = NixSupport
  { nixShell  :: !(Maybe Installed)
  , cabal2Nix :: !(Maybe Installed)
  } deriving (Eq, Ord, Show, Read)

findNixSupport :: IO NixSupport
findNixSupport = do
  mns  <- commandInformation
  mc2n <- commandInformation
  return $ NixSupport
    { nixShell  = (`proxy` pNixShell)  <$> mns
    , cabal2Nix = (`proxy` pCabal2Nix) <$> mc2n
    }

data NixShell

pNixShell :: Proxy NixShell
pNixShell = Proxy

instance Tool NixShell where
  commandName = "nix-shell"

data Cabal2Nix

pCabal2Nix :: Proxy Cabal2Nix
pCabal2Nix = Proxy

instance Tool Cabal2Nix where
  commandName = "cabal2nix"

  commandVersion = withTaggedF (tryFindVersionBy getVer)
    where
      -- There's a digit in the command name, so the naive approach
      -- doesn't work.
      getVer = takeVersion . drop 1 . dropWhile (not . isSpace)
