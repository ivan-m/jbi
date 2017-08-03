{- |
   Module      : JBI.Environment.Global
   Description : Build tool agnostic environment
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is used by build tools to help them determine how they should
   run (as opposed to configuration environment which is their actual
   working directories, etc.).

 -}
module JBI.Environment.Global where

import JBI.Commands.Nix

--------------------------------------------------------------------------------

newtype GlobalEnv = GlobalEnv
  { nix :: NixSupport
  } deriving (Eq, Show, Read)
