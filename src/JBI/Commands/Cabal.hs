{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Cabal
   Description : cabal-install support
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Cabal where

import JBI.Commands.BuildTool
import JBI.Commands.Tool
import JBI.Environment.Global
import JBI.Tagged

--------------------------------------------------------------------------------

data Cabal mode

instance Tool (Cabal mode) where
  commandName = "cabal"

class CabalMode mode where
  cabalProjectRoot :: Tagged (Cabal mode) CommandPath
                      -> IO (Maybe (Tagged (Cabal mode) ProjectRoot))
