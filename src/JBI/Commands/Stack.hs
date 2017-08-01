{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : JBI.Commands.Stack
   Description : Stack commands
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands.Stack where

import JBI.Commands.Common

--------------------------------------------------------------------------------

data Stack

instance BuildTool Stack where
  commandName = "stack"
