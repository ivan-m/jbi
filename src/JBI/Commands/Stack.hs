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

getCommand :: IO Command
getCommand = defaultCommand "stack"
