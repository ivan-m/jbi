{- |
   Module      : JBI.Commands
   Description : Running a specific build tool
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Commands where

data BuildTool = Stack
               | Cabal
               | CabalNew
               | Manual
  deriving (Eq, Ord, Show, Read)
