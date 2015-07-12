-- |
--
-- The two main parts of launching any command is getting there ('hop') and
-- actually running the executable ('run').  You will also probably want to look at ('pretty')
-- the command before you run ('fork', 'exec') it.
--
-- With this \"framework\" in place we can define convenience wrappers for specific
-- commands, like those in "Damit.Execute.Tmux" and "Damit.Execute.Shell".  The "Damit.Execute.Internal"
-- module provides the tools to define your own.
module Damit.Execute
  ( Command
  , hop
  , HopOptions
  , login
  , identity
  , run
  , RunOptions
  , args
  , env
  , (.=)
  , Mod
  , fork
  , exec
  , pretty
  ) where

import Damit.Execute.Internal
