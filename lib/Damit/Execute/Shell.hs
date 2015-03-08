-- | Convenient 'run' wrappers running commands in various common shells
module Damit.Execute.Shell
  ( sh
  , bash
  , zsh
  ) where

import Damit.Execute


-- | Ask @/bin/sh@ to evaluate the string.
sh :: String -> Command
sh c = run "/bin/sh" (args ["-c", c])

-- | Ask @bash@ to evaluate the string. Assumes @bash@ is installed and reachable on the host.
bash :: String -> Command
bash c = run "bash" (args ["-c", c])

-- | Ask @zsh@ to evaluate the string. Assumes @zsh@ is installed and reachable on the host.
zsh :: String -> Command
zsh c = run "zsh" (args ["-c", c])
