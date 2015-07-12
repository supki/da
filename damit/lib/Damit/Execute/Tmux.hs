{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Convenient 'run' wrapper for tmux <http://tmux.sourceforge.net/>
module Damit.Execute.Tmux
  ( tmux
  , TmuxOptions
  , binary
  , directory
  , cmd
  , socket
  ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (foldMap)
#endif
import Data.Monoid ((<>))
import System.FilePath ((</>))
import Prelude hiding (mod)

import Damit.Execute.Internal


-- | Start or attach to a tmux session.
tmux :: String -> Mod TmuxOptions -> Command
tmux n (Mod f) = run _binary mod
 where
  TmuxOptions { _cmd, _directory, _env, _binary, _socket } =
    f defaultTmuxOptions
  mod =
    args (foldMap (\s -> ["-S", s]) _socket ++
          ["new-session", "-AD", "-s", n] ++
          foldMap (\d -> ["-c", "${HOME}" </> d]) _directory ++
          foldMap return _cmd)
    <> env _env

-- | Tmux client call options with sane defaults.  See 'binary', 'directory',
-- 'cmd', 'socket', and 'env' for available customizations.
data TmuxOptions = TmuxOptions
  { _binary    :: String
  , _directory :: Maybe String
  , _cmd       :: Maybe String
  , _socket    :: Maybe FilePath
  , _env       :: [(String, String)]
  } deriving (Show, Eq)

defaultTmuxOptions :: TmuxOptions
defaultTmuxOptions = TmuxOptions
  { _binary    = "tmux"
  , _directory = Nothing
  , _cmd       = Nothing
  , _socket    = Nothing
  , _env       = []
  }

-- | Path to the tmux binary on the host.  By default, @PATH@ is assumed to
-- contain the @tmux@ executable which is then called.
binary :: FilePath -> Mod TmuxOptions
binary x = Mod (\t -> t { _binary = x })

-- | @directory "foo"@ changes the directory to @"foo"@ right before the session
-- is created.  The default is to stay in the current directory.
directory :: FilePath -> Mod TmuxOptions
directory x = Mod (\t -> t { _directory = Just x })

-- | Command to run when the session is created.  By default simply starts up the shell.
cmd :: String -> Mod TmuxOptions
cmd x = Mod (\t -> t { _cmd = Just x })

-- | Socket to connect to.  The default is to let @tmux@ decide for itself.
socket :: FilePath -> Mod TmuxOptions
socket x = Mod (\t -> t { _socket = Just x })

instance HasEnv TmuxOptions [(String, String)] where
  setEnv x t = t { _env = x }
