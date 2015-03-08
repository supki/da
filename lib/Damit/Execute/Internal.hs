{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module Damit.Execute.Internal
  ( Command(..)
  , hop
  , HopOptions
  , login
  , identity
  , run
  , RunOptions
  , HasArgs(..)
  , args
  , HasEnv(..)
  , env
  , (.=)
  , defaultOptions
  , Mod(..)
  , fork
  , exec
  , pretty
  ) where

import           Control.Monad (void, (<=<))
import           Data.Foldable (foldMap)
import           Data.Monoid (Monoid(..))
import qualified System.Posix as Posix

infix 1 .=


-- | The type for things that can be run on some host.
data Command =
    Hop String HopOptions Command
  | Run FilePath RunOptions
    deriving (Show, Eq)

-- | Connect to another host and run the command there.
--
-- It'll make use of the available SSH-agents and read SSH configuration files on the host.
hop :: String -> Mod HopOptions -> Command -> Command
hop h (Mod f) = Hop h (f defaultHopOptions)

-- | SSH client options.  See 'login' and 'identity' for available customizations.
data HopOptions = HopOptions
  { _login :: Maybe String
  , _identity :: Maybe FilePath
  } deriving (Show, Eq)

defaultHopOptions :: HopOptions
defaultHopOptions = HopOptions
  { _login = Nothing
  , _identity = Nothing
  }

-- | Set the SSH login.  By default the name of the user who runs the command is used.
login :: String -> Mod HopOptions
login x = Mod (\t -> t { _login = Just x })

-- | Set the path to the private key to use.
identity :: FilePath -> Mod HopOptions
identity x = Mod (\t -> t { _identity = Just x })

-- | Run the command.  @PATH@ will be searched for the given file path.
run :: FilePath -> Mod RunOptions -> Command
run e (Mod f) = Run e (f defaultOptions)

-- | General command options.  See 'args' and 'env' for supported modifications.
data RunOptions = RunOptions
  { _args :: [String]
  , _env  :: [(String, String)]
  } deriving (Show, Eq)

defaultOptions :: RunOptions
defaultOptions = RunOptions
  { _args = []
  , _env  = []
  }

class HasArgs s a | s -> a where
  setArgs :: a -> s -> s

instance HasArgs RunOptions [String] where
  setArgs x t = t { _args = x }

-- | Set the command arguments.  The list of arguments is empty by default.
args :: HasArgs s a => a -> Mod s
args = Mod . setArgs

class HasEnv s a | s -> a where
  setEnv :: a -> s -> s

instance HasEnv RunOptions [(String, String)] where
  setEnv x t = t { _env = x }

-- | Set the environment in which to run the command.  The environment is unmodified by default.
env :: HasEnv s a => a -> Mod s
env = Mod . setEnv

-- | An alias for '(,)' making defining large lists of environment variables' values palatable.
(.=) :: a -> b -> (a, b)
(.=) = (,)

-- | Option modifier.  The simplest modifiers represent the modification of a particular
-- property.  They can be composed using their 'Monoid' instance.
--
-- @
--   Mod t = 'Dual' ('Endo' t)
-- @
newtype Mod t = Mod (t -> t)

instance Monoid (Mod t) where
  mempty = Mod id
  Mod f `mappend` Mod g = Mod (g . f)

-- | Fork the child process for an action and wait for its completion.  This
-- function isn't directly related to anything damit does, but it's nevertheless
-- useful in conjunction with 'exec'.
fork :: IO a -> IO ()
fork = void . Posix.getProcessStatus True False <=< Posix.forkProcess . void

-- | Execute the 'Command'.  It calls @execve@ underneath, so you won't hear back
-- from the Haskell process, unless it fails for some reason.  In the latter case, you'll get
-- an exception.
exec :: Command -> IO a
exec c = Posix.executeFile x True xs Nothing where x : xs = command 0 c

-- | Convert a 'Command' to a 'String', which, when run in the shell, will do
-- the same thing 'exec' does.
pretty :: Command -> String
pretty = unwords . command 1

-- It's important to have escaping level as an argument, because pretty-printing and
-- execution have different requirements for escaping.  The former passes the arguments
-- to the @execve@ call verbatim, the latter attempts to generate the string suitable for
-- copy-pasting into the command line.
command
  :: Int -- ^ escaping level
  -> Command -> [String]
command n (Hop h HopOptions { _identity, _login } xs) =
  "ssh" : h :
  foldMap (\l -> ["-l", l]) _login ++
  foldMap (\i -> ["-i", i]) _identity ++
  "-t" : command (max 1 (n * 2)) xs
command n (Run bin RunOptions { _env, _args }) =
  case _env of
    [] -> map escape (bin : _args)
    vs -> "env" : map (\(k, v) -> k ++ "=" ++ escape v) vs ++ map escape (bin : _args)
 where
  escape :: String -> String
  escape = concatMap escapeChar

  escapeChar :: Char -> String
  escapeChar c
    | c `elem` escaped  = replicate n '\\' ++ return c
    | c `elem` escaping = replicate (n `div` 2) '\\' ++ return c
    | otherwise         = return c

  escaped :: [Char]
  escaped = " &"

  escaping :: [Char]
  escaping = "$"
