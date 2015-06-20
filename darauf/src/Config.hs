{-# LANGUAGE NamedFieldPuns #-}
module Config
  ( Config(..)
  , get
  ) where

import Control.Lens
import Env
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))


data Config = Config
  { configNixChannels :: FilePath
  } deriving (Show, Eq)

get :: IO Config
get = do
  nixChannels <- Env.parse (header "darauf" <> desc "Utility for querying Nix channels' status") $
    var (fmap pure . str)
        "DARAUF_NIX_CHANNELS"
        (help "Path to the nix-channels file" <> def defaultNixChannels <> helpDef (pure "~/.nix-channels"))
  Config <$> nixChannels
 where
  defaultNixChannels =
    getHomeDirectory <&> \home -> home </> ".nix-channels"
