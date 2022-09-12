{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Config
  ( Config
  , getConfig
  ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Env
import qualified Options.Applicative as Opt
import           Prelude hiding (length)

import qualified Meta_dazu as Meta


-- This helps us avoid type annotations in the ToJSON instance.
default (Integer, Double, Text)

data Config = Config
 { apiKey  :: Text
 , n       :: Int64
 , length  :: Int64
 } deriving (Show, Eq)

instance Aeson.ToJSON Config where
  toJSON Config { apiKey, n, length } = Aeson.object
    [ "jsonrpc" .= "2.0"
    , "method" .= "generateSignedStrings"
    , "params" .= Aeson.object
      [ "apiKey" .= apiKey
      , "n" .= n
      , "length" .= length
      , "characters" .=
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
          \abcdefghijklmnopqrstuvwxyz\
          \0123456789\
          \!@#$%^&*()-_=+"
      ]
    , "id" .= 0
    ]

getConfig :: IO Config
getConfig = do
  apiKey
    <- Env.parse
         (Env.header usageHeader . Env.footer ("You can get the key at <" <> apiKeyUrl <> ">"))
         (Env.var Env.str
           "RANDOMORG_API_KEY"
           (Env.help "random.org Beta API key"))
  (n, length)
    <- Opt.customExecParser
         (Opt.prefs Opt.showHelpOnError)
         (Opt.info (Opt.helper <*> opts)
                   (Opt.fullDesc <> Opt.header usageHeader))
  return Config { apiKey, n, length }
 where
  apiKeyUrl = "https://api.random.org/api-keys/beta"

  opts = (,)
    <$> Opt.option Opt.auto
                   (Opt.short 'n' <>
                    Opt.metavar "N" <>
                    Opt.value 10 <>
                    Opt.help "Number of passwords to generate")
    <*> Opt.option Opt.auto
                   (Opt.long "length" <>
                    Opt.metavar "M" <>
                    Opt.value 12 <>
                    Opt.help "Length of generated passwords")

usageHeader :: String
usageHeader =
  unwords [Meta.name, version]

version :: String
version =
  Meta.version ++ "-" ++ Meta.hash
