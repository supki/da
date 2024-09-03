{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Config
  ( Config
  , getConfig
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Int (Int64)
import Data.Text (Text)
import Env qualified
import Options.Applicative qualified as Opt
import Prelude hiding (length)

import Meta_dazu qualified as Meta


data Config = Config
 { apiKey  :: Text
 , n       :: Int64
 , length  :: Int64
 } deriving (Show, Eq)

instance Aeson.ToJSON Config where
  toJSON config =
    Aeson.object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= ("generateSignedStrings" :: Text)
      , "params" .= Aeson.object
        [ "apiKey" .= config.apiKey
        , "n" .= config.n
        , "length" .= config.length
        , "characters" .=
           ("ABCDEFGHIJKLMNOPQRSTUVWXYZ\
            \abcdefghijklmnopqrstuvwxyz\
            \0123456789\
            \!@#$%^&*()-_=+" :: Text)
        ]
      , "id" .= (0 :: Int64)
      ]

getConfig :: IO Config
getConfig = do
  apiKey
    <- Env.parse
         (Env.header usageHeader . Env.footer ("You can get the key at <" <> apiKeyUrl <> ">"))
         (Env.var Env.str
           "RANDOMORG_APIKEY"
           (Env.help "random.org Beta API key"))
  (n, length)
    <- Opt.customExecParser
         (Opt.prefs Opt.showHelpOnError)
         (Opt.info (Opt.helper <*> opts)
                   (Opt.fullDesc <> Opt.header usageHeader))
  return Config {apiKey, n, length}
 where
  apiKeyUrl = "https://api.random.org/dashboard"

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
