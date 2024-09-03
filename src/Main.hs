{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (traverse_)
import Data.Text.IO qualified as Text
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Http
import Prelude hiding (length)
import System.Exit (die)

import Config (Config, getConfig)
import Result qualified
import Sign qualified


main :: IO ()
main = do
  cfg <- getConfig
  runApp cfg

runApp :: Config -> IO ()
runApp cfg = do
  man <- Http.newManager Http.tlsManagerSettings
  bytes <- randomOrg cfg man
  case Result.parse bytes of
    Left err ->
      die ("Couldn't parse the response from <random.org>: " <> err)
    Right res
      | Sign.verify res.signature res.signedData -> do
          traverse_ Text.putStrLn res.passwords
      | otherwise ->
          die "Couldn't verify the passwords really are from <random.org>"

randomOrg :: Config -> Http.Manager -> IO Lazy.ByteString
randomOrg cfg man = do
  req0 <- Http.parseUrlThrow "POST https://api.random.org/json-rpc/2/invoke"
  let req = req0
        { Http.requestHeaders =
            [ ("Content-Type", "application/json")
            ]
        , Http.requestBody =
            Http.RequestBodyLBS (Aeson.encode cfg)
        }
  res <- Http.httpLbs req man
  pure (Http.responseBody res)
