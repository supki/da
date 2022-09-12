{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative (empty)
import           Control.Monad (unless)
import           Crypto.Hash.Algorithms (SHA512(SHA512))
import qualified Crypto.PubKey.RSA.PKCS15 as Rsa
import           Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as Lazy
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import           Prelude hiding (length)
import           System.Exit (die, exitFailure)

import           Config (Config, getConfig)
import           TH (key)


main :: IO ()
main = runApp =<< getConfig

runApp :: Config -> IO ()
runApp cfg = do
  url <- Http.parseUrlThrow "https://api.random.org/json-rpc/2/invoke"
  man <- Http.newManager Http.tlsManagerSettings
  let body = Aeson.encode cfg
      req = url
        { Http.method = "POST"
        , Http.requestHeaders = [("Content-Type", "application/json")]
        , Http.requestBody = Http.RequestBodyLBS body
        }
  res <- Http.httpLbs req man
  result (Http.responseBody res)

result :: Lazy.ByteString -> IO ()
result x = case Aeson.decode x of
  Nothing -> exitFailure
  Just Result { data_, signature } -> do
    let (_, y) = Strict.breakSubstring "{\"method\":" (Lazy.toStrict x)
        (z, _) = Strict.breakSubstring ",\"signature\":" y
    traverse_ Text.putStrLn data_
    unless (verify signature z)
           (die "Couldn't verify the passwords really are from <random.org>")

data Result = Result
  { data_     :: [Text]
  , signature :: Signature
  } deriving (Show, Eq)

instance Aeson.FromJSON Result where
  parseJSON (Aeson.Object o) = do
    r <- o .: "result"
    data_ <- (r .: "random") >>= \o' -> o' .: "data"
    x <- r .: "signature"
    case fmap Signature (Base64.decode (Text.encodeUtf8 x)) of
      Right signature -> return Result { data_, signature }
      _ -> empty
  parseJSON _ = empty

newtype Signature = Signature Strict.ByteString
    deriving (Show, Eq)

verify :: Signature -> Strict.ByteString -> Bool
verify (Signature signature) xs =
  Rsa.verify (Just SHA512) key xs signature
