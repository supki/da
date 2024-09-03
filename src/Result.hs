{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Result
  ( Result(..)
  , parse
  ) where

import Control.Applicative (empty)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString, breakSubstring)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import Sign (Signature(..))


data Result = Result
  { passwords  :: [Text]
  , signature  :: Signature
  , signedData :: ByteString
  } deriving (Show, Eq)

parse :: Lazy.ByteString -> Either String Result
parse x =
  case Aeson.eitherDecode x of
    Left err ->
      Left err
    Right Result0 {passwords, signature} -> do
      let (_, y) = breakSubstring "{\"method\":" (Lazy.toStrict x)
          (z, _) = breakSubstring ",\"signature\":" y
      Right Result
        { passwords
        , signature
        , signedData = z
        }

data Result0 = Result0
  { passwords :: [Text]
  , signature :: Signature
  } deriving (Show, Eq)

instance Aeson.FromJSON Result0 where
  parseJSON (Aeson.Object o) = do
    r <- o .: "result"
    passwords <- (r .: "random") >>= \o' -> o' .: "data"
    x <- r .: "signature"
    case fmap Signature (Base64.decode (Text.encodeUtf8 x)) of
      Right signature ->
        pure Result0 {passwords, signature}
      _ ->
        empty
  parseJSON _ = empty
