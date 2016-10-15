{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Status
  ( Status(..)
  , get
  , parse
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Conduit (Conduit, mapOutputMaybe)
import qualified Data.Conduit.List as CL
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import           Text.Xml.Lens

import           Channel (Channel(..))


data Status = Status
  { statusChannel     :: !Channel
  , statusId          :: !Text
  , statusReleaseData :: !Text
  } deriving (Show)

get :: MonadIO m => Conduit Channel m Status
get =
  mapOutputMaybe (uncurry parse) (CL.mapM (liftIO . go))
 where
  go channel@Channel {channelUrl} = do
    man <- Http.newManager Http.tlsManagerSettings
    body <- fmap Http.responseBody (Http.httpLbs channelUrl man)
    pure (channel, body)

parse :: Channel -> Lazy.ByteString -> Maybe Status
parse statusChannel raw = do
  (statusId, statusReleaseData) <- scrapeHtml raw
  return Status {..}

scrapeHtml :: Lazy.ByteString -> Maybe (Text, Text)
scrapeHtml raw = do
  header <- preview scrapeHeader raw
  let releaseData = view scrapeReleaseData raw
  pure (header, releaseData)

scrapeHeader :: AsHtmlDocument doc => Fold doc Text
scrapeHeader =
  html.folding universe.node "h1".text.folding (listToMaybe . drop 2 . Text.words)

scrapeReleaseData :: AsHtmlDocument doc => Fold doc Text
scrapeReleaseData =
  html.folding universe.node "p".texts
