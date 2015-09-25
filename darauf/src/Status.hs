{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Status
  ( Status(..)
  , Html
  , get
  , parse
  ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Conduit (Conduit, mapOutputMaybe)
import qualified Data.Conduit.List as CL
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import           Text.Xml.Lens

import           Channel (Channel(..))


type Html = Lazy.ByteString

data Status = Status
  { statusChannel :: Channel
  , statusCommit  :: Text
  , statusUpdate  :: Time.UTCTime
  } deriving (Show)

get :: MonadIO m => Conduit Channel m Status
get = mapOutputMaybe (uncurry parse) (CL.mapM (liftIO . go))
 where
  go channel@Channel { channelUrl } =
    fmap (\res -> (channel, Http.responseBody res))
         (Http.httpLbs channelUrl =<< Http.newManager Http.tlsManagerSettings)

parse :: Channel -> Html -> Maybe Status
parse statusChannel raw = do
  (header, timestamps) <- scrapeHtml raw
  statusCommit <- parseHeader header
  statusUpdate <- parseUpdate timestamps
  return Status { statusChannel, statusCommit, statusUpdate }

scrapeHtml :: Html -> Maybe (Text, [Text])
scrapeHtml raw = do
  header <- preview scrapeHeader raw
  timestamps <- nonempty (toListOf scrapeTimestamps raw)
  pure (header, timestamps)
 where
  nonempty [] = Nothing
  nonempty xs = Just xs

scrapeHeader :: Fold Html Text
scrapeHeader = html.folding universe.node "h1".text

scrapeTimestamps :: Fold Html Text
scrapeTimestamps =
  html.folding universe.node "td".attributed(ix "align".only "right").text.folding(Text.stripSuffix "  ")

parseHeader :: Text -> Maybe Text
parseHeader header =
  case Text.breakOnEnd "." header of
    (_, "")     -> Nothing     -- header ends with a dot
    ("", _)     -> Nothing     -- header does not include a dot
    (_, commit) -> Just commit

parseUpdate :: [Text] -> Maybe Time.UTCTime
parseUpdate = maximumOf (folded.folding parseTime)

parseTime :: Text -> Maybe Time.UTCTime
parseTime = Time.parseTimeM True Time.defaultTimeLocale "%d-%b-%Y %H:%M" . Text.unpack
