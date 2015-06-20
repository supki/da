{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module Channel
  ( Channel(..)
  , Url
  , parse
  ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (Producer, (=$=))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http


type Url = Http.Request

data Channel = Channel
  { channelUrl  :: Url
  , channelName :: Text
  } deriving (Show)

parse :: MonadResource m => FilePath -> Producer m Channel
parse fp =
  CB.sourceFile fp =$= CT.decode CT.utf8 =$= CT.linesBounded 240 =$= CL.mapMaybe parseLine

parseLine :: Text -> Maybe Channel
parseLine str = do
  [url, channelName]
             <- pure (Text.words str)
  channelUrl <- parseUrl url
  return Channel { channelUrl, channelName }

parseUrl :: Text -> Maybe Url
parseUrl = Http.parseUrl . Text.unpack
