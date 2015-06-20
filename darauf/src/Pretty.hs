{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Pretty
  ( print
  ) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Conduit (Sink, awaitForever)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Prelude hiding (print)

import           Channel (Channel(..))
import           Status (Status(..))


print :: MonadIO m => Sink Status m ()
print = do
  time <- liftIO getCurrentTime
  awaitForever $ \Status { statusChannel = Channel { channelName }, statusCommit, statusUpdate } -> do
    let diff = diffUTCTime time statusUpdate
    liftIO (Text.putStrLn ("‘" <> channelName <> "’ has been at " <>
                           statusCommit <> " " <> prettyDiff diff <>
                           " (since " <> prettyUpdate statusUpdate <> ")"))
prettyUpdate :: UTCTime -> Text
prettyUpdate = fromString . show

prettyDiff :: NominalDiffTime -> Text
prettyDiff diff
  | floor weeks   > 1 = "for " <> text weeks   <> " weeks"
  | floor days    > 1 = "for " <> text days    <> " days"
  | floor hours   > 1 = "for " <> text hours   <> " hours"
  | floor minutes > 1 = "for " <> text minutes <> " minutes"
  | otherwise         = "for " <> text seconds <> " seconds"
 where
  text    = fromString . show . floor
  weeks   = days    / 7
  days    = hours   / 24
  hours   = minutes / 60
  minutes = diff    / 60
  seconds = diff
