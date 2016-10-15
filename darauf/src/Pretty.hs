{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Pretty
  ( print
  ) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Conduit (Sink, awaitForever)
import           Prelude hiding (print)
import           Text.Printf (printf)

import           Channel (Channel(..))
import           Status (Status(..))


print :: MonadIO m => Sink Status m ()
print =
  awaitForever $ \Status {statusChannel=Channel {channelName}, ..} -> liftIO $
    putStrLn (printf "‘%s’ points to %s, %s" channelName statusId statusReleaseData)
