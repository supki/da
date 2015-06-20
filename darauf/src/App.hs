{-# LANGUAGE NamedFieldPuns #-}
module App
  ( app
  ) where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit ((=$=), runConduit)

import           Config (Config(..))
import qualified Channel
import qualified Pretty
import qualified Status


app :: Config -> IO ()
app Config { configNixChannels } =
  runResourceT (runConduit (Channel.parse configNixChannels =$= Status.get =$= Pretty.print))

