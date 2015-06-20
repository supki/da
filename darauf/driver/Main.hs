module Main (main) where

import           App (app)
import qualified Config


main :: IO ()
main = app =<< Config.get
