{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative ((<|>), some, optional)
import           Control.Monad (when, unless)
import           Data.Foldable (asum)
import           Env (header, switch, help)
import qualified Env
import           Prelude hiding ((**))
import qualified System.Directory as D
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath (combine, (</>))

import           Damit
import           Damit.Route.Extra (int, string)


data Conf = Conf
  { verbose :: Bool
  , dryRun  :: Bool
  }

getConf :: IO Conf
getConf = Env.parse (header "damit") $ Conf
  <$> switch "V" (help "Print the command before running it")
  <*> switch "D" (help "Only check the routing")

main :: IO ()
main = do
  xs   <- getArgs
  conf <- getConf
  res  <- Damit.route xs $ do
    name <- input
    asum
      [ with ("git" *> fmap (combine "git") string) $ \repo -> do
          home <- D.getHomeDirectory
          y    <- D.doesDirectoryExist repo
          return (myTmux name (directory (home </> (if y then repo else "git"))))

      , with ("svn" *> fmap (combine "svn") string) $ \repo -> do
          home <- D.getHomeDirectory
          y    <- D.doesDirectoryExist repo
          return (myTmux name (directory (home </> (if y then repo else "svn"))))

      , with ("play" *> string) $ \bucket -> do
          home <- D.getHomeDirectory
          let playground = home </> "playground" </> bucket
          D.createDirectoryIfMissing True playground
          return (myTmux name (directory playground))

      , with ("tt" *> "slave" *> int ** optional string) $ \(n, l) ->
          pure (hop (ns "tt" ("slave" ++ show n))
                    (foldMap login l)
                    (myTmux "main"
                            (env ["TERM" .= "screen-256color"])))

      , with ("tt" *> string ** optional string) $ \(host, l) ->
          pure (hop (ns "tt" host)
                    (foldMap login l)
                    (run "$SHELL"
                         (args ["-l"])))

      , with ("al" *> string ** optional string) $ \(host, l) ->
          pure (hop (ns "al" host)
                    (foldMap login l)
                    (run "$SHELL"
                         (env ["TERM" .= "screen-256color"])))

      , with ("dive" *> "work" *> some string) $ \session ->
          pure (hop "737dd1bc" mempty (run "damit" (args session)))

      , with ("dive" *> "ko" *> optional string) $ \bucket ->
          pure (maybe (hop "kolyskovi" mempty (myTmux "main" mempty))
                      (\b -> hop "kolyskovi" mempty (myTmux b (directory ("work" </> b))))
                      bucket)

      , with_ ("dive" *> "mail") $
          pure (run "dive-into-mail" mempty)

      , with ("package.nix" *> (string <|> pure ".")) $ \dir ->
          pure (run "nix-shell" (args ["-p", "cabal2nix", "--run", "cabal2nix " ++ dir ++ " > package.nix"]))

      , with_ pop $
          pure (myTmux name mempty)
      ]
  case res of
    Nothing -> exitFailure
    Just c  -> do
      when (verbose conf) $
        putStrLn (pretty c)
      unless (dryRun conf) $
        exec c

ns :: String -> String -> String
ns p h = p ++ ":" ++ h

input :: RouteT String IO String
input = fmap unwords inputs

hops :: [String] -> Command -> Command
hops hs c = foldr (\h -> hop h mempty) c hs
{-# ANN hops ("HLint: ignore Avoid lambda" :: String) #-}

myTmux :: String -> Mod TmuxOptions -> Command
#ifdef __CUSTOM_SOCKET__
myTmux s m = tmux s (mappend (socket __CUSTOM_SOCKET__) m)
#else
myTmux = tmux
#endif
