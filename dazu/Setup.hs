{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Version (showVersion)
import           Distribution.Package (PackageIdentifier(..), PackageName(..))
import           Distribution.PackageDescription (PackageDescription(package))
import           Distribution.Simple (defaultMainWithHooks, simpleUserHooks, buildHook)
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr)
import           Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import           Distribution.Simple.Utils (notice, rewriteFile)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>))
import           System.IO.Error (catchIOError)
import           System.Process (readProcess)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi uh bf -> do generateMeta lbi bf; buildHook simpleUserHooks pd lbi uh bf
  }

generateMeta :: LocalBuildInfo -> BuildFlags -> IO ()
generateMeta lbi bf = let
    verbosity = fromFlag (buildVerbosity bf)
    PackageIdentifier {pkgName = PackageName {unPackageName}, pkgVersion} = package (localPkgDescr lbi)
    metaName = "Meta_" ++ unPackageName
    metaPath = autogen </> metaName <.> "hs"
  in do
    hash <- gitHash
    createDirectoryIfMissing True autogen
    notice verbosity ("Generating " ++ metaPath ++ " ...")
    rewriteFile metaPath (unlines
      [ "module " ++ metaName
      , "  ( name"
      , "  , version"
      , "  , hash"
      , "  ) where"
      , ""
      , "import Data.String (IsString(fromString))"
      , ""
      , "name :: IsString str => str"
      , "name = fromString " ++ show unPackageName
      , ""
      , "version :: IsString str => str"
      , "version = fromString " ++ show (showVersion pkgVersion)
      , ""
      , "hash :: IsString str => str"
      , "hash = fromString " ++ show hash
      ])
 where
  autogen = autogenModulesDir lbi

gitHash :: IO String
gitHash =
  catchIOError (fmap sanitize (readProcess "git" ["describe", "--always", "--dirty=-dirty"] ""))
               (\_ -> return "unknown")
 where
  sanitize = List.dropWhileEnd Char.isSpace
