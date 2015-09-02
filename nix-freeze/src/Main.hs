{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import           Data.Maybe (listToMaybe)
import           Data.Version (showVersion)
import qualified Distribution.Text as Cabal
import qualified Distribution.Package as Cabal
import           System.Environment (lookupEnv)
import           System.Exit (die)
import           System.Process (readProcess)
import           Text.Printf (printf)


main :: IO ()
main =
  either die (putStrLn . renderCabalConfig) =<< listInstalledPackages =<< nixGhcPkg

nixGhcPkg :: IO FilePath
nixGhcPkg =
  maybe (die msg) return =<< lookupEnv "NIX_GHCPKG"
 where
  msg = "Not inside the Nix shell of a Haskell project, exiting.."

data Package = Package
  { name, version :: String
  } deriving (Show, Eq)

listInstalledPackages :: FilePath -> IO (Either String [Package])
listInstalledPackages ghcPkg =
  fmap parsePackages (readProcess ghcPkg ["list"] "")

-- | Parse packages' names and versions from ghc-pkg's output.
--
-- The first line of the output is the path to the package db, so we skip it.  The lines
-- with name-version pairs start with leading spaces, so we strip them.  The output ends
-- with the empty line, so we filter it out.
parsePackages :: String -> Either String [Package]
parsePackages =
  traverse parsePackage .
  filter (maybe False Char.isAlpha . listToMaybe) .
  map (dropWhile Char.isSpace) . drop 1 . lines

parsePackage :: String -> Either String Package
parsePackage str =
  maybe (Left msg) (Right . fromPackageIdentifier) (Cabal.simpleParse str)
 where
  fromPackageIdentifier Cabal.PackageIdentifier { Cabal.pkgName = Cabal.PackageName {Cabal.unPackageName}
                                                , Cabal.pkgVersion
                                                } =
      Package {name=unPackageName, version=showVersion pkgVersion}
  msg = printf "Cannot parse ‘%s’ as a name-version pair" str

renderCabalConfig :: [Package] -> String
renderCabalConfig =
  List.intercalate ",\n" . zipWith (++) (field : repeat (replicate (length field) ' ')) . map renderPackage
 where
  field = "constraints: "

renderPackage :: Package -> String
renderPackage Package {name, version} =
  printf "%s ==%s" name version
