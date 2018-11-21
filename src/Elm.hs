{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Elm
    ( PackageRegistry
        ( PackageRegistry
        , registrySize
        , registryPackages
        )
    , emptyPackageRegistry

    , PackageName
        ( PackageName
        , packageAuthor
        , packageProject
        )
    , packageNameFromText
    , renderPackageName

    , PackageVersion
        ( PackageVersion
        , versionMajor
        , versionMinor
        , versionPatch
        )
    , packageVersionFromText
    , renderPackageVersion

    -- * Paths
    , packageRegistryFile
    , packageCacheDir
    )
where

import Prelude

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Path
import qualified System.Directory as Directory
import qualified System.Environment as Environment

import Control.Applicative (liftA2, liftA3)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary, Get, Put, get, getWord8, put, putWord8)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word16)
import Path (Path, (</>))
import Text.Read (readMaybe)


data PackageRegistry = PackageRegistry
    { registrySize     :: Int
    , registryPackages :: Map PackageName [PackageVersion]
    }


emptyPackageRegistry :: PackageRegistry
emptyPackageRegistry = PackageRegistry 0 Map.empty


instance Binary PackageRegistry where
    get = getPackageRegistry
    put = putPackageRegistry


getPackageRegistry :: Get PackageRegistry
getPackageRegistry = liftA2 PackageRegistry get get


putPackageRegistry :: PackageRegistry -> Put
putPackageRegistry PackageRegistry {..} = do
    put registrySize
    put registryPackages


data PackageName = PackageName
    { packageAuthor  :: !Text  -- ^ e.g. "elm"
    , packageProject :: !Text  -- ^ e.g. "core"
    } deriving (Eq, Ord)


instance Show PackageName where
    show = Text.unpack . renderPackageName


packageNameFromText :: Text -> Maybe PackageName
packageNameFromText text = case Text.splitOn "/" text of
    [author, project] | not (Text.null author || Text.null project) ->
        Just (PackageName author project)
    _ -> Nothing


renderPackageName :: PackageName -> Text
renderPackageName PackageName {..} = packageAuthor <> "/" <> packageProject


instance Binary PackageName where
    get = getPackageName
    put = putPackageName


getPackageName :: Get PackageName
getPackageName = liftA2 PackageName get get


putPackageName :: PackageName -> Put
putPackageName packageName = do
    put (packageAuthor packageName)
    put (packageProject packageName)


data PackageVersion = PackageVersion
    { versionMajor :: {-# UNPACK #-} !Word16
    , versionMinor :: {-# UNPACK #-} !Word16
    , versionPatch :: {-# UNPACK #-} !Word16
    } deriving (Eq, Ord)


instance Show PackageVersion where
    show = Text.unpack . renderPackageVersion


instance Binary PackageVersion where
    get = getPackageVersion
    put = putPackageVersion


getPackageVersion :: Get PackageVersion
getPackageVersion = do
    word <- getWord8
    if word == 0
        then liftA3 PackageVersion get get get
        else do
            minor <- fmap fromIntegral getWord8
            patch <- fmap fromIntegral getWord8
            pure (PackageVersion (fromIntegral word) minor patch)


putPackageVersion :: PackageVersion -> Put
putPackageVersion PackageVersion {..}
    | all fitsInWord8 [versionMajor, versionMinor, versionPatch] = do
        putWord8 (fromIntegral versionMajor)
        putWord8 (fromIntegral versionMinor)
        putWord8 (fromIntegral versionPatch)
    | otherwise = do
        putWord8 0  -- NOTE: can't have major versions < 0 !
        put versionMajor
        put versionMinor
        put versionPatch
  where
    fitsInWord8 :: Word16 -> Bool
    fitsInWord8 = (< 256)


packageVersionFromText :: Text -> Maybe PackageVersion
packageVersionFromText text = case Text.splitOn "." text of
    [major, minor, patch] ->
        PackageVersion
            <$> readMaybe (Text.unpack major)
            <*> readMaybe (Text.unpack minor)
            <*> readMaybe (Text.unpack patch)
    _ -> Nothing


renderPackageVersion :: PackageVersion -> Text
renderPackageVersion PackageVersion {..} =
    Text.pack
        $  show versionMajor
        <> "."
        <> show versionMinor
        <> "."
        <> show versionPatch


-- PATHS


packageRegistryFile :: MonadIO m => m (Path Path.Abs Path.File)
packageRegistryFile = do
    packageCache <- packageCacheDir
    pure (packageCache </> versions)
  where
    versions :: Path Path.Rel Path.File
    versions = $(Path.mkRelFile "versions.dat")


packageCacheDir :: MonadIO m => m (Path Path.Abs Path.Dir)
packageCacheDir = rootDir $(Path.mkRelDir "package")


rootDir :: MonadIO m => Path Path.Rel Path.Dir -> m (Path Path.Abs Path.Dir)
rootDir dir = do
    elmHome <- elmHomeDir
    pure (elmHome </> compilerVersion </> dir)
  where
    compilerVersion :: Path Path.Rel Path.Dir
    compilerVersion = $(Path.mkRelDir "0.19.0")


elmHomeDir :: MonadIO m => m (Path Path.Abs Path.Dir)
elmHomeDir = liftIO elmHomeDir'


elmHomeDir' :: IO (Path Path.Abs Path.Dir)
elmHomeDir' =
    Environment.lookupEnv "ELM_HOME"
        >>= maybe (Directory.getAppUserDataDirectory "elm") pure
        >>= Path.parseAbsDir
