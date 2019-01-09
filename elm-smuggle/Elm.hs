{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
--
-- Mostly borrowed logic from https://github.com/elm/compiler
--
module Elm
    ( Command
    , command
    , makeDocs
    , CompilerVersion(Elm018, Elm019, UnknownCompilerVersion)
    , compilerVersion

    -- * Project manifest
    , Project(App, Package)
    , AppInfo
    , PackageInfo
        ( PackageInfo
        , packageName
        , packageVersion
        )
    , projectFile
    , parseProject

    -- * Package registry
    , PackageRegistry
        ( PackageRegistry
        , registrySize
        , registryPackages
        )
    , emptyPackageRegistry

    -- * Basic types
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

import qualified Command
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Path
import qualified Path.IO
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import Control.Applicative (liftA2, liftA3, (<|>))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:))
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word16)
import Path (Abs, Dir, File, Path, Rel, (</>))
import Text.Read (readMaybe)


-- | elm
newtype Command = Command { _unwrapCommand :: Command.Command }


command :: MonadIO m => m (Maybe Command)
command = fmap Command <$> Command.which $(Path.mkRelFile "elm")


-- NOTE: Running this may require package downloads!
makeDocs :: (MonadIO m, MonadCatch m) => Command -> m Command.Result
makeDocs (Command cmd) = Command.run cmd ["make", "--docs=docs.json"]
    -- This command should only serve to generate a docs.json file,
    -- other artifacts should be removed
    <* Path.IO.ignoringAbsence (Path.IO.removeDirRecur elmStuff)


data CompilerVersion
    = Elm018
    | Elm019
    | UnknownCompilerVersion String


compilerVersion :: MonadIO m => Command -> m CompilerVersion
compilerVersion (Command cmd) = do
    result <- Command.run cmd ["--version"]
    when (Command.exit result /= Exit.ExitSuccess)
        $ fail "error getting elm compiler version"
    case split '.' (Command.stdout result) of
        ("0" : "18" : _) -> pure Elm018
        ("0" : "19" : _) -> pure Elm019
        _ -> pure (UnknownCompilerVersion $ Command.stdout result)


split :: Char -> String -> [String]
split char s = case dropWhile (== char) s of
    "" -> []
    s' -> part : split char s'' where (part, s'') = break (== char) s'


data Project
    = App AppInfo
    | Package PackageInfo


projectFile :: Path Rel File
projectFile = $(Path.mkRelFile "elm.json")


parseProject :: LBS.ByteString -> Either String Project
parseProject = Aeson.eitherDecode'
    >=> Aeson.parseEither (liftA2 (<|>) parseApp parsePackage)
  where
    parseApp :: Aeson.Object -> Aeson.Parser Project
    parseApp object = do
        ty <- object .: "type"
        unless (ty == application) (fail "not an application project")
        pure (App ())

    parsePackage :: Aeson.Object -> Aeson.Parser Project
    parsePackage object = do
        ty <- object .: "type"
        unless (ty == package) (fail "not a package project")
        packageName    <- object .: "name"
        packageVersion <- object .: "version"
        pure (Package PackageInfo {..})

    application :: Text
    application = "application"

    package :: Text
    package = "package"


data PackageInfo = PackageInfo
    { packageName    :: PackageName
    , packageVersion :: PackageVersion
    }


type AppInfo = () -- not interested in applications here


-- | https://github.com/elm/compiler/blob/master/builder/src/Deps/Cache.hs
data PackageRegistry = PackageRegistry
    { registrySize     :: Int
    , registryPackages :: Map PackageName [PackageVersion]
    }


emptyPackageRegistry :: PackageRegistry
emptyPackageRegistry = PackageRegistry 0 Map.empty


instance Binary PackageRegistry where
    get = liftA2 PackageRegistry Binary.get Binary.get
    put PackageRegistry {..} = do
        Binary.put registrySize
        Binary.put registryPackages


-- | https://github.com/elm/compiler/blob/master/compiler/src/Elm/Package.hs
data PackageName = PackageName
    { packageAuthor  :: !Text  -- ^ e.g. "elm"
    , packageProject :: !Text  -- ^ e.g. "core"
    } deriving (Eq, Ord)


instance Aeson.FromJSON PackageName where
    parseJSON = Aeson.parseJSON >=>
        maybe (fail "bad package name") pure . packageNameFromText


packageNameFromText :: Text -> Maybe PackageName
packageNameFromText text = case Text.splitOn "/" text of
    [author, project] | not (Text.null author || Text.null project) ->
        Just (PackageName author project)
    _ -> Nothing


renderPackageName :: PackageName -> Text
renderPackageName PackageName {..} = packageAuthor <> "/" <> packageProject


instance Binary PackageName where
    get = liftA2 PackageName Binary.get Binary.get
    put PackageName {..} = do
        Binary.put packageAuthor
        Binary.put packageProject


-- | https://github.com/elm/compiler/blob/master/compiler/src/Elm/Package.hs
data PackageVersion = PackageVersion
    { versionMajor :: {-# UNPACK #-} !Word16
    , versionMinor :: {-# UNPACK #-} !Word16
    , versionPatch :: {-# UNPACK #-} !Word16
    } deriving (Eq, Ord)


instance Show PackageVersion where
    show = Text.unpack . renderPackageVersion


instance Aeson.FromJSON PackageVersion where
    parseJSON = Aeson.parseJSON >=>
        maybe (fail "bad package version") pure . packageVersionFromText


instance Binary PackageVersion where
    get = getPackageVersion
    put = putPackageVersion


getPackageVersion :: Binary.Get PackageVersion
getPackageVersion = do
    word <- Binary.getWord8
    if word == 0
        then liftA3 PackageVersion Binary.get Binary.get Binary.get
        else do
            minor <- fmap fromIntegral Binary.getWord8
            patch <- fmap fromIntegral Binary.getWord8
            pure (PackageVersion (fromIntegral word) minor patch)


putPackageVersion :: PackageVersion -> Binary.Put
putPackageVersion PackageVersion {..}
    | all fitsInWord8 [versionMajor, versionMinor, versionPatch] = do
        Binary.putWord8 (fromIntegral versionMajor)
        Binary.putWord8 (fromIntegral versionMinor)
        Binary.putWord8 (fromIntegral versionPatch)
    | otherwise = do
        Binary.putWord8 0  -- NOTE: can't have major versions < 0 !
        Binary.put versionMajor
        Binary.put versionMinor
        Binary.put versionPatch
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
--
-- (https://github.com/elm/compiler/blob/master/builder/src/Elm/PerUserCache.hs)


packageRegistryFile :: MonadIO m => m (Path Abs File)
packageRegistryFile = do
    packageCache <- packageCacheDir
    pure (packageCache </> versions)
  where
    versions :: Path Rel File
    versions = $(Path.mkRelFile "versions.dat")


packageCacheDir :: MonadIO m => m (Path Abs Dir)
packageCacheDir = rootDir $(Path.mkRelDir "package")


rootDir :: MonadIO m => Path Rel Dir -> m (Path Abs Dir)
rootDir dir = do
    elmHome <- elmHomeDir
    pure (elmHome </> $(Path.mkRelDir "0.19.0") </> dir)


elmHomeDir :: MonadIO m => m (Path Abs Dir)
elmHomeDir = liftIO $ do
    elmHome <- Environment.lookupEnv "ELM_HOME"
    maybe (Path.IO.getAppUserDataDir "elm") Path.parseAbsDir elmHome


elmStuff :: Path Rel Dir
elmStuff = $(Path.mkRelDir "elm-stuff")
