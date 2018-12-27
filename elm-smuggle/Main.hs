{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import Prelude

import qualified Command
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Elm
import qualified Git
import qualified Options
import qualified Path
import qualified Path.IO
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.Info
import qualified System.IO as IO

import Control.Monad (guard, unless, when)
import Data.Bifunctor (bimap)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Options (Options(..))
import Path (Abs, Dir, Path, (</>))


main :: IO ()
main = do
    let streams = [IO.stdout, IO.stderr]
    traverse_ (`IO.hSetBuffering` IO.NoBuffering) streams
    traverse_ (`IO.hSetEncoding` IO.utf8)         streams

    Options { deps } <- Options.get
    when (null deps) $ do
        TextIO.putStrLn "Nothing to do!"
        Exit.exitSuccess

    git <- Git.command >>= \case
        Just cmd -> pure cmd
        Nothing  -> do
            TextIO.hPutStrLn IO.stderr "Error: git not found"
            Exit.exitFailure

    elm <- Elm.command >>= \case
        Just cmd -> pure cmd
        Nothing  -> do
            TextIO.hPutStrLn IO.stderr "Error: elm not found"
            Exit.exitFailure

    currentRegistry <- readElmPackageRegistry >>= \case
        Right registry -> pure registry
        Left  err      -> do
            TextIO.hPutStrLn IO.stderr
                             "Error: couldn't decode package registry\n"
            TextIO.hPutStrLn IO.stderr (Text.pack err)
            Exit.exitFailure

    TextIO.putStrLn "Starting downloads...\n"
    msgChan   <- Concurrent.newChan
    _         <- Concurrent.forkIO (downloadProjects msgChan git elm deps)
    installed <- installProjects msgChan =<< Elm.packageCacheDir
    let newRegistry = smugglePackages installed currentRegistry
    writeElmPackageRegistry newRegistry
    TextIO.putStrLn "\nDependencies ready!"


downloadProjects :: MsgChan -> Git.Command -> Elm.Command -> [Git.Url] -> IO ()
downloadProjects chan git elm deps =
    Path.IO.withSystemTempDir "elm-smuggle" $ \tmpDir -> do
        Async.mapConcurrently_ (downloadProject chan git elm tmpDir) deps
        Concurrent.writeChan chan Close


downloadProject
    :: MsgChan -> Git.Command -> Elm.Command -> Path Abs Dir -> Git.Url -> IO ()
downloadProject chan git elm tmpDir url = handleError $ do
    repoDir <- Path.IO.createTempDir tmpDir ""

    Git.clone git url repoDir >>= checkResult_ GitCloneError

    Path.IO.withCurrentDir repoDir $ do
        tags <- Git.tags git >>= checkResult GitTagsError
        for_ (tagsToVersions tags) $ handleError . \version -> do
            versionDir <- Path.IO.createTempDir repoDir ""

            Git.checkoutTo git versionDir (show version)
                >>= checkResult_ (GitCheckoutError version)

            Path.IO.withCurrentDir versionDir $ do
                elmJsonExists <- Path.IO.doesFileExist Elm.projectFile
                unless elmJsonExists
                    $ Exception.throwIO (MissingElmJson version)

                elmJson <- Elm.parseProject
                    <$> LBS.readFile (Path.fromRelFile Elm.projectFile)

                case elmJson of
                    Left err -> Exception.throwIO (BadElmJson version err)
                    Right (Elm.App _) ->
                        Exception.throwIO (IsApplication version)
                    Right (Elm.Package projectInfo) -> do
                        Elm.makeDocs elm
                            >>= checkResult_ (ElmMakeDocsError version)
                        sendProject chan versionDir projectInfo
  where
    checkResult_ :: (Int -> String -> Error) -> Command.Result -> IO ()
    checkResult_ description result = () <$ checkResult description result

    checkResult :: (Int -> String -> Error) -> Command.Result -> IO String
    checkResult mkError result = case Command.exit result of
        Exit.ExitSuccess -> pure (Command.stdout result)
        Exit.ExitFailure code ->
            Exception.throwIO $ mkError code (Command.stderr result)

    handleError :: IO () -> IO ()
    handleError action =
        action `Exception.catch` (Concurrent.writeChan chan . Error url)

    tagsToVersions :: String -> [Elm.PackageVersion]
    tagsToVersions = mapMaybe (tagToVersion . Text.strip . Text.pack) . lines
      where
        tagToVersion :: Text -> Maybe Elm.PackageVersion
        tagToVersion tag = do
            version <- Elm.packageVersionFromText tag
            guard (Elm.versionMajor version >= 1)
            pure version


data Error
    = GitCloneError Int String
    | GitTagsError Int String
    | GitCheckoutError Elm.PackageVersion Int String
    | ElmMakeDocsError Elm.PackageVersion Int String
    | MissingElmJson Elm.PackageVersion
    | BadElmJson Elm.PackageVersion String
    | IsApplication Elm.PackageVersion
    deriving (Show, Exception.Exception)


installProjects
    :: MsgChan -> Path Abs Dir -> IO (Map Elm.PackageName [Elm.PackageVersion])
installProjects chan cacheDir = Concurrent.readChan chan >>= \case
    Close -> pure Map.empty

    Error url err -> logError url err >> installProjects chan cacheDir

    Project absDir packageInfo -> do
        installDir <- mkInstallDir packageInfo
        Path.IO.copyDirRecur absDir installDir
        logInstalled packageInfo
        Map.insertWith (<>)
                       (Elm.packageName packageInfo)
                       [Elm.packageVersion packageInfo]
            <$> installProjects chan cacheDir
  where
    mkInstallDir :: Elm.PackageInfo -> IO (Path Abs Dir)
    mkInstallDir Elm.PackageInfo {..} = do
        let author  = Text.unpack . Elm.packageAuthor $ packageName
        let project = Text.unpack . Elm.packageProject $ packageName
        let version = Text.unpack . Elm.renderPackageVersion $ packageVersion

        authorDir  <- Path.parseRelDir author
        projectDir <- Path.parseRelDir project
        versionDir <- Path.parseRelDir version

        pure (cacheDir </> authorDir </> projectDir </> versionDir)

    logInstalled :: Elm.PackageInfo -> IO ()
    logInstalled Elm.PackageInfo {..} =
        TextIO.putStrLn
            $  indent
            <> green bullet
            <> " "
            <> Elm.renderPackageName packageName
            <> " "
            <> Elm.renderPackageVersion packageVersion

    logError :: Git.Url -> Error -> IO ()
    logError url err =
        TextIO.hPutStrLn IO.stderr
            $  indent
            <> red bullet
            <> " "
            <> Text.pack (dropSlash (Git.urlPath url))
            <> " "
            <> case err of
                   GitCloneError _ _ -> "git clone failed"

                   GitTagsError  _ _ -> "git tag failed"

                   GitCheckoutError version _ _ ->
                       Elm.renderPackageVersion version
                           <> " git checkout failed"

                   ElmMakeDocsError version _ _ ->
                       Elm.renderPackageVersion version
                           <> " error making elm docs"

                   MissingElmJson version ->
                       Elm.renderPackageVersion version <> " missing elm.json"

                   BadElmJson version _ ->
                       Elm.renderPackageVersion version
                           <> " error parsing elm.json"

                   IsApplication version ->
                       Elm.renderPackageVersion version <> " not a package"

    indent :: IsString s => s
    indent = "  "

    dropSlash :: String -> String
    dropSlash ('/' : rest) = rest
    dropSlash string       = string


type MsgChan = Concurrent.Chan Msg


data Msg
    = Project (Path Abs Dir) Elm.PackageInfo
    | Error Git.Url Error
    | Close


sendProject :: MsgChan -> Path Abs Dir -> Elm.PackageInfo -> IO ()
sendProject chan dir info = Concurrent.writeChan chan (Project dir info)


-- UTIL


readElmPackageRegistry :: IO (Either String Elm.PackageRegistry)
readElmPackageRegistry = do
    file   <- Elm.packageRegistryFile
    exists <- Path.IO.doesFileExist file
    if not exists
        then pure (Right Elm.emptyPackageRegistry)
        else decodeBinary . LBS.fromStrict <$> BS.readFile
            (Path.fromAbsFile file)


smugglePackages
    :: Map Elm.PackageName [Elm.PackageVersion]
    -> Elm.PackageRegistry
    -> Elm.PackageRegistry
smugglePackages packages registry = registry
    { Elm.registryPackages = Elm.registryPackages registry <> packages
    }
    -- NOTE: Deliberately not incrementing the registry size!
    --
    -- Elm invalidates the local registry by checking whether the remote registry
    -- is bigger. So if we incremented the size here the user would lose important
    -- updates. Fortunately the compiler doesn't actually _calculate_ the size
    -- (i.e. via `Map.size`) so we can just quietly insert things and all is well.


writeElmPackageRegistry :: Elm.PackageRegistry -> IO ()
writeElmPackageRegistry registry = do
    file <- Elm.packageRegistryFile
    let cacheDir = Path.parent file
    Path.IO.createDirIfMissing True cacheDir -- mkdir -p
    Binary.encodeFile (Path.fromAbsFile file) registry


decodeBinary :: Binary.Binary a => LBS.ByteString -> Either String a
decodeBinary = bimap third third . Binary.decodeOrFail
  where
    third :: (a, b, c) -> c
    third (_, _, c) = c


bullet :: IsString s => s
bullet = if isWindows then "+" else "●"


isWindows :: Bool
isWindows = System.Info.os == "mingw32"


green :: Text -> Text
green text = vividSGR ANSI.Green <> text <> resetSGR


red :: Text -> Text
red text = vividSGR ANSI.Red <> text <> resetSGR


vividSGR :: ANSI.Color -> Text
vividSGR color =
    Text.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]


resetSGR :: Text
resetSGR = Text.pack $ ANSI.setSGRCode [ANSI.Reset]
