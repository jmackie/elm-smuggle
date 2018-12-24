{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main (main) where

import Prelude

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser, parseEither)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Elm
import qualified Options
import qualified Path
import qualified Path.IO
import qualified System.Console.ANSI as ANSI
import qualified System.Exit as Exit
import qualified System.Info
import qualified System.IO as IO
import qualified System.Process as Process

import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:?))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Options (Options)
import Path (Path, (</>))
import Text.Printf (printf)


main :: IO ()
main = do
    initStreams
    options <- Options.get
    result  <- runExceptT . runScript $ mainScript options
    case result of
        Left err -> do
            logError . red $ printError err
            Exit.exitFailure

        Right RegistryUpdated -> putStrLn "\nDependencies ready!"
        Right NothingToDo     -> putStrLn "Nothing to do!"


initStreams :: IO ()
initStreams =
    traverse_ (`IO.hSetEncoding` IO.utf8) [IO.stdin, IO.stdout, IO.stderr]


-- USER FEEDBACK


data Error
    -- | Working directory is missing an `elm.json`
    = NoElmJson
    -- | Error decoding `elm.json` to an Aeson.Value (malformed json?)
    | BadElmJson String
    -- | Error decoding `versions.dat`
    | BadPackageRegistry String
    -- | Git executable not found
    | GitNotFound
    -- | Error running a git command
    | GitError GitError
    -- | Error running `elm make --docs docs.json`
    | ElmMakeDocsError String
    -- | Handled IO exception
    | IOFailure IOFailure
    -- | Unexpected file path
    | BadPath String
    -- | Unhandled IO exception
    | UnhandledIOFailure Exception.SomeException


printError :: Error -> String
printError = \case
    NoElmJson               -> "There's no elm.json here!"
    BadElmJson         why  -> "Error decoding elm.json: " <> why
    BadPackageRegistry why  -> "Error decoding package registry: " <> why
    ElmMakeDocsError   why  -> "Error writing package documentation: " <> why
    GitNotFound             -> "Git not found on path"
    IOFailure          what -> "IO error: " <> printIOFailure what
    GitError           what -> "Git error: " <> printGitError what
    BadPath            why  -> "Unexpected path: " <> why
    UnhandledIOFailure what -> "Unhandled IO exception: " <> show what


data GitError
    -- | Error running `git clone`
    = CloneError String
    -- | Error running `git tags`
    | TagsError String
    -- | Error running `git checkout`
    | CheckoutError String


printGitError :: GitError -> String
printGitError = \case
    CloneError    why -> "clone failed: " <> why
    TagsError     why -> "tags failed: " <> why
    CheckoutError why -> "checkout failed: " <> why


data IOFailure = IOFail
    { ioFailureDescription :: String
    , ioFailureException   :: Exception.IOException
    }


printIOFailure :: IOFailure -> String
printIOFailure failure =
    ioFailureDescription failure <> "\n\n" <> show (ioFailureException failure)


-- SCRIPT


newtype Script a = Script { runScript :: ExceptT Error IO a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadCatch
        , MonadThrow
        , MonadMask
        , MonadError Error
        )

instance MonadIO Script where
    liftIO ioa = Script $ do
        result <- liftIO (Exception.try ioa)
        case result of
            -- Catch unhandled exceptions
            Left err -> throwError (UnhandledIOFailure err)
            Right a  -> pure a


data GitDependency = GitDependency
    { dependencyName :: Elm.PackageName
    , dependencyUrl  :: Url
    }


type Url = String -- passed to `git clone`


data Result
    = RegistryUpdated
    | NothingToDo


type AbsDir  = Path Path.Abs Path.Dir
type RelDir  = Path Path.Rel Path.Dir
type AbsFile = Path Path.Abs Path.File
type RelFile = Path Path.Rel Path.File


mainScript :: Options -> Script Result
mainScript _ = do
    depends <- readGitDependencies
    if null depends
        then pure NothingToDo
        else do
            checkForGit  -- we need git to be available
            currentRegistry <- readPackageRegistry
            logInfo "Starting downloads...\n"
            newRegistry <- addGitDependencies depends currentRegistry
            writePackageRegistry newRegistry
            pure RegistryUpdated


-- | Read the git-dependencies field of elm.json
readGitDependencies :: Script [GitDependency]
readGitDependencies = do
    elmJson <- readElmJson
    extractGitDependencies elmJson <?> BadElmJson


-- | Read elm.json as a raw json value.
readElmJson :: Script Aeson.Value
readElmJson = do
    elmJsonExists <- fileExists elmJsonPath
    unless elmJsonExists (throwError NoElmJson)

    bytes <- readBytes elmJsonPath
    Aeson.eitherDecode bytes <?> BadElmJson
  where
    elmJsonPath :: RelFile
    elmJsonPath = $(Path.mkRelFile "elm.json")


-- | Read the cached package registry.
readPackageRegistry :: Script Elm.PackageRegistry
readPackageRegistry = do
    file   <- Elm.packageRegistryFile
    exists <- fileExists file
    if not exists
        then pure Elm.emptyPackageRegistry
        else do
            bytes <- readBytes file
            decodeBinary bytes <?> BadPackageRegistry


writePackageRegistry :: Elm.PackageRegistry -> Script ()
writePackageRegistry registry = do
    file <- Elm.packageRegistryFile
    let cacheDir = Path.parent file
    Path.IO.createDirIfMissing True cacheDir
        <!?> printf "create %s if missing" (show cacheDir)
    Binary.encodeFile (Path.toFilePath file) registry
        <!?> "write package registry cache"


-- | Parse the `git-dependencies` field of elm.json
extractGitDependencies :: Aeson.Value -> Either String [GitDependency]
extractGitDependencies = Aeson.parseEither parser
  where
    parser :: Aeson.Value -> Aeson.Parser [GitDependency]
    parser value = do
        object <- Aeson.parseJSON value
        object .:? "git-dependencies" >>= \case
            Nothing   -> pure []
            Just deps -> traverse parseDependency (HashMap.toList deps)

    parseDependency :: (Text, Url) -> Aeson.Parser GitDependency
    parseDependency (key, url) = case Elm.packageNameFromText key of
        Nothing          -> fail ("invalid package name: " <> Text.unpack key)
        Just packageName -> pure (GitDependency packageName url)


addGitDependencies
    :: [GitDependency] -> Elm.PackageRegistry -> Script Elm.PackageRegistry
addGitDependencies []                 registry = pure registry
addGitDependencies (depend : depends) registry = do
    versions <- addGitDependency depend
    addGitDependencies depends $ registry
        { Elm.registryPackages = Map.insert (dependencyName depend)
                                            versions
                                            (Elm.registryPackages registry)
        }


addGitDependency :: GitDependency -> Script [Elm.PackageVersion]
addGitDependency GitDependency { dependencyName, dependencyUrl } =
    withTempDir "elm-smuggle" $ \repoDir -> do
        gitClone dependencyUrl repoDir
        logInfos
            [ " "
            , green bullet
            , show dependencyName
            , "cloned into"
            , yellow (Path.toFilePath repoDir)
            ]
        tags <- gitTags repoDir
        let versions = filter validVersion (tagsToVersions tags)
        traverse_ (addElmPackage repoDir dependencyName) versions
        pure versions
  where
    tagsToVersions :: [String] -> [Elm.PackageVersion]
    tagsToVersions = Maybe.mapMaybe (Elm.packageVersionFromText . Text.pack)

    validVersion :: Elm.PackageVersion -> Bool
    validVersion version = Elm.versionMajor version >= 1


addElmPackage :: AbsDir -> Elm.PackageName -> Elm.PackageVersion -> Script ()
addElmPackage repoDir packageName packageVersion = do
    let branch = show packageVersion
    gitCheckout repoDir branch
    targetDir <- targetDirectory
    rmDirIfExists targetDir
    copyDir repoDir targetDir
    elmMakeDocs targetDir
    logInfos [replicate 5 ' ', cyan arrow, "version", branch, "added"]
    cleanup targetDir
  where
    targetDirectory :: Script AbsDir
    targetDirectory = do
        packageCache <- Elm.packageCacheDir
        author <- parseRelDir (Text.unpack (Elm.packageAuthor packageName))
        project <- parseRelDir (Text.unpack (Elm.packageProject packageName))
        version <- parseRelDir (show packageVersion)
        pure (packageCache </> author </> project </> version)

    cleanup :: AbsDir -> Script ()
    cleanup dir = do
        rmDirIfExists (dir </> $(Path.mkRelDir ".git"))
        rmDirIfExists (dir </> $(Path.mkRelDir "elm-stuff"))


-- | Is `git` on the users path? Throw an error if not.
checkForGit :: Script ()
checkForGit = which $(Path.mkRelFile "git") >>= \case
    Nothing -> (throwError GitNotFound)
    Just _  -> pure ()


-- IO STUFF


-- | rm -rf
rmDir :: (MonadIO m, MonadError Error m) => Path b Path.Dir -> m ()
rmDir dir =
    Path.IO.removeDirRecur dir <!?> printf "remove directory %s" (show dir)


-- | rm -rf (only if directory exists)
rmDirIfExists :: (MonadIO m, MonadError Error m) => Path b Path.Dir -> m ()
rmDirIfExists dir = do
    exists <- dirExists dir
    when exists (rmDir dir)


which :: (MonadIO m, MonadError Error m) => RelFile -> m (Maybe AbsFile)
which exe =
    Path.IO.findExecutable exe <!?> printf "search for %s executable" (show exe)


dirExists :: (MonadIO m, MonadError Error m) => Path b Path.Dir -> m Bool
dirExists dir = Path.IO.doesDirExist dir
    <!?> printf "check if directory %s exists" (show dir)


fileExists :: (MonadIO m, MonadError Error m) => Path b Path.File -> m Bool
fileExists file =
    Path.IO.doesFileExist file <!?> printf "check if file %s exists" (show file)


readBytes :: (MonadIO m, MonadError Error m) => Path b Path.File -> m ByteString
readBytes file =
    ByteString.readFile filePath <!?> printf "read file %s" filePath
    where filePath = Path.toFilePath file


withTempDir :: (MonadIO m, MonadMask m) => String -> (AbsDir -> m a) -> m a
withTempDir = Path.IO.withSystemTempDir


copyDir
    :: (MonadIO m, MonadError Error m)
    => Path b Path.Dir
    -> Path b Path.Dir
    -> m ()
copyDir src dst = do
    Path.IO.createDirIfMissing True dst  -- mkdir -p
        <!?> printf "create %s if missing" (show dst)
    Path.IO.copyDirRecur src dst
        <!?> printf "recursively copy %s to %s" (show src) (show dst)


parseRelDir :: (MonadIO m, MonadError Error m) => FilePath -> m RelDir
parseRelDir filePath = maybe (throwError err) pure (Path.parseRelDir filePath)
  where
    err :: Error
    err = BadPath (filePath <> " is not a relative directory")


-- | Add exception handling to an IO action.
tryIO :: forall m a . (MonadIO m, MonadError Error m) => String -> IO a -> m a
tryIO description action =
    liftIO (Exception.try action) >>= either handleIOException pure
  where
    handleIOException :: Exception.IOException -> m a
    handleIOException = throwError . IOFailure . IOFail description


-- | `git clone`
gitClone :: (MonadIO m, MonadError Error m) => Url -> AbsDir -> m ()
gitClone url outputDir = do
    (exit, _, stderr) <- runProcess
        "git"
        ["clone", url, Path.fromAbsDir outputDir]

    case exit of
        Exit.ExitSuccess -> pure ()
        Exit.ExitFailure code ->
            throwError . GitError . CloneError $ describeExitFailure code stderr


-- | `git tag`
gitTags :: (MonadIO m, MonadError Error m) => AbsDir -> m [String]
gitTags repo = do
    (exit, stdout, stderr) <- runProcessWithin repo "git" ["tag"]
    case exit of
        Exit.ExitSuccess -> pure (lines stdout)
        Exit.ExitFailure code ->
            throwError . GitError . TagsError $ describeExitFailure code stderr


-- | `git checkout`
gitCheckout :: (MonadIO m, MonadError Error m) => AbsDir -> String -> m ()
gitCheckout repo branch = do
    (exit, _stdout, stderr) <- runProcessWithin repo "git" ["checkout", branch]
    case exit of
        Exit.ExitSuccess -> pure ()
        Exit.ExitFailure code ->
            throwError . GitError . CheckoutError $ describeExitFailure
                code
                stderr


-- | `elm make --docs docs.json`
elmMakeDocs :: (MonadIO m, MonadError Error m) => AbsDir -> m ()
elmMakeDocs project = do
    (exit, _, stderr) <- runProcessWithin project
                                          "elm"
                                          ["make", "--docs", "docs.json"]
    case exit of
        Exit.ExitSuccess -> pure ()
        Exit.ExitFailure code ->
            throwError . ElmMakeDocsError $ describeExitFailure code stderr


describeExitFailure :: Int -> String -> String
describeExitFailure code stderr =
    "non-zero exit code: " <> show code <> "\n\n" <> stderr


-- | Start a child process.
runProcess
    :: (MonadIO m, MonadError Error m)
    => String
    -> [String]
    -> m (Exit.ExitCode, String, String)
runProcess cmd args =
    Process.readCreateProcessWithExitCode createProcess ""
        <!?> printf "run %s %s" cmd (unwords args)
  where
    createProcess :: Process.CreateProcess
    createProcess = Process.proc cmd args


-- | Start a child process within a particular directory.
runProcessWithin
    :: (MonadIO m, MonadError Error m)
    => AbsDir
    -> String
    -> [String]
    -> m (Exit.ExitCode, String, String)
runProcessWithin cwd cmd args =
    Process.readCreateProcessWithExitCode createProcess ""
        <!?> printf "run %s %s (in %s)" cmd (unwords args) (Path.fromAbsDir cwd)
  where
    createProcess :: Process.CreateProcess
    createProcess =
        (Process.proc cmd args) { Process.cwd = Just (Path.fromAbsDir cwd) }


-- | Decode some binary data.
decodeBinary :: Binary.Binary a => ByteString -> Either String a
decodeBinary = bimap third third . Binary.decodeOrFail


-- | Log some helpful information.
logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn


-- | Log an error.
logError :: MonadIO m => String -> m ()
logError = liftIO . IO.hPutStrLn IO.stderr


logInfos :: MonadIO m => [String] -> m ()
logInfos = logInfo . unwords


-- | Add exception handling to an IO action.
(<!?>) :: (MonadIO m, MonadError Error m) => IO a -> String -> m a
(<!?>) = flip tryIO


-- | Lift an Either into MonadError.
(<?>) :: MonadError e m => Either e' a -> (e' -> e) -> m a
(<?>) ea f = either (throwError . f) pure ea


third :: (a, b, c) -> c
third (_, _, c) = c


-- PRETTY OUTPUT


bullet :: String
bullet = if windoze then "+" else "●"


arrow :: String
arrow = if windoze then "-" else "→"


cyan :: String -> String
cyan s =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
        <> s
        <> ANSI.setSGRCode [ANSI.Reset]


green :: String -> String
green s =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
        <> s
        <> ANSI.setSGRCode [ANSI.Reset]


yellow :: String -> String
yellow s =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
        <> s
        <> ANSI.setSGRCode [ANSI.Reset]


red :: String -> String
red s =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
        <> s
        <> ANSI.setSGRCode [ANSI.Reset]


windoze :: Bool
windoze = System.Info.os == "mingw32"
