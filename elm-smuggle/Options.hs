{-# LANGUAGE TemplateHaskell #-}
module Options
    ( Options(Options, deps, quiet)
    , defaults
    , get
    )
where

import Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Version as Version
import qualified Git
import qualified Language.Haskell.TH as TH
import qualified Path
import qualified Path.IO
import qualified Paths_elm_smuggle as CabalFile
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Path (File, Path, Rel)


-- | Program options.
data Options = Options
    { deps  :: [Git.Url]
    , quiet :: Bool
    }


-- | Default options.
defaults :: Options
defaults = Options [] False


addDeps :: [Git.Url] -> Options -> Options
addDeps urls opts = opts { deps = deps opts <> urls }


-- | Get options from the commadline.
get :: MonadIO m => m Options
get = liftIO $ do
    args <- parseArgs <$> Environment.getArgs
    case resolveMode args of
        Help          -> printUsage Exit.ExitSuccess
        PrintVersion  -> printVersion
        BadUsage why  -> badUsage why
        RunWith  opts -> do
            hasDotFile <- Path.IO.doesFileExist dotfile
            if hasDotFile
                then do
                    f    <- TextIO.readFile (Path.fromRelFile dotfile)
                    urls <- either (badUsage . BadDotfile) pure (parseDotfile f)
                    pure (addDeps urls opts)
                else pure opts
  where
    badUsage :: Problem -> IO a
    badUsage problem = do
        IO.hPutStrLn IO.stderr ("Oops: " <> describeProblem problem)
        putStrLn ""
        printUsage (Exit.ExitFailure 1)

    printUsage :: Exit.ExitCode -> IO a
    printUsage exitCode = do
        putStrLn usage
        Exit.exitWith exitCode

    printVersion :: IO a
    printVersion = do
        putStrLn ("elm-smuggle " <> Version.showVersion CabalFile.version)
        Exit.exitSuccess


usage :: String
usage =
    $(TH.runIO $ do
         let usageTxt = "usage.txt"
         installed <- CabalFile.getDataFileName usageTxt
         TH.LitE . TH.StringL <$> (readFile installed <|> readFile "usage.txt")
         --                                 ^^^^^^^^^
         --                          NOTE: doesn't exist in dev
     )


data Mode
    = RunWith Options
    | Help
    | PrintVersion
    | BadUsage Problem


resolveMode :: [Arg] -> Mode
resolveMode = go defaults
  where
    go :: Options -> [Arg] -> Mode
    go options []                      = RunWith options
    go _       (HelpFlag       : _   ) = Help
    go _       (VersionFlag    : _   ) = PrintVersion
    go _       (Unknown arg    : _   ) = BadUsage (UnknownArg arg)
    go options (QuietFlag      : rest) = go options { quiet = True } rest
    go options (Positional str : rest) = case Git.parseUrl str of
        Nothing  -> BadUsage (BadURI str)
        Just uri -> go (addDeps [uri] options) rest


data Problem
    = UnknownArg String
    | BadURI String
    | BadDotfile String


describeProblem :: Problem -> String
describeProblem (UnknownArg str) = "unknown argument " <> str
describeProblem (BadURI str)     = "bad url " <> str
describeProblem (BadDotfile err) = "error parsing dotfile: " <> err


data Arg
    = Positional String
    | HelpFlag            -- ^ -h|--help
    | VersionFlag         -- ^ -v|--version
    | QuietFlag           -- ^ -q|--quiet
    | Unknown String


parseArgs :: [String] -> [Arg]
parseArgs = Maybe.mapMaybe parseArg


parseArg :: String -> Maybe Arg
parseArg ""                = Nothing

parseArg "--help"          = Just HelpFlag
parseArg "-h"              = Just HelpFlag

parseArg "--version"       = Just VersionFlag
parseArg "-v"              = Just VersionFlag

parseArg unknown@('-' : _) = Just (Unknown unknown)
parseArg positional        = Just (Positional positional)


dotfile :: Path Rel File
dotfile = $(Path.mkRelFile ".elm-smuggle")


parseDotfile :: Text -> Either String [Git.Url]
parseDotfile = traverse (parseLine . Text.unpack . Text.strip) . Text.lines
  where
    parseLine :: String -> Either String Git.Url
    parseLine line = case Git.parseUrl line of
        Nothing  -> Left ("bad url: " <> line)
        Just url -> Right url
