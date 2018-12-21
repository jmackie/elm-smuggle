{-# LANGUAGE TemplateHaskell #-}
module Options
    ( Options(..)
    , defaults
    , get
    )
where

import Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Text.IO as TextIO
import qualified Data.Version as Version
import qualified Language.Haskell.TH as TH
import qualified Network.URI as URI
import qualified Path
import qualified Path.IO
import qualified Paths_elm_smuggle as CabalFile
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import Data.Text (Text)
import Network.URI (URI)
import Path (Path)


-- | Program options.
data Options = Options
    { optProjects :: [URI]
    , optQuiet    :: Bool
    }


-- | Default options.
defaults :: Options
defaults = Options [] False


addProjects :: [URI] -> Options -> Options
addProjects uris opts = opts { optProjects = optProjects opts <> uris }


-- | Get options from the commadline.
get :: IO Options
get = do
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
                    uris <- either (badUsage . BadDotfile) pure (parseDotfile f)
                    pure (addProjects uris opts)
                else pure opts
  where
    badUsage :: Problem -> IO a
    badUsage problem = do
        IO.hPutStrLn IO.stderr ("Oops: " <> printProblem problem)
        putStrLn ""
        printUsage (Exit.ExitFailure 1)

    printProblem :: Problem -> String
    printProblem (UnknownArg str) = "unknown argument " <> str
    printProblem (BadURI str)     = "bad url " <> str
    printProblem (BadDotfile err) = "error parsing dotfile: " <> err

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
    $(TH.runIO $ TH.LitE . TH.StringL <$> readFile "usage.txt")


data Mode
    = RunWith Options
    | Help
    | PrintVersion
    | BadUsage Problem


data Problem
    = UnknownArg String
    | BadURI String
    | BadDotfile String


resolveMode :: [Arg] -> Mode
resolveMode = go defaults
  where
    go :: Options -> [Arg] -> Mode
    go options []                      = RunWith options
    go _       (HelpFlag       : _   ) = Help
    go _       (VersionFlag    : _   ) = PrintVersion
    go _       (Unknown arg    : _   ) = BadUsage (UnknownArg arg)
    go options (QuietFlag      : rest) = go options { optQuiet = True } rest
    go options (Positional str : rest) = case URI.parseURI str of
        Nothing  -> BadUsage (BadURI str)
        Just uri -> go (addProjects [uri] options) rest

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


dotfile :: Path Path.Rel Path.File
dotfile = $(Path.mkRelFile ".elm-smuggle")


-- | TODO
parseDotfile :: Text -> Either String [URI]
parseDotfile = undefined
