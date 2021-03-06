{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Options
    ( Options(Options, deps, quiet, suppressErrors, reinstall)
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
import qualified Elm
import qualified Path.IO
import qualified Paths_elm_smuggle as CabalFile
import qualified System.Console.ANSI as ANSI
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Path (File, Path, Rel)


-- | Program options.
data Options = Options
    { deps           :: [(Git.Url, Maybe Elm.Constraint)]
    , quiet          :: Bool
    , suppressErrors :: Bool
    , reinstall      :: Bool
    }


-- | Default options.
defaults :: Options
defaults = Options
    { deps           = []
    , quiet          = False
    , suppressErrors = False
    , reinstall      = False
    }


addDeps :: [(Git.Url, Maybe Elm.Constraint)] -> Options -> Options
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
        TextIO.hPutStrLn IO.stderr
            .  red
            .  Text.pack
            $  "Oops: "
            <> describeProblem problem
        TextIO.putStrLn ""
        printUsage (Exit.ExitFailure 1)

    printUsage :: Exit.ExitCode -> IO a
    printUsage exitCode = do
        TextIO.putStrLn (Text.pack usage)
        Exit.exitWith exitCode

    printVersion :: IO a
    printVersion = do
        TextIO.putStrLn . Text.pack $ Version.showVersion CabalFile.version
        Exit.exitSuccess


usage :: String
usage =
    $(TH.runIO $ do
        let usageTxt = "usage.txt" :: FilePath
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
    go options []                   = RunWith options
    go _       (HelpFlag    : _   ) = Help
    go _       (VersionFlag : _   ) = PrintVersion
    go _       (Unknown arg : _   ) = BadUsage (UnknownArg arg)
    go options (QuietFlag   : rest) = go options { quiet = True } rest
    go options (SuppressErrorsFlag : rest) =
        go options { suppressErrors = True } rest
    go options (ReinstallFlag  : rest) = go options { reinstall = True } rest
    go options (Positional str : rest) = case Git.parseUrl str of
        Nothing  -> BadUsage (BadURI str)
        Just uri -> go (addDeps [(uri, Nothing)] options) rest


data Problem
    = UnknownArg String
    | BadURI String
    | BadDotfile String


describeProblem :: Problem -> String
describeProblem (UnknownArg str) = "unknown argument " <> str
describeProblem (BadURI     str) = "bad url " <> str
describeProblem (BadDotfile err) = "error parsing dotfile: " <> err


data Arg
    = Positional String
    | HelpFlag            -- ^ -h|--help
    | VersionFlag         -- ^ -v|--version
    | QuietFlag           -- ^ -q|--quiet
    | SuppressErrorsFlag  -- ^ --suppress-errors
    | ReinstallFlag       -- ^ --reinstall
    | Unknown String


parseArgs :: [String] -> [Arg]
parseArgs = Maybe.mapMaybe parseArg


parseArg :: String -> Maybe Arg
parseArg ""                  = Nothing

parseArg "--help"            = Just HelpFlag
parseArg "-h"                = Just HelpFlag

parseArg "--version"         = Just VersionFlag
parseArg "-v"                = Just VersionFlag

parseArg "--suppress-errors" = Just SuppressErrorsFlag
parseArg "--reinstall"       = Just ReinstallFlag

parseArg unknown@('-' : _)   = Just (Unknown unknown)
parseArg positional          = Just (Positional positional)


dotfile :: Path Rel File
dotfile = $(Path.mkRelFile ".elm-smuggle")


-- TODO: A better format for this file (probs json)
parseDotfile :: Text -> Either String [(Git.Url, Maybe Elm.Constraint)]
parseDotfile = traverse (parseLine . Text.strip) . Text.lines
  where
    parseLine :: Text -> Either String (Git.Url, Maybe Elm.Constraint)
    parseLine line = case Text.strip <$> Text.breakOn " " line of
        (url', "") -> do
            url <- note ("bad url: " <> Text.unpack url') $
                   Git.parseUrl (Text.unpack url')
            pure (url, Nothing)

        (url', constraint') -> do
            url <- note ("bad url: " <> Text.unpack url') $
                   Git.parseUrl (Text.unpack url')
            constraint <- note ("bad constraint: " <> Text.unpack constraint') $
                          Elm.constraintFromText constraint'
            pure (url, Just constraint)


note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a


red :: Text -> Text
red text = vividSGR ANSI.Red <> text <> resetSGR


vividSGR :: ANSI.Color -> Text
vividSGR color =
    Text.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]


resetSGR :: Text
resetSGR = Text.pack $ ANSI.setSGRCode [ANSI.Reset]
