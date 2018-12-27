{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Git
    ( Command
    , command
    , clone
    , checkoutTo
    , tags
    , Url
    , parseUrl
    , urlPath
    )
where

import Prelude

import qualified Command
import qualified Network.URI
import qualified Path

import Control.Monad.IO.Class (MonadIO)
import Path (Abs, Dir, Path)


-- | git
newtype Command = Command { _unCommand :: Command.Command }


-- | Lookup the command from $PATH.
command :: MonadIO m => m (Maybe Command)
command = fmap Command <$> Command.which $(Path.mkRelFile "git")


clone :: MonadIO m => Command -> Url -> Path b Dir -> m Command.Result
clone (Command cmd) from into =
    Command.run cmd ["clone", "--quiet", show from, Path.toFilePath into]


tags :: MonadIO m => Command -> m Command.Result
tags (Command cmd) = Command.run cmd ["tag", "--list"]


checkoutTo :: MonadIO m => Command -> Path Abs Dir -> String -> m Command.Result
checkoutTo (Command cmd) absDir branch = Command.run
    cmd
    [ "--work-tree=" <> Path.fromAbsDir absDir
    , "checkout"
    , "--quiet"
    , branch
    , "--"
    , "."
    ]


-- | A url that can be passed to clone.
newtype Url = Url { unUrl :: Network.URI.URI }
    deriving newtype (Show)


-- | Parse a url from a raw string.
parseUrl :: String -> Maybe Url
parseUrl = fmap Url . Network.URI.parseURI


urlPath :: Url -> String
urlPath = Network.URI.uriPath . unUrl
