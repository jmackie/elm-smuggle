{-# LANGUAGE RecordWildCards #-}
module Command
    ( Command
    , which
    , run
    , Result
    , exit
    , stdout
    , stderr
    )
where

import Prelude

import qualified Path
import qualified Path.IO
import qualified System.Exit as Exit
import qualified System.Process as Process

import Control.Monad.IO.Class (MonadIO, liftIO)
import Path (Abs, File, Path, Rel)


newtype Command = Command { _commandPath :: Path Abs File }


which :: MonadIO m => Path Rel File -> m (Maybe Command)
which = fmap (fmap Command) . Path.IO.findExecutable


data Result = Result
    { exit   :: Exit.ExitCode
    , stdout :: String
    , stderr :: String
    }


run :: MonadIO m => Command -> [String] -> m Result
run = run' id


run'
    :: MonadIO m
    => (Process.CreateProcess -> Process.CreateProcess)
    -> Command
    -> [String]
    -> m Result
run' f (Command cmd) args = liftIO $ do
    (exit, stdout, stderr) <- Process.readCreateProcessWithExitCode
        (f createProcess)
        stdin
    pure Result {..}
  where
    createProcess :: Process.CreateProcess
    createProcess = Process.proc (Path.fromAbsFile cmd) args

    stdin :: String
    stdin = mempty
