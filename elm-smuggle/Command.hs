{-# LANGUAGE RecordWildCards #-}
module Command
    ( Command
    , which
    , whichFind
    , run
    , runWithYes
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
import qualified System.IO as IO
import qualified System.Process as Process

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Path (Abs, File, Dir, Path, Rel)


newtype Command = Command { _commandPath :: Path Abs File }


which :: MonadIO m => Path Rel File -> m (Maybe Command)
which = fmap (fmap Command) . Path.IO.findExecutable


whichFind :: MonadIO m => [Path b Dir] -> Path Rel File -> m (Maybe Command)
whichFind dirs = fmap (fmap Command) . Path.IO.findFile dirs


data Result = Result
    { exit   :: Exit.ExitCode
    , stdout :: String
    , stderr :: String
    }


run :: MonadIO m => Command -> [String] -> m Result
run (Command cmd) args = liftIO $ do
    (exit, stdout, stderr) <- Process.readCreateProcessWithExitCode
        createProcess
        "" -- stdin
    pure Result {..}
  where
    -- NOTE: Inheriting std streams
    createProcess :: Process.CreateProcess
    createProcess = Process.proc (Path.fromAbsFile cmd) args


runWithYes :: MonadIO m => Command -> [String] -> m Result
runWithYes (Command cmd) args = liftIO $ do
    (hStdin, hStdout, hStderr, processHandle) <- Process.runInteractiveProcess
        (Path.fromAbsFile cmd)
        args
        Nothing -- default working directory
        Nothing -- default environment

    _        <- forkIO (yes hStdin)
    exitCode <- Process.waitForProcess processHandle
    Result exitCode <$> IO.hGetContents hStdout <*> IO.hGetContents hStderr
  where
    yes :: IO.Handle -> IO ()
    yes h = do
        closed <- IO.hIsClosed h
        unless closed $ do
            IO.hPutChar h 'y'
            threadDelay 500000 -- 500ms
            yes h
