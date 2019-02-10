{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Concurrent where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E

import System.Process
import System.Exit
import System.IO
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import Data.Functor
-- import Data.ByteString as B


-- | Fork a fish action, returning an mvar which will
--   contain the resulting state once the action finishes.
--   Second parameter is a cleanup routine, which will be run
--   regardless of any errors.
forkFish :: Fish () -> IO () -> Fish (MVar (Either (Maybe HFishError) FishState))
forkFish f cleanup = do
  r <- ask
  s <- get
  liftIO $ do
    mvar <- newEmptyMVar
    flip forkFinally (const cleanup) $ do
      s' <- runFish f (resetReader mvar r) s
      {- Run fish action and ignore attempts to
         jump out of forked fish-action -}
      putMVar mvar $ Right s'
    pure mvar
  where
    onErr mvar err = liftIO $ do
      putMVar mvar (Left err)
      myThreadId >>= killThread
    resetReader mvar = resetK (repeat $ onErr mvar)


