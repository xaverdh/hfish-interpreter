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
import System.Unix.IO
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import Data.Functor
-- import Data.ByteString as B


-- | Create a handle-mvar pair. Spawns a thread which
--   reads from the read end of the returned fd
--   and writes the result into the /MVar/. Returns the
--   write end of the fd along with said /MVar/.
createHandleMVarPair :: Fish (MVar Str,PT.Fd)
createHandleMVarPair =
  liftIO $ do
    (rE,wE) <- P.createPipe
    mvar <- newEmptyMVar
    forkIO ( fdGetContents rE >>= putMVar mvar )
    -- forkIO ( P.fdToHandle rE >>= B.hGetContents >>= putMVar mvar )
    pure (mvar,wE)


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
    let onErr err = liftIO $ putMVar mvar (Left err) >> myThreadId >>= killThread
    flip forkFinally (const cleanup) $ do
      s' <- runFish f (disallowK onErr r) s
      {- Run fish action and ignore attempts to
         jump out of forked fish-action -}
      putMVar mvar (Right s')
    pure mvar

