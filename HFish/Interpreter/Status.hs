{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Status where

import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Var
import HFish.Interpreter.Env as Env
import qualified HFish.Interpreter.Str as Str

import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Data.Sequence
import Data.Semigroup
import Control.Lens
import Control.Monad
import System.Exit

instance Enum ExitCode where
  toEnum = \case
    0 -> ExitSuccess
    i -> ExitFailure i
  fromEnum = \case
    ExitSuccess -> 0
    ExitFailure i -> i

getStatus :: Fish ExitCode
getStatus = use status

setStatus :: ExitCode -> Fish ()
setStatus exCode = do
  status .= exCode
  readOnlyEnv %= insert "status"
    (mkVar . pure
      . Str.showStr $ fromEnum exCode)

modifyStatus :: (ExitCode -> ExitCode) -> Fish ()
modifyStatus f = do
  status %= f
  exCode <- use status
  readOnlyEnv %= insert "status"
    (mkVar . pure
      . Str.showStr $ fromEnum exCode)

invertStatus :: Fish ()
invertStatus =
  modifyStatus $ \case
    ExitSuccess -> ExitFailure 1
    ExitFailure _ -> ExitSuccess

onStatus ::
  (Int -> Fish a) -- ^ ExitFailure _ continuation
  -> Fish a -- ^ ExitSuccess continuation
  -> Fish a
onStatus onErr onSucc = 
  getStatus >>= \case
    ExitSuccess -> onSucc
    ExitFailure i -> onErr i

isOk :: Fish Bool
isOk = onStatus
  (const $ pure False)
  (pure True)

bad :: Fish ()
bad = setStatus (ExitFailure 1)

ok :: Fish ()
ok = setStatus ExitSuccess

ifOk :: Fish () -> Fish ()
ifOk f = isOk >>= flip when f

unlessOk :: Fish () -> Fish ()
unlessOk f = isOk >>= flip unless f


