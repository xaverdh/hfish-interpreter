{-# language OverloadedStrings, LambdaCase #-}
module HFish.Interpreter.Builtins.Status
  ( statusF )
where

import HFish.Interpreter.Core
import HFish.Interpreter.Status
import HFish.Interpreter.Args
import qualified HFish.Interpreter.Stringy as Str

import Data.Semigroup
import Control.Lens
import Control.Monad.Extra

statusF :: Builtin
statusF _ = argsChoice [0,1] $ \case
  [] -> pure () -- TODO
  ["is-interactive"] -> ifM (view interactive) ok bad
  _ -> errork "status: invalid argument"

