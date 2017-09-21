{-# language LambdaCase, GADTs, OverloadedStrings, FlexibleContexts #-}
module HFish.Interpreter.Builtins (
  allBuiltins
) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Fish.Lang
import HFish.Interpreter.Core
import HFish.Interpreter.Interpreter
import HFish.Interpreter.Env as Env
import Data.Bifunctor
import qualified Data.Text as T

import HFish.Interpreter.Builtins.Debug
import HFish.Interpreter.Builtins.Flow
import HFish.Interpreter.Builtins.Exit
import HFish.Interpreter.Builtins.Cd
import HFish.Interpreter.Builtins.Dirstack
import HFish.Interpreter.Builtins.Echo
import HFish.Interpreter.Builtins.Printf
import HFish.Interpreter.Builtins.Bool
import HFish.Interpreter.Builtins.Read
import HFish.Interpreter.Builtins.String
import HFish.Interpreter.Builtins.Random
import HFish.Interpreter.Builtins.Seq
import HFish.Interpreter.Builtins.Contains
import HFish.Interpreter.Builtins.Source
import HFish.Interpreter.Builtins.Math
import HFish.Interpreter.Builtins.Eval
import HFish.Interpreter.Builtins.Breakpoint
import HFish.Interpreter.Builtins.Test
import HFish.Interpreter.Builtins.Emit
import HFish.Interpreter.Builtins.Count
import HFish.Interpreter.Builtins.Functions
import HFish.Interpreter.Builtins.Status

allBuiltins :: Env Builtin
allBuiltins =
  Env.fromList [
    ("debug",debugF)
    ,("return",returnF)
    ,("break",breakF)
    ,("continue",continueF)
    ,("exit",exitF)
    ,("cd",cdF)
    ,("pushd",pushdF)
    ,("popd",popdF)
    ,("dirs",dirsF)
    ,("echo",echoF)
    ,("read",readF)
    ,("string",stringF)
    ,("random",randomF)
    ,("seq",seqF)
    ,("contains",containsF)
    ,("source",sourceF)
    ,("true",trueF)
    ,("false",falseF)
    ,("math",mathF)
    ,("eval",evalF)
    ,("exec",execF)
    ,("breakpoint",breakpointF)
    ,("test",testF)
    ,("printf",printfF)
    ,("emit",emitF)
    ,("count",countF)
    ,("functions",functionsF)
    ,("status",statusF)
  ]

