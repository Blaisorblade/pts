{-# OPTIONS -fno-cse -fno-full-laziness #-}
module PTS.Syntax.FreshNames (fresh, Eval, runEval) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import PTS.Syntax.Names

idx :: IORef Int
idx = unsafePerformIO $ newIORef 0
{-# NOINLINE idx #-}

getAndIncIdx :: IO Int
getAndIncIdx =
  do
    x <- readIORef idx
    writeIORef idx $ x + 1
    return x

fresh :: Name -> Name
fresh n =
  NumberName (unsafePerformIO $ getAndIncIdx) (HiddenName (getBaseName n))
{-# NOINLINE fresh #-}
