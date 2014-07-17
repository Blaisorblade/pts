{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# OPTIONS -fno-cse -fno-full-laziness #-}
module PTS.Syntax.Names
  ( Name (PlainName, IndexName, MetaName)
  , Names
  , freshvarl
  , ModuleName (ModuleName)
  , parts
  , Eval
  , runEval
  , fresh
  ) where

import Control.Monad.Identity

import Data.Char (isAlphaNum, isDigit, isLetter, isLower)
import Data.Data (Data)
import Data.Set (Set, member)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data ModuleName
  =  ModuleName [String]
  deriving (Eq, Ord, Show)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs

data Name
  = PlainName String
  | IndexName String Int
  | MetaName String
  | NumberName Int HiddenName
  deriving (Eq, Ord, Data, Typeable)

-- Store a name but avoids using it for comparison
newtype HiddenName = HiddenName Name
  deriving (Data, Typeable)
instance Eq HiddenName where
  (==) = const . const True
instance Ord HiddenName where
  compare = const . const EQ

type Names = Set Name

instance Show Name where
  showsPrec _ (PlainName text) = showString text
  showsPrec _ (IndexName text i) = showString text . shows i
  showsPrec _ (MetaName text) = showChar '$' . showString text
  showsPrec _ (NumberName i (HiddenName n)) = shows n . showString "#" . shows i

instance Read Name where
  readsPrec _ (c:cs) | isLetter c = [plainName [c] cs] where
    plainName text (c:cs) | isDigit c = indexName text [c] cs
    plainName text (c:cs) | isAlphaNum c = plainName (c : text) cs
    plainName text rest = (PlainName (reverse text), rest)

    indexName text index (c:cs) | isDigit c = indexName text (c : index) cs
    indexName text index (c:cs) | isAlphaNum c = plainName (index ++ text) cs
    indexName text index rest = (IndexName (reverse text) (read (reverse index)), rest)

  readsPrec _ ('$':c:cs) | isLower c = [metaName [c] cs] where
    metaName text (c:cs) | isAlphaNum c = metaName (c : text) cs
    metaName text rest = (MetaName (reverse text), rest)

  readsPrec _ _ = []

nextIndex :: Name -> Name
nextIndex (PlainName text) = IndexName text 0
nextIndex (IndexName text index) = IndexName text (index + 1)
nextIndex (NumberName index n) = NumberName (index + 1) n

freshvarl :: Names -> Name -> Name
freshvarl xs x
  =  if x `member` xs
     then freshvarl xs (nextIndex x)
     else x

idx :: IORef Int
idx = unsafePerformIO $ newIORef 0
{-# NOINLINE idx #-}

getAndIncIdx :: IO Int
getAndIncIdx =
  do
    x <- readIORef idx
    writeIORef idx $ x + 1
    return x
{-# NOINLINE getAndIncIdx #-}

fresh :: Name -> Name
fresh n =
  NumberName (unsafePerformIO $ getAndIncIdx) (HiddenName (getBase n))
{-# NOINLINE fresh #-}

getBase (NumberName _ (HiddenName n)) = n
getBase n = n

runEval _ = runIdentity
type Eval = Identity
