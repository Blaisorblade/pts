{-# LANGUAGE DeriveDataTypeable #-}
module PTS.Syntax.Names
  ( Name (PlainName, IndexName, MetaName)
  , Names
  , NamesMap
  , freshvarl
  , freshvarlMap
  , envToNamesMap
  , ModuleName (ModuleName)
  , parts
  ) where

import Data.Char (isAlphaNum, isDigit, isLetter, isLower)
import Data.Data (Data)
import Data.Set (Set, member)
import qualified Data.Map as Map
import Data.Typeable (Typeable)

data ModuleName
  =  ModuleName [String]
  deriving (Eq, Ord, Show)

parts :: ModuleName -> [String]
parts (ModuleName xs) = xs

data Name
  = PlainName String
  | IndexName String Int
  | MetaName String
  deriving (Eq, Ord, Data, Typeable)

type Names = Set Name

instance Show Name where
  showsPrec _ (PlainName text) = showString text
  showsPrec _ (IndexName text i) = showString text . shows i
  showsPrec _ (MetaName text) = showChar '$' . showString text

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

freshvarl :: Names -> Name -> Name
freshvarl xs x
  =  if x `member` xs
     then freshvarl xs (nextIndex x)
     else x

-- Map name to its current max index.
type NamesMap = Map.Map String Int

-- Merge with fresh, this is just a state monad.
freshvarlMap :: NamesMap -> Name -> (Name, NamesMap)
freshvarlMap names n =
  (case oldLookup of
     Nothing -> n
     Just idx ->
       IndexName raw $
                 -- Here we reupdate the index, because insertLookupWithKey
                 -- returns the old content. However, that's still cheap,
                 -- especially compared to doing a new lookup.
                 transformIdx idx
  , newNames)
    where
      raw = rawName n
      oldIdx = getIdx n
      transformIdx = (+1)
      updateVal key badReplacementValue oldValue = transformIdx oldValue
      (oldLookup, newNames) = Map.insertLookupWithKey updateVal raw oldIdx names

rawName (PlainName text) = text
rawName (IndexName text _) = text

getIdx (PlainName _) = -1
getIdx (IndexName _ idx) = idx

envToNamesMap :: [(Name, a)] -> NamesMap
envToNamesMap = Map.fromListWith max . map (\(name, _) -> (rawName name, getIdx name))
