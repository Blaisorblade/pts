{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Parametric.Parser where

import Prelude (String, (++), flip, ($), undefined, Show(..), Either (..), print, (.), pred)

import System.IO

import Control.Applicative hiding (many)
import Control.Monad

import Data.Either
import Data.Eq
import Data.Foldable

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.ParserCombinators.Parsec as Parsec

import Parametric.Error
import Parametric.Parser.Error

import Tools.Errors.Class
import Tools.Instances


     ---------------------
    -- PARAMETRIC PARSER --
     ---------------------

-- left-recursion handling
term simple rec pos msg = result where
  result = combine <$> getPosition <*> simple <*> many ((,) <$> rec <*> getPosition)
  combine p = foldl' (\x (f, q) -> setPos pos p (f x) q)

-- right-recursive syntax pattern: "lambda ident : qualifier . body"
abs
  :: (identT -> qualT -> bodyT -> r) ->
    GenParser tok st unusedLambda -> GenParser tok st identT -> GenParser tok st unusedColon ->
    GenParser tok st qualT -> GenParser tok st unusedDot -> GenParser tok st bodyT ->
    GenParser tok st r
abs cons lambda ident colon qualifier dot body
  = cons <$> try (lambda *> ident <* colon) <*> qualifier <*> (dot *> body)

-- left-recursive syntax pattern: "x -> y"
arr :: Applicative f => (a1 -> a2 -> r) -> f ignored -> f a2 -> f (a1 -> r)
arr cons arrow simple = flip cons <$> (arrow *> simple)

-- left-recursive syntax pattern: "x y"
app :: Functor f => (a1 -> a2 -> r) -> f a2 -> f (a1 -> r)
app cons simple = flip cons <$> simple

-- non-recursive syntax pattern: "ident"
var :: Functor f => (a -> b) -> f a -> f b
var cons ident = cons <$> ident

-- non-recursive syntax pattern: "constant"
con :: Functor f => a -> f b -> f a
con cons constant = cons <$ constant

withPos :: (Position -> a -> b) -> GenParser tok st a -> GenParser tok st b
withPos f p = setPos f <$> getPosition <*> p <*> getPosition

setPos :: (Position -> a -> r) -> SourcePos -> a -> SourcePos -> r
setPos f p1 x p2 = f (Position (sourceName p1) (sourceLine p1) (sourceLine p2) (sourceColumn p1) (pred $ sourceColumn p2)) x

