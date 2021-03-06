-- Entry point for `cabal repl`.
--
-- Calling :browse should give a nice overview of the API for interactive use.
--
-- To this end, I try to have in scope just enough stuff to minimize qualified
-- names there, and to monomorphise signatures for higher readability.
--
-- Moreover, I use qualified imports just to have shorter names in :browse.

module PTS.Interactive
           ( module PTS.Interactive -- Everything defined here.
           , module PTS.Syntax.Term
           --, module PTS.Syntax -- too many parsing-related details
           --, module PTS.Statics -- we export wrapped functions
           , module PTS.Dynamics
           , module PTS.QuasiQuote
           , showPretty
           -- Qualified names from PTS.Instances
           , Instances.coc
           , Instances.fomegastar
           ) where

-- ASTs
import PTS.Syntax.Names (Name(..))
import PTS.Syntax.Term (Term(..), TermStructure(..), BinOp(..))
import PTS.Dynamics.TypedTerm (TypedTerm(..))

import PTS.Syntax

import PTS.Statics
import PTS.QuasiQuote

import PTS.Error

import PTS.Process.File hiding (getBindings)
import PTS.Interactive.Runners

import qualified PTS.Instances as Instances
import PTS.Dynamics
import qualified PTS.Dynamics.Value as Value
import qualified PTS.Dynamics.Evaluation as Evaluation

import Data.Map (Map)
import Data.Maybe

parseSimple :: String -> Either Errors Term
parseSimple input = parseTerm "REPL" input

parseStSimple :: String -> Either Errors Stmt
parseStSimple input = parseStmt "REPL" input

nbeClosed :: TypedTerm Eval -> Term
nbeClosed = nbe []

reifyEnv :: Bindings Eval -> Value Eval -> Term
reifyEnv env = runEval env . reify

reifyClosed :: Value Eval -> Term
reifyClosed = reifyEnv []

processFileSimple inst f = runErrorsAndOpts inst (processFile f)
processFileSimpleInt inst f = runErrorsAndOpts inst (processFileInt f)
processStmtSimple inst stmt = runErrorsAndOptsGetState inst (processStmt stmt)

-- With lens, this is r ^. _Right . _2 . _3
-- (Maybe ModuleName, (Map ModuleName (Module Eval), [ModuleName], Bindings Eval))
getBindings :: Either Errors (Maybe (Module Eval)) -> Bindings Eval
getBindings (Right (Just (Module _ _ bindings))) = bindings
getBindings _ = []

wrapTypecheckPull ::
  Maybe Instances.PTS
  -> Term
  -> Bindings Eval
  -> IO (Either Errors (TypedTerm Eval))
wrapTypecheckPush ::
  Maybe Instances.PTS
  -> Term
  -> Value Eval
  -> Bindings Eval
  -> IO (Either Errors (TypedTerm Eval))
wrapTypecheckPushUntyped ::
  Maybe Instances.PTS
     -> Term
     -> Term
     -> Bindings Eval
     -> IO (Either Errors (TypedTerm Eval))

wrapTypecheckPull inst term =
  typecheckWrapper inst (typecheckPull term)

-- expectedType must already have been typechecked. `typecheckPushUntyped` does that for you.
wrapTypecheckPush inst term expectedType =
  typecheckWrapper inst (typecheckPush term expectedType)

wrapTypecheckPushUntyped inst term untypedExpectedType =
  typecheckWrapper inst (typecheckPushUntyped term untypedExpectedType)
