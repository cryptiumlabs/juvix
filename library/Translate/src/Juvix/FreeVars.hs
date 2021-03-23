-- |
-- TODO ∷ determine what has changed in the rebasing of this algo
-- - FreeVars is an algorithm that checks for free symbols in the AST.
-- - The =ExcludedSet= holds the symbols defined... These are needed
--   in case of a degenerate case like
--   #+BEGIN_SRC ocaml
--     let foo =
--       let type point = {x : int, y : int} in
--       let our-point  = {x = 3, y = 4} in
--       our-point.x + our-point.y
--   #+END_SRC
--   + here we need to dismiss =our-point.x= and =our-point.y=, just
--     filtering out =our-point= isn't enough! we have to check if the
--     first-part of the name has =our-point=, since everything shares
--     the same namespace
-- - TODO :: How do we handle this case?
--   #+BEGIN_SRC ocaml
--     mod Foo where
--
--     let foo (x :: xs) = x + TopLevel.Foo.foo xs
--     let foo []        = 0
--   #+END_SRC
--   + To Handle this, we need to unqualify the foo, and have the
--     module handle the symbol allocation
-- - NOTE :: we assume in =nameifyAdt= which takes effect in the =\\=
--   call to =nameifyLetType=, that definitions of constructors before
--   this point can't be redefined
--   + This means that if we have ordered definitions, we'll silently
--     drop the calls to the old constructors.
--   + Thus, please redefine the logic there to support such modes
-- - _Reasons to update_
--   1. let's not being recursive
--      - we assume lets are recursive, if this changes the code
--        has to be updated to account for that'
--
--   2. Language becomes ordered
--      - see first note above
--
--   3. Universe or Declaration talk about free variables
--      - currently universe is unfinished, and are not
--        first class
-- |
-- - FreeVars is an algorithm that checks for free symbols in the AST.
-- - The =ExcludedSet= holds the symbols defined... These are needed
--   in case of a degenerate case like
--   #+BEGIN_SRC ocaml
--     let foo =
--       let type point = {x : int, y : int} in
--       let our-point  = {x = 3, y = 4} in
--       our-point.x + our-point.y
--   #+END_SRC
--   + here we need to dismiss =our-point.x= and =our-point.y=, just
module Juvix.FreeVars where

import qualified Data.HashSet as Set
import qualified Juvix.Contextify.Environment as Env
import qualified Juvix.Core.Common.Closure as Closure
import Juvix.Library hiding (Set)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

op :: Sexp.T -> Set.HashSet NameSymbol.T
op = free . snd . runM . freeVarPass

data Env
  = Env
      { closure :: Closure.T,
        ignoreSet :: Set.HashSet NameSymbol.T,
        free :: Set.HashSet NameSymbol.T
      }
  deriving (Generic, Show)

type EnvAlias =
  (State Env)

newtype EnvM a = Ctx {_run :: EnvAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" EnvAlias
  deriving
    ( HasState "ignoreSet" (Set.HashSet NameSymbol.T),
      HasSource "ignoreSet" (Set.HashSet NameSymbol.T),
      HasSink "ignoreSet" (Set.HashSet NameSymbol.T)
    )
    via StateField "ignoreSet" EnvAlias
  deriving
    ( HasState "free" (Set.HashSet NameSymbol.T),
      HasSource "free" (Set.HashSet NameSymbol.T),
      HasSink "free" (Set.HashSet NameSymbol.T)
    )
    via StateField "free" EnvAlias

runM :: EnvM a -> (a, Env)
runM (Ctx c) =
  runState c (Env Closure.empty (Set.fromList Env.namedForms) Set.empty)

freeVarPass ::
  ( HasReader "closure" Closure.T f,
    HasState "free" (Set.HashSet NameSymbol.T) f,
    HasState "ignoreSet" (Set.HashSet NameSymbol.T) f
  ) =>
  Sexp.T ->
  f Sexp.T
freeVarPass form =
  Env.onExpression form (\x -> x == ":atom" || x == ":primitive") freeVarRes

freeVarRes ::
  ( HasReader "closure" Closure.T m,
    HasState "ignoreSet" (Set.HashSet NameSymbol.T) m,
    HasState "free" (Set.HashSet NameSymbol.T) m
  ) =>
  Sexp.Atom ->
  Sexp.T ->
  m Sexp.T
freeVarRes _a form@(Sexp.List [name])
  | Just Sexp.A {atomName} <- Sexp.atomFromT name = do
    modify @"ignoreSet" (Set.insert atomName)
    pure form
freeVarRes Sexp.A {atomName = name} sexpAtom = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just _ -> pure sexpAtom
    Nothing -> do
      set <- get @"ignoreSet"
      case name `Set.member` set of
        True -> pure sexpAtom
        False -> do
          modify @"free" (Set.insert name)
          pure sexpAtom
freeVarRes _ s = pure s
