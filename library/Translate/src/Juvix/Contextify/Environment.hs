module Juvix.Contextify.Environment
  ( ErrorS (..),
    ErrS,
    SexpContext,
    HasClosure,
    passContextSingle,
    passContext,
    Pass (..),
    extractInformation,
    lookupPrecedence,
    Minimal (..),
    MinimalAlias,
    MinimalAliasIO,
    MinimalM (..),
    MinimalMIO (..),
    runMIO,
    runM,
    namedForms,
    onExpression,
  )
where

import Control.Lens hiding ((|>))
import qualified Juvix.Contextify.InfixPrecedence.ShuntYard as Shunt
import qualified Juvix.Core.Common.Closure as Closure
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

data ErrorS
  = CantResolve [Sexp.T]
  | UnknownSymbol NameSymbol.T
  | ImpossibleMoreEles
  | Clash
      (Shunt.Precedence Sexp.T)
      (Shunt.Precedence Sexp.T)
  deriving (Show, Eq)

type SexpContext = Context.T Sexp.T Sexp.T Sexp.T

type HasClosure m = HasReader "closure" Closure.T m

type ErrS m = HasThrow "error" ErrorS m

------------------------------------------------------------
-- Runner environment
------------------------------------------------------------

data Minimal = Minimal
  { closure :: Closure.T
  }
  deriving (Generic, Show)

type MinimalAlias =
  ExceptT ErrorS (State Minimal)

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAlias
  deriving
    (HasThrow "error" ErrorS)
    via MonadError MinimalAlias

type MinimalAliasIO =
  ExceptT ErrorS (StateT Minimal IO)

newtype MinimalMIO a = CtxIO {_runIO :: MinimalAliasIO a}
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasReader "closure" Closure.T,
      HasSource "closure" Closure.T
    )
    via ReaderField "closure" MinimalAliasIO
  deriving
    (HasThrow "error" ErrorS)
    via MonadError MinimalAliasIO

runMIO :: MinimalMIO a -> IO (Either ErrorS a, Minimal)
runMIO (CtxIO c) = runStateT (runExceptT c) (Minimal Closure.empty)

runM :: MinimalM a -> (Either ErrorS a, Minimal)
runM (Ctx c) = runState (runExceptT c) (Minimal Closure.empty)

------------------------------------------------------------
-- Main Functionality
------------------------------------------------------------

data Pass m = Pass
  { sumF :: SexpContext -> Sexp.Atom -> Sexp.T -> m Sexp.T,
    termF :: SexpContext -> Sexp.Atom -> Sexp.T -> m Sexp.T,
    tyF :: SexpContext -> Sexp.Atom -> Sexp.T -> m Sexp.T
  }

-- | @passContextSingle@ Traverses the context firing off sexp
-- traversals based on the given trigger.
passContextSingle ::
  (HasClosure m, ErrS m) =>
  -- | the context in which our code resides in
  SexpContext ->
  -- | the trigger function that states what forms to fire off
  -- on. :atom for firing on every atom
  (NameSymbol.T -> Bool) ->
  -- | Our transformation function that is responsible for handling the triggers
  (SexpContext -> Sexp.Atom -> Sexp.T -> m Sexp.T) ->
  m SexpContext
passContextSingle ctx trigger f =
  passContext ctx trigger (Pass f f f)

-- | @passContext@ like @passContextSingle@ but we supply a different
-- function for each type term and sum representation form.
passContext ::
  (HasClosure m, ErrS m) => SexpContext -> (NameSymbol.T -> Bool) -> Pass m -> m SexpContext
passContext ctx trigger Pass {sumF, termF, tyF} =
  Context.mapWithContext
    ctx
    Context.CtxForm
      { -- Need to do this consing of type to figure out we are in a type
        -- we then need to remove it, as it shouldn't be there
        sumF = \form ->
          fmap Sexp.cdr . pass sumF (Sexp.Cons (Sexp.atom "type") form),
        termF = pass termF,
        tyF = pass tyF
      }
  where
    pass func form ctx =
      Sexp.foldSearchPred
        form
        (trigger, func ctx)
        (bindingForms, searchAndClosure ctx)

-- | @onExpression@ runs an algorithm similar to @passContext@ however
-- only on a single expression instead of an entire context. For this
-- to have the closure work properly, please run this after :open-in is
-- gone
onExpression ::
  HasClosure f =>
  Sexp.T ->
  (NameSymbol.T -> Bool) ->
  (Sexp.Atom -> Sexp.T -> f Sexp.T) ->
  f Sexp.T
onExpression form trigger func =
  Sexp.foldSearchPred
    form
    (trigger, func)
    (bindingForms, searchAndClosureNoCtx)

-- | @bindingForms@ is a predicate that answers true for every form
-- that instantiates a new variable
bindingForms :: (Eq a, IsString a) => a -> Bool
bindingForms x =
  x
    `elem` [ "type",
             ":open-in",
             ":let-type",
             ":let-match",
             "case",
             ":lambda-case",
             ":declaim",
             ":lambda",
             ":primitive"
           ]

-- should really be moved out of here

-- | @namedForms@ a list of all named special forms
namedForms :: [NameSymbol.T]
namedForms =
  [ "type",
    ":open-in",
    ":let-type",
    ":let-match",
    "case",
    ":lambda-case",
    ":declaim",
    ":lambda",
    ":tuple",
    ":primitive",
    ":progn",
    "declare",
    ":infix",
    ":list",
    ":record-no-pun",
    ":paren",
    ":block",
    ":primitive"
  ]

-- | @searchAndClosureNoCtx@ like searchAndClosure but does not rely on
-- a context, thus the open pass can't be done.
searchAndClosureNoCtx ::
  (HasClosure f) =>
  -- | the atom to dispatch on
  Sexp.Atom ->
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
searchAndClosureNoCtx a as cont
  | named "case" = case' as cont
  -- this case happens at the start of every defun
  | named ":lambda-case" = lambdaCase as cont
  -- This case is a bit special, as we must check the context for
  -- various names this may introduce to the
  | named ":declaim" = declaim as cont
  | named ":let-match" = letMatch as cont
  | named ":primitive" = primitive as cont
  | named ":let-type" = letType as cont
  | named "type" = type' as cont
  | named ":lambda" = lambda as cont
  where
    named = Sexp.isAtomNamed (Sexp.Atom a)
searchAndClosureNoCtx _ _ _ = error "imporper closure call"

-- | @searchAndClosure@ is responsible for properly updating the
-- closure based on any binders we may encounter. The signature is made
-- to fit into the @Sexp.foldSearchPred@'s binder dispatch clause, the
-- only difference is that we take an extra context before that
-- signature.
searchAndClosure ::
  (HasClosure f, ErrS f) =>
  -- | The Context, an extra function that is required the by the
  -- :open-in case.
  SexpContext ->
  -- | the atom to dispatch on
  Sexp.Atom ->
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
searchAndClosure ctx a as cont
  | named ":open-in" = openIn ctx as cont
  | otherwise = searchAndClosureNoCtx a as cont
  where
    named = Sexp.isAtomNamed (Sexp.Atom a)
searchAndClosure _ _ _ _ = error "imporper closure call"

------------------------------------------------------------
-- Environment functionality
------------------------------------------------------------
extractInformation ::
  Context.Definition term ty sumRep -> Maybe [Context.Information]
extractInformation (Context.Def Context.D {defPrecedence}) =
  Just [Context.Prec defPrecedence]
extractInformation (Context.Information is) =
  Just is
extractInformation _ = Nothing

lookupPrecedence ::
  (ErrS m, HasClosure m) => NameSymbol.T -> Context.T t y s -> m Context.Precedence
lookupPrecedence name ctx = do
  closure <- ask @"closure"
  let symbolName = NameSymbol.hd name
  case Closure.lookup symbolName closure of
    Just Closure.Info {info}
      | NameSymbol.toSymbol name == symbolName ->
        pure $ fromMaybe Context.default' (Context.precedenceOf info)
    Just Closure.Info {mOpen = Just prefix} ->
      contextCase (prefix <> name)
    Just Closure.Info {} ->
      throw @"error" (UnknownSymbol name)
    Nothing ->
      contextCase name
  where
    contextCase name =
      case Context.lookup name ctx >>= extractInformation . Context.extractValue of
        Nothing -> throw @"error" (UnknownSymbol name)
        Just pr -> pure (fromMaybe Context.default' (Context.precedenceOf pr))

------------------------------------------------------------
-- searchAndClosure function dispatch table
------------------------------------------------------------

primitive :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
primitive (Sexp.List [name]) cont
  | Just Sexp.A {atomName} <- Sexp.atomFromT name = do
    -- this is bad, however it's FINE, as we only have the primitive
    -- which no operation should disrupt.
    -- Namely if it's (:primitive Michelson.Foo)
    -- we add Michelson to the closure, which is wrong.
    -- However, since we just have the primitive at this point, we
    -- can't have any other symbol named Michelson in this scope, so
    -- it's not an actual issue.
    local @"closure" (Closure.insertGeneric (NameSymbol.hd atomName)) $ do
      prim <- cont name
      pure $ Sexp.list [prim]
primitive _ _ = error "malformed primitive"

lambda :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
lambda (Sexp.List [arguments, body]) cont =
  local @"closure" (\cnt -> foldr Closure.insertGeneric cnt (nameStar arguments)) $ do
    args <- cont arguments
    bod <- cont body
    pure $ Sexp.list [args, bod]
lambda _ _ = error "malformed lambda"

letType :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letType (Sexp.List [assocName, args, dat, body]) cont = do
  local @"closure" closureUpdate $ do
    d <- cont dat
    assoc <- cont assocName
    bod <- cont body
    pure $ Sexp.list [assoc, args, d, bod]
  where
    bindings = nameStar args
    consturctors = nameGather dat
    closureUpdate cnt =
      foldr Closure.insertGeneric cnt (bindings <> consturctors)
letType _ _ = error "malformed let-type"

type' :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
type' (assocName Sexp.:> args Sexp.:> dat) cont =
  local @"closure" (\cnt -> foldr Closure.insertGeneric cnt grabBindings) $ do
    d <- cont dat
    assoc <- cont assocName
    pure $ assoc Sexp.:> args Sexp.:> d
  where
    grabBindings = nameStar args
type' s _ = panic $ "malformed type: " <> show s

-- | @openIn@ opens @mod@, adding the contents to the closure of
-- @body@. Note that we first =resolve= what mod is by calling the
-- continuation, @cont@, in case any transformations want to change
-- what the @mod@ is.
openIn ::
  (ErrS f, HasClosure f) => SexpContext -> Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
openIn ctx (Sexp.List [mod, body]) cont = do
  -- Fully run what we need to on mod
  newMod <- cont mod
  -- Now let us open up the box
  case Sexp.atomFromT newMod of
    Just Sexp.A {atomName} ->
      case ctx Context.!? atomName >>| Context.extractValue of
        Just (Context.Record record) ->
          let NameSpace.List {publicL} = NameSpace.toList (record ^. Context.contents)
              --
              newSymbs = fst <$> publicL
              --
              addSymbolInfo symbol =
                Closure.insert symbol (Closure.Info Nothing [] (Just atomName))
           in --
              local @"closure" (\cnt -> foldr addSymbolInfo cnt newSymbs) do
                newBody <- cont body
                pure $ Sexp.list [newMod, newBody]
        _ ->
          throw @"error" (CantResolve [newMod])
    _ ->
      throw @"error" (CantResolve [newMod])
openIn _ _ _ = error "malformed open-in"

-- | @lambdaCase@ we encounter a @:lambda-case@ at the start of every
-- Definition in the context. This ensures the arguments are properly
-- bound for the inner computation.
lambdaCase :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
lambdaCase binds cont =
  mapF (`matchMany` cont) binds

letMatch :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letMatch (Sexp.List [name, bindings, body]) cont
  | Just nameSymb <- eleToSymbol name =
    local @"closure" (Closure.insertGeneric nameSymb) $ do
      -- this just makes it consistent with the lambdaCase case
      let grouped = Sexp.groupBy2 bindings
      form <- mapF (`matchMany` cont) grouped
      bod <- cont body
      pure $ Sexp.list [name, Sexp.unGroupBy2 form, bod]
letMatch _ _ = error "malformed let-match"

-- | @case'@ is similar to @lambdaCase@ except that it has a term it's
-- matching on that it must first change without having an extra
-- binders around it
case' :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
case' (t Sexp.:> binds) cont = do
  op <- cont t
  binding <- mapF (`match` cont) binds
  pure (Sexp.Cons op binding)
case' _ _ = error "malformed case"

-- This works as we should only do a declaration after the function
-- locally, so if it gets overwritten its' not a big deal

-- | @declaim@ takes a declaration and adds the declaration information
-- to the context
declaim :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
declaim (Sexp.List [d, e]) cont
  | Just (name, information) <- declaration d =
    local @"closure" (Closure.insert name information) $ do
      -- safe to do dec here, as if we modify the declaration it
      -- would be fine to do it after, as all a pass would do is to
      -- make it a namesymbol, meaning it wouldn't work as is ☹
      dec <- cont d
      exp <- cont e
      pure $ Sexp.list [dec, exp]
declaim _ _ = error "malformed declaim"

------------------------------------------------------------
-- Helpers for the various Search and Closure dispatch
------------------------------------------------------------

-- | @matchMany@ deals with a @((binding-1 … binding-n) body) term, and
-- proper continues the transformation on the body, and the bindings
-- after making sure to register that they are indeed bound terms
matchMany :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
matchMany = matchGen nameStar

-- | @match@ deals with a @(bindings body)@ term coming down, see
-- @matchMany@ for more details
match :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
match = matchGen nameStarSingle

-- | @matchGen@ is a generic/general version of match and matchMany as
-- the form that comes in may be a list of binders or a single term
-- being bound.
matchGen ::
  (HasClosure m, Foldable t) =>
  (Sexp.T -> t Symbol) ->
  Sexp.T ->
  (Sexp.T -> m Sexp.T) ->
  m Sexp.T
matchGen nameStarFunc (Sexp.List [path, body]) cont =
  -- Important we must do this first!
  local @"closure" (\cnt -> foldr Closure.insertGeneric cnt grabBindings) $ do
    -- THIS MUST happen in the local, as we don't want to have a pass
    -- confuse the variables here as something else... imagine if we
    -- are doing a pass which resolves symbols, then we'd try to
    -- resolve the variables we bind. However for constructors and what
    -- not they need to be ran through this pass
    newPath <- cont path
    newB <- cont body
    pure (Sexp.list [newPath, newB])
  where
    grabBindings = nameStarFunc path
matchGen _ _ _ = error "malformed match"

-- | @nameStarSingle@ like @nameStar@ but we are matching on a single
-- element
nameStarSingle :: Sexp.T -> [Symbol]
nameStarSingle = nameStar . (\x -> Sexp.list [x])

-- | @nameStar@ grabs names recursively
nameStar :: Sexp.T -> [Symbol]
nameStar ((_caar Sexp.:> cadr) Sexp.:> cdr) =
  -- we ignore the _caar as it's a cosntructor!
  nameStar cadr <> nameStar cdr
nameStar (x Sexp.:> xs)
  | Just symb <- eleToSymbol x =
    symb : nameStar xs
  | otherwise =
    -- the car is not a cons or an atom, thus a number, we should
    -- ignore it
    nameStar xs
nameStar Sexp.Atom {} = []
nameStar Sexp.Nil = []

-- Sexp.parse "((Cons (:arrow (:infix -> Int Int))) (Nil))" >>| nameGather

-- | @nameGather@ takes an adt sexp and extracts the constructors from it
nameGather :: Sexp.T -> [Symbol]
nameGather ((caar Sexp.:> _cdar) Sexp.:> cdr)
  | Just symb <- eleToSymbol caar,
    symb /= ":" || symb /= ":record-d" =
    symb : nameGather cdr
nameGather (_ Sexp.:> cdr) = nameGather cdr
nameGather _ = []

------------------------------------------------------------
-- Helpers for declaim
------------------------------------------------------------

-- | @declaration@ takes a declaration and tries to get the information
-- along with the name from it.
-- - Note :: we can only get symbol declarations to update, as we rely
--   on closure semantics whicich only work on symbols unfortunately.
declaration :: Sexp.T -> Maybe (Symbol, Closure.Information)
declaration (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let func =
          if
              | Sexp.isAtomNamed inf "infix" ->
                Context.NonAssoc
              | Sexp.isAtomNamed inf "infixl" ->
                Context.Left
              | Sexp.isAtomNamed inf "infixr" ->
                Context.Right
              | otherwise -> error "malformed declaration"
     in Just
          ( atomName,
            Closure.Info
              Nothing
              [Context.Prec $ Context.Pred func (fromInteger atomNum)]
              Nothing
          )
declaration _ = Nothing

------------------------------------------------------------
-- Move to Sexp library
------------------------------------------------------------

mapF :: Applicative f => (Sexp.T -> f Sexp.T) -> Sexp.T -> f Sexp.T
mapF f (x Sexp.:> xs) =
  Sexp.Cons <$> f x <*> mapF f xs
mapF _ Sexp.Nil = pure Sexp.Nil
mapF _ a = pure a

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
