{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Sexp where

import Control.Lens (set)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Desugar.Types as Repr
import qualified Juvix.FrontendContextualise.Contextify.Types as Type
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

-- the name symbols are the modules we are opening
-- TODO ∷ parallelize this
run,
  contextify ::
    Type.ContextSexp ->
    (Context.NameSymbol, [Sexp.T]) ->
    IO (Either Context.PathError Type.PassSexp)
contextify cont (nameSymb, xs) = do
  newNamespace <- Context.switchNameSpace nameSymb cont
  case newNamespace of
    Left errr -> pure (Left errr)
    Right ctxS ->
      foldM f Type.PS {ctxS, opensS = [], modsDefinedS = []} xs
        >>| Right
  where
    f Type.PS {ctxS, opensS, modsDefinedS} top = do
      Type.PS {ctxS = ctx', opensS = opens', modsDefinedS = modsDefined'} <-
        updateTopLevel top ctxS
      pure
        Type.PS
          { ctxS = ctx',
            opensS = opensS <> opens',
            modsDefinedS = modsDefinedS <> modsDefined'
          }
run = contextify

updateTopLevel :: Sexp.T -> Type.ContextSexp -> IO Type.PassSexp
updateTopLevel x ctx
  | Sexp.isAtomNamed x ":type-class" = pure $ Type.PS ctx [] []
  | Sexp.isAtomNamed x ":instance" = pure $ Type.PS ctx [] []
updateTopLevel (name Sexp.:> body) ctx
  | named ":defsig-match" = defun body ctx
  | named "declare" = undefined
  | named "type" = undefined
  | named "open" = undefined
  where
    named = Sexp.isAtomNamed name

defun (f Sexp.:> sig Sexp.:> forms) ctx
  | Just name <- eleToSymbol f = do
    let precendent =
          case Context.extractValue <$> Context.lookup (pure name) ctx of
            Just (Context.Def Context.D {defPrecedence}) ->
              defPrecedence
            Just (Context.Information info) ->
              fromMaybe Context.default' (Context.precedenceOf info)
            _ -> Context.default'
    undefined

-- TODO ∷ why is the context empty?
-- we should somehow note what lists are in scope

-- TODO ∷
-- - once we have type checking, rely on that
-- - for dep types where inference is undecidable, force signature
-- - for functions like (f x) where that evals to a module, where it
--   is dependent but decidable, force signature?

-- The NameSymbol.T return returns back all record names that are found
-- we don't return a map, as we can't do opens in a record

-- | decideRecordOrDef tries to figure out
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  Sexp.T ->
  Symbol ->
  NameSymbol.T ->
  Context.Precedence ->
  Maybe Sexp.T ->
  IO (Type.DefinitionSexp, [NameSymbol.T])
decideRecordOrDef xs@(Sexp.List [Sexp.List [Sexp.Nil, body]]) recordName currModName pres ty =
  -- For the two matched cases eventually
  -- turn these into record expressions
  case body of
    name Sexp.:> rest
      | Sexp.isAtomNamed name ":record-no-pun" -> do
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at
        -- types
        (nameSpace, innerMods) <- foldM f (NameSpace.empty, []) grouped
        --
        emptyRecord <- atomically Context.emptyRecord
        --
        let updated = set Context.contents nameSpace . set Context.mTy ty
        --
        pure (Context.Record (updated emptyRecord), newRecordName : innerMods)
      where
        Just grouped = Sexp.toList (Sexp.groupBy2 rest)
        newRecordName = currModName <> pure recordName
        --
        f (nameSpace, prevModNames) (Sexp.List [s, e])
          | Just Sexp.A {atomName} <- Sexp.atomFromT s =
            let fieldN = NonEmpty.last atomName
                like = Sexp.list [Sexp.list [Sexp.Nil, e]]
             in Nothing
                  |> decideRecordOrDef like fieldN newRecordName Context.default'
                  >>| bimap
                    (\d -> NameSpace.insert (NameSpace.Pub fieldN) d nameSpace)
                    (<> prevModNames)
    _ -> def
  where
    def = pure (Context.Def (Context.D Nothing ty xs pres), [])
decideRecordOrDef xs _ _ pres ty =
  pure (Context.Def (Context.D Nothing ty xs pres), [])

collectConstructors :: Repr.Data -> [Symbol]
collectConstructors dat =
  let adt' =
        case dat of
          Repr.Arrowed _ adt -> adt
          Repr.NonArrowed adt -> adt
      constructors (Repr.Sum sum) =
        NonEmpty.toList (Repr.sumConstructor <$> sum)
      constructors Repr.Product {} = empty
   in constructors adt'

----------------------------------------
-- Helpers
----------------------------------------

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing

toSymbolList :: Sexp.T -> Maybe [Symbol]
toSymbolList x = Sexp.toList x >>= traverse eleToSymbol
