{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Repr
import Juvix.Library

type Repr f =
  f (NonEmpty (Repr.FunctionLike Repr.Expression)) Repr.Signature Repr.Type

type Context =
  Repr Context.T

type Definition =
  Repr Context.Definition

contextify ::
  Context -> (Context.NameSymbol, [Repr.TopLevel]) -> Either Context.PathError Context
contextify cont (nameSymb, xs) =
  case Context.switchNameSpace nameSymb cont of
    Left errrr -> Left errrr
    Right cont -> Right (foldr updateTopLevel cont xs)

-- TODO ∷ We should return a tuple of opens and the context
updateTopLevel :: Repr.TopLevel -> Context -> Context
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ _)) ctx =
  Context.add (NameSpace.Pub name) (Context.TypeDeclar t) ctx
updateTopLevel (Repr.Function (Repr.Func name f sig)) ctx =
  Context.add (NameSpace.Pub name) (decideRecordOrDef f sig) ctx
updateTopLevel (Repr.ModuleOpen (Repr.Open _mod)) ctx =
  -- TODO ∷ Update this case!
  ctx
updateTopLevel Repr.TypeClass ctx = ctx
updateTopLevel Repr.TypeClassInstance ctx = ctx

-- TODO ∷ why is the context empty?
-- we should somehow note what lists are in scope

-- TODO ∷
-- - once we have type checking, rely on that
-- - for dep types where inference is undecidable, force signature
-- - for functions like (f x) where that evals to a module, where it
--   is dependent but decidable, force signature?

-- | decideRecordOrDef tries to figure out
-- if a given defintiion is a record or a definition
decideRecordOrDef ::
  NonEmpty (Repr.FunctionLike Repr.Expression) -> Maybe Repr.Signature -> Definition
decideRecordOrDef xs ty
  | len == 1 && emptyArgs args =
    -- For the two matched cases eventually
    -- turn these into record expressions
    case body of
      Repr.ExpRecord (Repr.ExpressionRecord i) ->
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at types
        let f (Repr.NonPunned s e) =
              NameSpace.insert
                (NameSpace.Pub (NonEmpty.head s))
                (decideRecordOrDef (Repr.Like [] e :| []) Nothing)
         in Context.Record (foldr f NameSpace.empty i) ty
      Repr.Let _l ->
        def
      _ -> def
  | otherwise = def
  where
    len = length xs
    Repr.Like args body = NonEmpty.head xs
    def = Context.Def Nothing ty xs Context.default'

----------------------------------------
-- Helpers
----------------------------------------

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False
