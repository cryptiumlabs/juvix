{-# LANGUAGE LiberalTypeSynonyms #-}

module Juvix.FrontendContextualise.Contextify.Transform where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.FrontendContextualise.Contextify.Types as Type
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Repr
import Juvix.Library

-- the name symbols are the modules we are opening
-- TODO ∷ parallize this
f ,
  contextify ::
    Type.Context ->
    (Context.NameSymbol, [Repr.TopLevel]) ->
    Either Context.PathError Type.Pass
contextify cont (nameSymb, xs) =
  case Context.switchNameSpace nameSymb cont of
    Left errr -> Left errr
    Right ctx -> Right (foldr f Type.P {ctx, opens = [], modsDefined = []} xs)
  where
    f top Type.P {ctx, opens, modsDefined} =
      let Type.P {ctx = ctx', opens = opens', modsDefined = modsDefined'} =
            updateTopLevel top ctx
       in Type.P
            { ctx = ctx',
              opens = opens <> opens',
              modsDefined = modsDefined <> modsDefined'
            }
f = contextify

-- we can't just have a list, we need to have a map with implicit opens as
-- well...
updateTopLevel :: Repr.TopLevel -> Type.Context -> Type.Pass
updateTopLevel (Repr.Type t@(Repr.Typ _ name _ _)) ctx =
  Type.P
    { ctx = Context.add (NameSpace.Pub name) (Context.TypeDeclar t) ctx,
      opens = [],
      modsDefined = []
    }
updateTopLevel (Repr.Function (Repr.Func name f sig)) ctx =
  let (def, modsDefined) =
        decideRecordOrDef name (Context.currentName ctx) f sig
   in Type.P
        { ctx = Context.add (NameSpace.Pub name) def ctx,
          opens = [],
          modsDefined
        }
updateTopLevel (Repr.ModuleOpen (Repr.Open mod)) ctx =
  -- Mod isn't good enough, have to resolve it to the full symbol
  Type.P
    { ctx = ctx,
      opens = [mod],
      modsDefined = []
    }
updateTopLevel Repr.TypeClass ctx =
  Type.P ctx [] []
updateTopLevel Repr.TypeClassInstance ctx =
  Type.P ctx [] []

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
  Symbol ->
  NameSymbol.T ->
  NonEmpty (Repr.FunctionLike Repr.Expression) ->
  Maybe Repr.Signature ->
  (Type.Definition, [NameSymbol.T])
decideRecordOrDef recordName currModName xs ty
  | len == 1 && emptyArgs args =
    -- For the two matched cases eventually
    -- turn these into record expressions
    case body of
      Repr.ExpRecord (Repr.ExpressionRecord i) ->
        -- the type here can eventually give us arguments though looking at the
        -- lambda for e, and our type can be found out similarly by looking at
        -- types
        (Context.Record nameSpace ty, newRecordName : innerMods)
        where
          newRecordName = currModName <> pure recordName
          --
          (nameSpace, innerMods) = foldr f (NameSpace.empty, []) i
          --
          f (Repr.NonPunned s e) (nameSpace, prevModNames) =
            let fieldN = NonEmpty.last s
                like = Repr.Like [] e :| []
             in Nothing
                  |> decideRecordOrDef fieldN newRecordName like
                  |> bimap
                    (\d -> NameSpace.insert (NameSpace.Pub fieldN) d nameSpace)
                    (\inNames -> inNames <> prevModNames)
      Repr.Let _l ->
        def
      _ -> def
  | otherwise = def
  where
    len = length xs
    Repr.Like args body = NonEmpty.head xs
    def = (Context.Def Nothing ty xs Context.default', [])

----------------------------------------
-- Helpers
----------------------------------------

emptyArgs :: [a] -> Bool
emptyArgs [] = True
emptyArgs (_ : _) = False
