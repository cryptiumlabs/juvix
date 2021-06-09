module Juvix.ToCore.FromFrontend.Transform.TypeSig where

import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Juvix.ToCore.FromFrontend.Transform.HR
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( ReduceEff,
    eleToSymbol,
  )
import Juvix.ToCore.Types
  ( CoreSig (..),
    CoreSigHR,
    Error (..),
    HasCoreSigs,
    HasParam,
    HasPatVars,
    HasThrowFF,
    throwFF,
  )
import Prelude (error)

transformTypeSig ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m, Show primTy, Show primVal) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  Sexp.T ->
  m [CoreSigHR primTy primVal]
transformTypeSig q _name (typeCon Sexp.:> args Sexp.:> typeForm)
  | Just typeArgs <- Sexp.toList args >>= traverse eleToSymbol = do
    (baseTy, hd) <- transformIndices typeArgs typeCon
    let dataType = foldr makeTPi baseTy typeArgs
    (dataCons, conSigs) <- unzip <$> transformConSigs q hd typeCon typeForm
    let dataSig = DataSig {dataType, dataCons}
    pure $ dataSig : conSigs
  where
    -- ff k (x Sexp.:> xs)
    --   | k == x = Just xs
    --   | otherwise = ff k xs
    -- ff _ _ = Nothing
    -- transformIndices typeArgs (_ Sexp.:> grouped)
    --   | Just dataArrow <- ff (Sexp.atom ":type") grouped = do

    --     typ <- transformTermHR q dataArrow
    --     let hd0 = HR.Var name
    --     let args = HR.Elim . HR.Var . NameSymbol.fromSymbol <$> typeArgs
    --     pure (typ, Just $ HR.Elim $ foldl HR.App hd0 args)
    transformIndices _ _ =
      pure (HR.Star 0, Just $ HR.Star 0) -- TODO metavar for universe
    makeTPi name res =
      -- TODO metavars for the named args instead of defaulting to types
      HR.Pi mempty (NameSymbol.fromSymbol name) (HR.Star 0) res
transformTypeSig _ _ _ = error "malformed type"

transformConSigs ::
  (ReduceEff primTy primVal m, HasPatVars m, HasParam primTy primVal m, Show primTy, Show primVal) =>
  -- | namespace containing declaration
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  -- | rhs
  Sexp.T ->
  m [(NameSymbol.T, CoreSigHR primTy primVal)]
transformConSigs pfx hd typeCon =
  traverse (transformProduct pfx hd typeCon) <=< toProducts
  where
    -- We have a single constructor, which is a record
    toProducts (r@(record Sexp.:> _) Sexp.:> Sexp.Nil)
      | Sexp.isAtomNamed record ":record-d" = do
        -- E.g.
        -- ((":record-d" "x" "TopLevel.Prelude.Circuit.field" "y" "TopLevel.Prelude.Circuit.field" "z" "TopLevel.Prelude.Circuit.field"),
        --   ":record-d",
        --   ["Datatypes"],
        --   Nothing)
        throwFF $ RecordUnimplemented r
    -- we can't have another standalone product here, so just send to
    -- sum
    toProducts sums
      | Just cons <- Sexp.toList sums = do
        pure $ map toProduct1 cons
    toProducts _ = error "malformed data constrcutor"
    toProduct1 (sumConstructor Sexp.:> value)
      | Just sumSym <- eleToSymbol sumConstructor =
        (sumSym, value)
    toProduct1 _ = error "malformed sum constructor"

transformProduct ::
  ( HasPatVars m,
    HasThrowFF primTy primVal m,
    HasParam primTy primVal m,
    HasCoreSigs primTy primVal m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.Mod ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  (Symbol, Sexp.T) ->
  m (NameSymbol.T, CoreSigHR primTy primVal)
transformProduct q hd typeCon (x, prod) =
  (NameSymbol.qualify1 q x,) . makeSig
    <$> transformConSig q (NameSymbol.fromSymbol x) hd typeCon prod
  where
    makeSig ty = ConSig {conType = Just ty}

transformConSig ::
  (ReduceEff primTy primVal m, HasPatVars m, Show primTy, Show primVal) =>
  NameSymbol.Mod ->
  NameSymbol.T ->
  -- | datatype head
  Maybe (HR.Term primTy primVal) ->
  -- | type constructor
  Sexp.T ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformConSig q name mHd typeCon r@((t Sexp.:> ts) Sexp.:> _)
  | named ":record-d" = do
    let convertedSexp = Sexp.list [arrow, Sexp.list $ removeFieldNames ts, Sexp.car typeCon]
    transformConSig q name mHd typeCon convertedSexp
  -- throwFF $ RecordUnimplemented r

  | named ":arrow" = transformTermHR q ts
  | isNothing mHd = do
    transformTermHR q ts
  where
    arrow = Sexp.atom "TopLevel.Prelude.->"
    removeFieldNames fields
      | Just l <- Sexp.toList (Sexp.groupBy2 fields) = Sexp.cdr <$> l
      | otherwise = notImplemented

    -- g (Sexp.List [s, e]) = e
    named = Sexp.isAtomNamed t
transformConSig q name mHd _ r@(t Sexp.:> ts)
  -- TODO: Should check if there any data constructor with a signature (See GADT)
  | isNothing mHd = do
    throwFF $ InvalidConstructor name r
  | Just hd <- mHd,
    Just xs <- Sexp.toList r =
    let makeArr (x, arg) res =
          HR.Pi (Usage.SNat 1) x <$> transformTermHR q arg <*> pure res
        names = makeFieldName <$> [0 ..]
        makeFieldName i = NameSymbol.fromText $ "$field" <> show (i :: Int)
     in foldrM makeArr hd $ zip names xs
  where
    named = Sexp.isAtomNamed t
transformConSig _ _ _ _ r = do
  error "malformed transformConSig"
