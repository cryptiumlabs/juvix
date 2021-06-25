module Juvix.ToCore.FromFrontend.Transform.Def (transformDef) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.Core.Translate as Translate
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.ToCore.FromFrontend.Transform.HR (transformTermHR)
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( ReduceEff,
    eleToSymbol,
    getConSig,
    getDataSig,
    getParamConstant,
    getValSig,
    lookupSig,
    splitDataType,
  )
import Juvix.ToCore.FromFrontend.Transform.IR (transformTermIR)
import Juvix.ToCore.Types
  ( CoreDef (..),
    CoreSig (..),
    Error (..),
    HasNextPatVar,
    HasParam,
    HasPatVars,
    HasThrowFF,
    throwFF,
  )
import Prelude (error)

transformDef ::
  ( ReduceEff primTy primVal m,
    HasNextPatVar m,
    HasPatVars m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreDef primTy primVal]
transformDef x def = do
  sig <- lookupSig Nothing x
  case sig of
    Just (SpecialSig s) -> pure [SpecialDef x s]
    _ -> map CoreDef <$> transformNormalDef q x def
  where
    q = NameSymbol.mod x

    transformNormalDef q x (Ctx.TypeDeclar dec) =
      transformType x dec
      where
        transformCon x ty _def = do
          -- def <- traverse (transformFunction q (conDefName x)) def
          pure $
            IR.RawDataCon
              { rawConName = x,
                rawConType = hrToIR ty,
                rawConDef = Nothing --def
              }
        transformType name _ = do
          (ty, conNames) <- getDataSig q name
          let getConSig' x = do ty <- getConSig q x; pure (x, ty, def)
          conSigs <- traverse getConSig' conNames
          cons <- traverse (uncurry3 transformCon) conSigs
          (args, ℓ) <- splitDataType name ty
          let dat' =
                IR.RawDatatype
                  { rawDataName = name,
                    rawDataArgs = args,
                    rawDataLevel = ℓ,
                    rawDataCons = cons,
                    -- TODO ∷ replace
                    rawDataPos = []
                  }
          pure $ IR.RawGDatatype dat' : fmap IR.RawGDataCon cons
    transformNormalDef _ _ Ctx.CurrentNameSpace = pure []
    transformNormalDef _ _ Ctx.Information {} = pure []
    transformNormalDef _ _ (Ctx.Unknown _) = pure []
    transformNormalDef _ _ (Ctx.Record _) = pure [] -- TODO
    transformNormalDef _ _ Ctx.SumCon {} = pure []
    transformNormalDef q x (Ctx.Def def) = do
      f <- transformFunction q x def
      pure [IR.RawGFunction f]

    transformFunction q x (Ctx.D _ _ (_lambdaCase Sexp.:> defs) _)
      | Just xs <- Sexp.toList defs >>= NonEmpty.nonEmpty = do
        (π, typ) <- getValSig q x
        clauses <- traverse (transformClause q) xs
        pure $
          IR.RawFunction
            { rawFunName = x,
              rawFunUsage = π,
              rawFunType = hrToIR typ,
              rawFunClauses = clauses
            }
    transformFunction _ _ _ = error "malformed defun"

    transformClause q (Sexp.List [args', body])
      | Just args <- Sexp.toList args' = do
        put @"patVars" mempty
        put @"nextPatVar" 0
        pattsHR <- traverse transformArgHR args
        let (patts, pattsTable) = Translate.hrPatternsToIR pattsHR
            transformTermIR q fe =
              Translate.hrToIRWith pattsTable <$> transformTermHR q fe
        clauseBody <- transformTermIR q body
        pure $ IR.RawFunClause [] patts clauseBody False
    transformClause _ _ = error "malformed tansformClause"

    transformArgHR p@(name Sexp.:> _rest)
      | Sexp.isAtomNamed name ":implicit-a" =
        throwFF $ PatternUnimplemented p
    transformArgHR pat = transformPatHR pat

    transformPatHR ::
      (HasThrowFF primTy primVal m, HasParam primTy primVal m) =>
      Sexp.T ->
      m (HR.Pattern primTy primVal)
    transformPatHR p@(asCon Sexp.:> con)
      -- implicit arguments are not supported
      -- TODO ∷ translate as patterns into @let@
      | Sexp.isAtomNamed asCon ":as" =
        throwFF $ PatternUnimplemented p
      | Just args <- Sexp.toList con,
        Just Sexp.A {atomName} <- Sexp.atomFromT asCon =
        HR.PCon atomName <$> traverse transformPatHR args
    transformPatHR n
      | Just x <- Sexp.nameFromT n = do
        pure $ HR.PVar x
      | Just n@Sexp.N {} <- Sexp.atomFromT n =
        HR.PPrim <$> getParamConstant n
      | otherwise =
        error "malformed match pattern"
