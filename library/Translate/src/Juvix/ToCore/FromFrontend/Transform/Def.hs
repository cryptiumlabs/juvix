module Juvix.ToCore.FromFrontend.Transform.Def where


import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.IR as IR
import Juvix.Core.Translate (hrToIR)
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Juvix.ToCore.FromFrontend.Transform.HR ( transformTermHR )
import Juvix.ToCore.Types
    ( throwFF,
      CoreDef(..),
      HasNextPatVar,
      HasPatVars,
      Error(..),
      CoreSig(..) )
import Prelude (error)
import Debug.Pretty.Simple (pTraceShowM, pTraceShow)
import Juvix.ToCore.FromFrontend.Transform.Helpers
    ( getParamConstant,
      lookupSig,
      getValSig,
      getConSig,
      getDataSig,
      splitDataType,
      ReduceEff,
      conDefName,
      eleToSymbol )

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
  traceM "Transform Def"
  pTraceShowM (x, sig)
  case sig of
    Just (SpecialSig s) -> pure [SpecialDef x s]
    _ -> map CoreDef <$> transformNormalDef q x def
    where
    q = NameSymbol.mod x

    transformNormalDef q x (Ctx.TypeDeclar dec) 
      = pTraceShow ("transformNormalDef", x, dec) transformType x dec
      where
        transformCon x ty def = do
          traceM "TransformCon"
          pTraceShowM (x, conDefName x, def)
          -- def <- traverse (transformFunction q (conDefName x)) def
          pure $
            IR.RawDataCon
              { rawConName = x,
                rawConType = hrToIR ty,
                rawConDef = Nothing--def
              }
        transformType name _ = do
          (ty, conNames) <- getDataSig q name
          traceM "ConNames"
          pTraceShowM (ty, conNames)
          let getConSig' x = do (ty, def) <- getConSig q x; pure (x, ty, def)
          conSigs <- traverse getConSig' conNames
          traceM "ConSigs"
          pTraceShowM (conSigs)
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
        traceM "Get Val Sig"
        pTraceShowM (x, defs)
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
        patts <- traverse transformArg args
        clauseBody <- transformTermIR q body
        pure $ IR.RawFunClause [] patts clauseBody False
        where
        -- | Transform S-expression form into IR
        transformTermIR q fe = do
          hrToIR <$> transformTermHR q fe
    transformClause _ _ = error "malformed tansformClause"

    transformArg p@(name Sexp.:> _rest)
      | Sexp.isAtomNamed name ":implicit-a" =
        throwFF $ PatternUnimplemented p
    transformArg pat = transformPat pat

    transformPat p@(asCon Sexp.:> con)
      -- implicit arguments are not supported
      -- TODO ∷ translate as patterns into @let@
      | Sexp.isAtomNamed asCon ":as" =
        throwFF $ PatternUnimplemented p
      | Just args <- Sexp.toList con,
        Just Sexp.A {atomName} <- Sexp.atomFromT asCon =
        IR.PCon atomName <$> traverse transformPat args
    transformPat n
      | Just x <- eleToSymbol n = do
        var <- getNextPatVar
        modify @"patVars" $ HM.insert (NameSymbol.fromSymbol x) var
        pure $ IR.PVar var
      | Just n@Sexp.N {} <- Sexp.atomFromT n =
        IR.PPrim <$> getParamConstant n
      | otherwise = error "malformed match pattern"

    getNextPatVar :: HasNextPatVar m => m IR.PatternVar
    getNextPatVar = state @"nextPatVar" \v -> (v, succ v)

