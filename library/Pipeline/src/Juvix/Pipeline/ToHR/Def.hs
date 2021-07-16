
module Juvix.Pipeline.ToHR.Def 
( CoreDef(..),
  CoreDefs,
  defName,
  mergeSigs,
  transformDef 
)
where

import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Context as Ctx
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.HR as HR
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Pipeline.ToHR.Term (transformTermHR)

import Prelude (error)

---------------------
-- Core Definition --
---------------------

transformDef ::
  ( ReduceEff HR.T primTy primVal m,
    HasNextPatVar m,
    HasPatVars m,
    Show primTy,
    Show primVal
  ) =>
  NameSymbol.T ->
  Ctx.Definition Sexp.T Sexp.T Sexp.T ->
  m [CoreDef HR.T primTy primVal]
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
            Core.RawDataCon
              { rawConName = x,
                rawConType = ty,
                rawConDef = Nothing --def
              }

        transformType name _ = do
          (ty, conNames) <- getDataSig q name
          let getConSig' x = do ty <- getConSig q x; pure (x, ty, def)
          conSigs <- traverse getConSig' conNames
          cons <- traverse (uncurry3 transformCon) conSigs
          (args, ℓ) <- splitDataTypeHR name ty
          let dat' =
                Core.RawDatatype
                  { rawDataName = name,
                    rawDataArgs = args,
                    rawDataLevel = ℓ,
                    rawDataCons = cons,
                    -- TODO ∷ replace
                    rawDataPos = []
                  }
          pure $ Core.RawGDatatype dat' : fmap Core.RawGDataCon cons
    transformNormalDef _ _ Ctx.CurrentNameSpace = pure []
    transformNormalDef _ _ Ctx.Information {} = pure []
    transformNormalDef _ _ (Ctx.Unknown _) = pure []
    transformNormalDef _ _ (Ctx.Record _) = pure [] -- TODO
    transformNormalDef _ _ Ctx.SumCon {} = pure []
    transformNormalDef q x (Ctx.Def def) = do
      f <- transformFunction q x def
      pure [Core.RawGFunction f]
    transformFunction q x (Ctx.D _ _ (_lambdaCase Sexp.:> defs) _)
      | Just xs <- Sexp.toList defs >>= NonEmpty.nonEmpty = do
        (π, typ) <- getValSig q x
        clauses <- traverse (transformClause q) xs
        pure $
          Core.RawFunction
            { rawFunName = x,
              rawFunUsage = π,
              rawFunType = typ,
              rawFunClauses = clauses
            }
    transformFunction _ _ _ = error "malformed defun"
    transformClause q (Sexp.List [args', body])
      | Just args <- Sexp.toList args' = do
        put @"patVars" mempty
        put @"nextPatVar" 0
        pattsHR <- traverse transformArg args
        clauseBody <- transformTermHR q body
        pure $ Core.RawFunClause [] pattsHR clauseBody False
    transformClause _ _ = error "malformed tansformClause"

transformArg ::
  (HasThrowFF HR.T primTy primVal m, HasParam primTy primVal m) =>
  Sexp.T ->
  m (HR.Pattern primTy primVal)
transformArg p@(name Sexp.:> _rest)
  | Sexp.isAtomNamed name ":implicit-a" =
    throwFF $ PatternUnimplemented p
transformArg pat = transformPat pat

transformPat ::
  (HasThrowFF HR.T primTy primVal m, HasParam primTy primVal m) =>
  Sexp.T ->
  m (HR.Pattern primTy primVal)
transformPat p@(asCon Sexp.:> con)
  -- implicit arguments are not supported
  -- TODO ∷ translate as patterns into @let@
  | Sexp.isAtomNamed asCon ":as" =
    throwFF $ PatternUnimplemented p
  | Just args <- Sexp.toList con,
    Just Sexp.A {atomName} <- Sexp.atomFromT asCon =
    HR.PCon atomName <$> traverse transformPat args
transformPat n
  | Just x <- Sexp.nameFromT n = do
    pure $ HR.PVar x
  | Just n@Sexp.N {} <- Sexp.atomFromT n =
    HR.PPrim <$> getParamConstant n
  | otherwise =
    error "malformed match pattern"



defName :: CoreDef ext primTy primVal -> NameSymbol.T
defName = \case
  CoreDef (Core.RawGDatatype Core.RawDatatype {rawDataName = x}) -> x
  CoreDef (Core.RawGDataCon Core.RawDataCon {rawConName = x}) -> x
  CoreDef (Core.RawGFunction Core.RawFunction {rawFunName = x}) -> x
  CoreDef (Core.RawGAbstract Core.RawAbstract {rawAbsName = x}) -> x
  SpecialDef x _ -> x

hrToIRDef :: CoreDef HR.T ty val -> Core.PatternMap Core.GlobalName -> (Core.PatternMap Core.GlobalName, CoreDef IR.T ty val)
hrToIRDef (SpecialDef sym s) pats = (pats, SpecialDef sym s)
hrToIRDef (CoreDef global) pats = Core.hrToIRDef global pats


splitDataType ::
  (Show primTy, Show primVal, HasThrowFF ext primTy primVal m) =>
  NameSymbol.T ->
  HR.Term primTy primVal ->
  m ([Core.RawDataArg' HR.T primTy primVal], Core.Universe)
splitDataType x ty0 = go ty0
  where
    go (HR.Pi π x s t) = first (arg :) <$> splitDataType x t
      where
        arg =
          Core.RawDataArg
            { rawArgName = x,
              rawArgUsage = π,
              rawArgType = s
            }
    go (HR.Star ℓ) = pure ([], ℓ)
    go _ = throwFF $ InvalidDatatypeType x ty0



-- hrToIRDefs :: CoreDefs HR.T ty val -> (Core.PatternMap Core.GlobalName, CoreDefs IR.T ty val)
-- hrToIRDefs = HM.foldlWithKey' f (mempty, mempty)
--   where
--     f (m, defs) globalName def =
--       let (m', def') = hrToIRDef def m
--        in (m', HM.insert globalName def' defs)
