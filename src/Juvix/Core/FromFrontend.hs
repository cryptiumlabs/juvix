{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# LANGUAGE LiberalTypeSynonyms, UndecidableInstances #-}

module Juvix.Core.FromFrontend where

import Juvix.Library
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as FE
import qualified Juvix.FrontendContextualise as FE
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.HR as HR
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.Core.Parameterisation as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


data Error primTy primVal =
    UnknownUnsupported (Maybe IR.GlobalName)
  | ConstraintsUnsupported [FE.Expression]
  | RefinementsUnsupported FE.TypeRefine
  | UniversesUnsupported FE.UniverseExpression
  | UnsupportedConstant FE.Constant
  | UnknownPrimitive NameSymbol.T

data CoreSig' ext primTy primVal =
    DataSig (IR.Term' ext primTy primVal)
  | ValSig  IR.GlobalUsage (IR.Term' ext primTy primVal)
  deriving Generic

deriving instance
  ( Eq primTy, Eq primVal,
    IR.TermAll Eq ext primTy primVal,
    IR.ElimAll Eq ext primTy primVal
  ) =>
  Eq (CoreSig' ext primTy primVal)

deriving instance
  ( Show primTy, Show primVal,
    IR.TermAll Show ext primTy primVal,
    IR.ElimAll Show ext primTy primVal
  ) =>
  Show (CoreSig' ext primTy primVal)

deriving instance
  ( Data ext, Data primTy, Data primVal,
    IR.TermAll Data ext primTy primVal,
    IR.ElimAll Data ext primTy primVal
  ) =>
  Data (CoreSig' ext primTy primVal)

type CoreSigIR = CoreSig' IR.NoExt
type CoreSigHR = CoreSig' HR.T

type CoreSigs' ext primTy primVal =
  HashMap IR.GlobalName (CoreSig' ext primTy primVal)

type CoreSigsIR primTy primVal = CoreSigs' IR.NoExt primTy primVal
type CoreSigsHR primTy primVal = CoreSigs' HR.T     primTy primVal


data FFState primTy primVal =
  FFState {
    frontend :: FE.FinalContext,
    param    :: P.Parameterisation primTy primVal,
    coreSigs :: CoreSigsIR primTy primVal,
    core     :: IR.Globals primTy primVal
  }
  deriving Generic

type EnvAlias primTy primVal =
  ExceptT (Error primTy primVal) (State (FFState primTy primVal))

newtype Env primTy primVal a =
    Env {unEnv :: EnvAlias primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasThrow "fromFrontendError" (Error primTy primVal))
    via MonadError (EnvAlias primTy primVal)
  deriving
    ( HasSource "frontend" FE.FinalContext,
      HasReader "frontend" FE.FinalContext
    )
    via ReaderField "frontend" (EnvAlias primTy primVal)
  deriving
    ( HasSource "param" (P.Parameterisation primTy primVal),
      HasReader "param" (P.Parameterisation primTy primVal)
    )
    via ReaderField "param" (EnvAlias primTy primVal)
  deriving
    ( HasSource "core" (IR.Globals primTy primVal),
      HasSink   "core" (IR.Globals primTy primVal),
      HasState  "core" (IR.Globals primTy primVal)
    )
    via StateField "core" (EnvAlias primTy primVal)

throwFF :: Error primTy primVal -> Env primTy primVal a
throwFF = throw @"fromFrontendError"


paramConstant :: P.Parameterisation primTy primVal
              -> FE.Constant -> Maybe primVal
paramConstant p (FE.Number (FE.Double'  d)) = P.floatVal  p d
paramConstant p (FE.Number (FE.Integer' i)) = P.intVal    p i
paramConstant p (FE.String (FE.Sho      s)) = P.stringVal p s


transformTerm :: FE.Expression -> Env primTy primVal (IR.Term primTy primVal)
transformTerm = fmap hrToIR . transformTermHR

transformTermHR :: FE.Expression -> Env primTy primVal (HR.Term primTy primVal)
transformTermHR (FE.Constant k) = do
  p <- ask @"param"
  case paramConstant p k of
    Just x  -> pure $ HR.Prim x
    Nothing -> throwFF $ UnsupportedConstant k
transformTermHR (FE.Let l) = _
transformTermHR (FE.LetType l) = _
transformTermHR (FE.Match m) = _
transformTermHR (FE.Name n) = pure $ HR.Elim $ HR.Var _n
transformTermHR (FE.Lambda l) = _
transformTermHR (FE.Application (FE.App f xs)) = do
  f' <- toElim =<< transformTermHR f
  HR.Elim . foldl HR.App f' <$> traverse transformTermHR xs
transformTermHR (FE.Primitive p) = _
transformTermHR (FE.List (FE.ListLit es)) =
  makeList <$> traverse transformTermHR es
transformTermHR (FE.Tuple (FE.TupleLit es)) =
  makeTuple <$> traverse transformTermHR es
transformTermHR (FE.Block (FE.Bloc e)) =
  transformTermHR e
transformTermHR (FE.ExpRecord (FE.ExpressionRecord fs)) =
  makeRecord <$> traverse traverseField (toList fs)
 where
  traverseField (FE.NonPunned n e) = (n,) <$> transformTermHR e
transformTermHR (FE.ArrowE a) = transformArrow a
transformTermHR (FE.NamedTypeE (FE.NamedType' _ e)) =
  -- TODO the name will only become relevant (outside of arrows)
  -- when refinements are supported
  transformTermHR e
transformTermHR (FE.RefinedE r) =
  throwFF $ RefinementsUnsupported r
transformTermHR (FE.UniverseName i) =
  -- TODO for universe polymorphism
  throwFF $ UniversesUnsupported i
transformTermHR (FE.Parened e) = transformTermHR e

transformArrow :: FE.ArrowExp -> Env primTy primVal (HR.Term primTy primVal)
transformArrow (FE.Arr' xa π b) =
  case xa of
    -- TODO implicit arrows
    FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b
    a                                 -> go π _unusedName a b
 where
  getName (FE.Implicit x) = x
  getName (FE.Concrete x) = x
  go π x a b =
    HR.Pi <$> transformUsage π
          <*> pure x
          <*> transformTermHR a
          <*> transformTermHR b

makeList :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeList = _

makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeTuple [t] = t
makeTuple _ts = _

makeRecord :: [(NameSymbol.T, HR.Term primTy primVal)]
           -> HR.Term primTy primVal
makeRecord = _

toElim :: HR.Term primTy primVal -> Env primTy primVal (HR.Elim primTy primVal)
toElim (HR.Elim e) = pure e
toElim t = do
  π <- _
  ty <- _
  ℓ <- _
  pure $ HR.Ann π t ty ℓ

transformUsage :: FE.Expression -> Env primTy primVal Usage.T
transformUsage = _


transformGUsage :: FE.Expression -> Env primTy primVal IR.GlobalUsage
transformGUsage = _


transformSig :: FE.Final Ctx.Definition
             -> Env primTy primVal (CoreSigHR primTy primVal)
transformSig (Ctx.Def π msig _ _) = transformValSig π msig -- why two usages?
transformSig (Ctx.Record _ msig) = transformValSig Nothing msig
transformSig (Ctx.TypeDeclar (FE.Typ π _ args dat)) =
  -- args :: [Symbol], dat :: FE.Data
  DataSig <$> _
transformSig (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported $ FE.signatureName <$> sig
transformSig Ctx.CurrentNameSpace = _ -- TODO ???

transformType :: forall primTy primVal.
  FE.Type -> Env primTy primVal (HR.Term primTy primVal)
transformType (FE.Typ π _ args dat) = do
  ty' <- transformTermHR $ datType dat
  foldrM makePi ty' args
 where
  makePi :: Symbol -> HR.Term primTy primVal
         -> Env primTy primVal (HR.Term primTy primVal)
  makePi a b = HR.Pi _π a <$> freshMeta <*> pure b

  datType :: FE.Data -> FE.Expression
  -- TODO should Arrowed be allowed to be repeated?
  datType (FE.Arrowed a b) = FE.ArrowE (FE.Arr' a _ _star)
  datType (FE.NonArrowed _) = _star

transformValSig :: Maybe Usage.T -> Maybe FE.Signature
                -> Env primTy primVal (CoreSigHR primTy primVal)
transformValSig π (Just (FE.Sig _ π' ty cons))
  | null cons = ValSig _π <$> transformTermHR ty
  | otherwise = throwFF $ ConstraintsUnsupported cons
transformValSig π Nothing = ValSig _π <$> freshMeta

-- will have to be an HR+Unify term but TODO
freshMeta :: Env primTy primVal (HR.Term primTy primVal)
freshMeta = _

transformDef :: FE.Final Ctx.Definition
             -> Env primTy primVal [IR.Global primTy primVal]
transformDef (Ctx.Def π _ def _)  = _
transformDef (Ctx.Record rec _)   = _
transformDef (Ctx.TypeDeclar typ) = _
transformDef (Ctx.Unknown _)      = pure []
transformDef Ctx.CurrentNameSpace = pure [] -- FIXME ???
