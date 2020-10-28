{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.Core.FromFrontend where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.FrontendContextualise as FE
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as FE
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data Error
  = -- features not yet implemented

    -- | constraints are not yet implemented
    ConstraintsUnsupported [FE.Expression]
  | -- | refinements are not yet implemented
    RefinementsUnsupported FE.TypeRefine
  | -- | universe polymorphism is not yet implemented
    UniversesUnsupported FE.UniverseExpression
  | -- | implicit arguments are not yet implemented
    ImplicitsUnsupported FE.ArrowExp
  | -- | type inference for definitions is not yet implemented
    SigRequired (FE.Final Ctx.Definition)
  | -- | head of application not an Elim
    NotAnElim FE.Expression
  | -- actual errors

    -- | unknown found at declaration level
    UnknownUnsupported (Maybe IR.GlobalName)
  | -- | current backend doesn't support this type of constant
    UnsupportedConstant FE.Constant
  | -- | current backend doesn't have this primitive
    UnknownPrimitive NameSymbol.T
  | -- | expression is not a usage
    NotAUsage FE.Expression
  | -- | expression is not 0 or ω
    NotAGUsage FE.Expression

data CoreSig' ext primTy primVal
  = DataSig (IR.Term' ext primTy primVal)
  | ValSig IR.GlobalUsage (IR.Term' ext primTy primVal)
  deriving (Generic)

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.TermAll Eq ext primTy primVal,
    IR.ElimAll Eq ext primTy primVal
  ) =>
  Eq (CoreSig' ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.TermAll Show ext primTy primVal,
    IR.ElimAll Show ext primTy primVal
  ) =>
  Show (CoreSig' ext primTy primVal)

deriving instance
  ( Data ext,
    Data primTy,
    Data primVal,
    IR.TermAll Data ext primTy primVal,
    IR.ElimAll Data ext primTy primVal
  ) =>
  Data (CoreSig' ext primTy primVal)

type CoreSigIR = CoreSig' IR.NoExt

type CoreSigHR = CoreSig' HR.T

type CoreSigs' ext primTy primVal =
  HashMap IR.GlobalName (CoreSig' ext primTy primVal)

type CoreSigsIR primTy primVal = CoreSigs' IR.NoExt primTy primVal

type CoreSigsHR primTy primVal = CoreSigs' HR.T primTy primVal

data FFState primTy primVal
  = FFState
      { frontend :: FE.FinalContext,
        param :: P.Parameterisation primTy primVal,
        coreSigs :: CoreSigsIR primTy primVal,
        core :: IR.Globals primTy primVal
      }
  deriving (Generic)

type EnvAlias primTy primVal =
  ExceptT Error (State (FFState primTy primVal))

newtype Env primTy primVal a
  = Env {unEnv :: EnvAlias primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (HasThrow "fromFrontendError" Error)
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
      HasSink "core" (IR.Globals primTy primVal),
      HasState "core" (IR.Globals primTy primVal)
    )
    via StateField "core" (EnvAlias primTy primVal)

throwFF :: Error -> Env primTy primVal a
throwFF = throw @"fromFrontendError"

paramConstant ::
  P.Parameterisation primTy primVal ->
  FE.Constant ->
  Maybe primVal
paramConstant p (FE.Number (FE.Double' d)) = P.floatVal p d
paramConstant p (FE.Number (FE.Integer' i)) = P.intVal p i
paramConstant p (FE.String (FE.Sho s)) = P.stringVal p s

transformTerm :: FE.Expression -> Env primTy primVal (IR.Term primTy primVal)
transformTerm = fmap hrToIR . transformTermHR

transformTermHR :: FE.Expression -> Env primTy primVal (HR.Term primTy primVal)
transformTermHR (FE.Constant k) = do
  p <- ask @"param"
  case paramConstant p k of
    Just x -> pure $ HR.Prim x
    Nothing -> throwFF $ UnsupportedConstant k
transformTermHR (FE.Let l) = _
transformTermHR (FE.LetType l) = _
transformTermHR (FE.Match m) = _
transformTermHR (FE.Name n) = pure $ HR.Elim $ HR.Var _n
transformTermHR (FE.Lambda l) = _
transformTermHR (FE.Application (FE.App f xs)) = do
  f' <- toElim f =<< transformTermHR f
  HR.Elim . foldl HR.App f' <$> traverse transformTermHR xs
transformTermHR (FE.Primitive (FE.Prim p)) = do
  param <- ask @"param"
  maybe (throwFF $ UnknownPrimitive p) pure $
    primTy param p <|> primVal param p
  where
    primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
    primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
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
transformArrow f@(FE.Arr' xa π b) =
  case xa of
    FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b
    a -> go π _unusedName a b
  where
    getName (FE.Concrete x) = pure x
    getName (FE.Implicit x) = throwFF $ ImplicitsUnsupported f
    go π x a b =
      HR.Pi <$> transformUsage π
        <*> x
        <*> transformTermHR a
        <*> transformTermHR b

makeList :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeList =
  -- TODO: make lists a builtin syntax form in core (translated to
  -- e.g. michelson lists)
  _

-- | translate (1,2,3,4) to (1,(2,(3,4)))
makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeTuple [] = _ -- TODO: add unit to core, probably
makeTuple [t] = t
makeTuple (t : ts) = IR.Pair t (makeTuple ts)

makeRecord ::
  [(NameSymbol.T, HR.Term primTy primVal)] ->
  HR.Term primTy primVal
makeRecord =
  -- TODO:
  -- 1) an extended version of core with record literals to begin with
  -- 2) translation after typechecking of these to an application of the right
  --    constructor to the fields in the originally declared order
  _

toElim ::
  -- | the original expression
  FE.Expression ->
  HR.Term primTy primVal ->
  Env primTy primVal (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e

-- TODO put an annotation with metas for the usage/type

transformUsage :: FE.Expression -> Env primTy primVal Usage.T
transformUsage (FE.Constant (FE.Number (FE.Integer' i)))
  | i >= 0 =
    pure $ Usage.SNat $ fromInteger i
transformUsage e = throwFF $ NotAUsage e

-- FIXME ω

transformGUsage :: FE.Expression -> Env primTy primVal IR.GlobalUsage
transformGUsage (FE.Constant (FE.Number (FE.Integer' 0))) = pure IR.GZero
transformGUsage e = throwFF $ NotAGUsage e

-- FIXME ω

transformSig ::
  FE.Final Ctx.Definition ->
  Env primTy primVal (CoreSigHR primTy primVal)
transformSig def@(Ctx.Def π msig _ _) =
  -- why two usages?
  transformValSig def π msig
transformSig def@(Ctx.Record _ msig) = transformValSig def Nothing msig
transformSig (Ctx.TypeDeclar typ) = DataSig <$> transformType typ
transformSig (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported $ FE.signatureName <$> sig
transformSig Ctx.CurrentNameSpace = _ -- TODO ???

transformType ::
  forall primTy primVal.
  FE.Type ->
  Env primTy primVal (HR.Term primTy primVal)
transformType (FE.Typ {typeForm = FE.Arrowed {dataArrow}}) =
  transformTermHR dataArrow
transformType typ@(FE.Typ {typeForm = FE.NonArrowed {}}) =
  throwFF $ SigRequired $ Ctx.TypeDeclar typ

transformValSig ::
  FE.Final Ctx.Definition ->
  Maybe Usage.T ->
  Maybe FE.Signature ->
  Env primTy primVal (CoreSigHR primTy primVal)
transformValSig _ π (Just (FE.Sig _ π' ty cons))
  | null cons = ValSig _π <$> transformTermHR ty
  | otherwise = throwFF $ ConstraintsUnsupported cons
transformValSig def π Nothing = throwFF $ SigRequired def

transformDef ::
  FE.Final Ctx.Definition ->
  Env primTy primVal [IR.Global primTy primVal]
transformDef (Ctx.Def π _ def _) = _
transformDef (Ctx.Record rec _) = _
transformDef (Ctx.TypeDeclar typ) = _
transformDef (Ctx.Unknown _) = pure []
transformDef Ctx.CurrentNameSpace = pure [] -- FIXME ???
