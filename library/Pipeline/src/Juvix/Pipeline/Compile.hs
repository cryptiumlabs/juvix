{-# LANGUAGE RecordWildCards #-}

module Juvix.Pipeline.Compile
  ( toCoreDef,
    isMain,
    typePrims,
    unsafeEvalGlobal,
  )
where

import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.Base as Core
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.ToCore.Types (CoreDef (..))
import qualified Prelude as P

type Debug primTy primVal =
  ( Show primTy,
    Show primVal,
    Show (Param.ApplyErrorExtra primTy),
    Show (Param.ApplyErrorExtra primVal),
    Show (Param.Arg primTy),
    Show (Param.Arg primVal)
  )

toCoreDef ::
  Alternative f =>
  CoreDef ext primTy primVal ->
  f (Core.RawGlobal' ext primTy primVal)
toCoreDef (CoreDef g) = pure g
toCoreDef _ = empty

isMain :: Core.RawGlobal' ext primTy primVal -> Bool
isMain (Core.RawGFunction (Core.RawFunction (_ :| ["main"]) _ _ _)) = True
isMain _ = False

-- | Evaluate terms of a global definition
unsafeEvalGlobal ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawGlobal primTy primVal ->
  IR.Global primTy primVal
unsafeEvalGlobal globals g =
  case g of
    Core.RawGDatatype (Core.RawDatatype n pos a l cons) ->
      Core.GDatatype (Core.Datatype n pos (argEval globals <$> a) l (conEval globals <$> cons))
    Core.RawGDataCon (Core.RawDataCon n t d) ->
      Core.GDataCon $ Core.DataCon n (unsafeEval globals t) (funEval globals <$> d)
    Core.RawGFunction (Core.RawFunction n u t cs) ->
      Core.GFunction $
        Core.Function n u (unsafeEval globals t) (map (funClauseEval globals) cs)
    Core.RawGAbstract (Core.RawAbstract n u t) ->
      Core.GAbstract $ Core.Abstract n u (unsafeEval globals t)

-- | Type primitive values of a global definition
typePrims ::
  (Show ty, Show val) =>
  ty ->
  IR.RawGlobal ty val ->
  IR.RawGlobal ty (Param.TypedPrim ty val)
typePrims ty g =
  case g of
    Core.RawGDatatype (Core.RawDatatype n pos a l cons) ->
      Core.RawGDatatype (Core.RawDatatype n pos (argReturn ty <$> a) l (conReturn ty <$> cons))
    Core.RawGDataCon (Core.RawDataCon n t d) ->
      Core.RawGDataCon (Core.RawDataCon n (baseToReturn ty t) (funReturn ty <$> d))
    Core.RawGFunction (Core.RawFunction n u t cs) ->
      Core.RawGFunction (Core.RawFunction n u (baseToReturn ty t) (funClauseReturn ty <$> cs))
    Core.RawGAbstract (Core.RawAbstract n u t) ->
      Core.RawGAbstract (Core.RawAbstract n u (baseToReturn ty t))

argReturn ::
  ty ->
  IR.RawDataArg ty val ->
  IR.RawDataArg ty (Param.TypedPrim ty val)
argReturn ty arg@Core.RawDataArg {rawArgType} =
  arg {Core.rawArgType = baseToReturn ty rawArgType}

argEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawDataArg primTy primVal ->
  IR.DataArg primTy primVal
argEval globals arg@Core.RawDataArg {rawArgName, rawArgUsage, rawArgType} =
  Core.DataArg rawArgName rawArgUsage (unsafeEval globals rawArgType)

conReturn ::
  ty ->
  IR.RawDataCon ty val ->
  IR.RawDataCon ty (Param.TypedPrim ty val)
conReturn ty con@Core.RawDataCon {rawConType, rawConDef} =
  con {Core.rawConType = baseToReturn ty rawConType, Core.rawConDef = funReturn ty <$> rawConDef}

conEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawDataCon primTy primVal ->
  IR.DataCon primTy primVal
conEval globals con@Core.RawDataCon {rawConName, rawConType, rawConDef} =
  Core.DataCon rawConName (unsafeEval globals rawConType) (funEval globals <$> rawConDef)

funReturn ::
  ty ->
  IR.RawFunction ty val ->
  IR.RawFunction ty (Param.TypedPrim ty val)
funReturn ty (Core.RawFunction name usage term clauses) =
  Core.RawFunction name usage (baseToReturn ty term) (funClauseReturn ty <$> clauses)

funEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawFunction primTy primVal ->
  IR.Function primTy primVal
funEval globals (Core.RawFunction name usage term clauses) =
  Core.Function name usage (unsafeEval globals term) (funClauseEval globals <$> clauses)

funClauseReturn ::
  ty ->
  IR.RawFunClause ty val ->
  IR.RawFunClause ty (Param.TypedPrim ty val)
funClauseReturn ty (Core.RawFunClause tel patts term catchall) =
  Core.RawFunClause (telescopeReturn ty tel) (map (pattEval ty) patts) (baseToReturn ty term) catchall

funClauseEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawFunClause primTy primVal ->
  IR.FunClause primTy primVal
funClauseEval globals (Core.RawFunClause tel patts rhs catchall) =
  Core.FunClause
    (telescopeEval globals tel)
    patts
    rhs
    Nothing -- TODO:-- | @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
    catchall
    Nothing --TODO

telescopeReturn ::
  ty ->
  Core.RawTelescope IR.T ty val ->
  Core.RawTelescope IR.T ty (Param.TypedPrim ty val)
telescopeReturn ty = fmap f
  where
    f t@Core.RawTeleEle {rawTy, rawExtension} = t {Core.rawTy = baseToReturn ty rawTy, Core.rawExtension = rawExtension}

telescopeEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  Core.RawTelescope IR.T primTy primVal ->
  Core.Telescope IR.T IR.T primTy primVal
telescopeEval globals ts = f <$> ts
  where
    f rawT@Core.RawTeleEle {..} =
      Core.TeleEle
        { name = rawName,
          usage = rawUsage,
          ty = unsafeEval globals rawTy,
          extension = rawExtension
        }

pattEval ::
  ty ->
  IR.Pattern ty val ->
  IR.Pattern ty (Param.TypedPrim ty val)
pattEval ty patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map (pattEval ty) ps)
    IR.PPair x y -> IR.PPair (pattEval ty x) (pattEval ty y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn ty t)
    -- TODO
    IR.PPrim p -> IR.PPrim (CoreApp.Return (Param.PrimType $ ty :| []) p)

baseToReturn ::
  ty ->
  Core.Term' IR.T ty val ->
  Core.Term' IR.T ty (Param.TypedPrim ty val)
baseToReturn ty t =
  case t of
    IR.Star u -> IR.Star u
    IR.PrimTy p -> IR.PrimTy p
    IR.Prim p -> IR.Prim (CoreApp.Return (Param.PrimType $ ty :| []) p)
    IR.Pi u x y -> IR.Pi u (baseToReturn ty x) (baseToReturn ty y)
    IR.Lam t -> IR.Lam (baseToReturn ty t)
    IR.Sig u x y -> IR.Sig u (baseToReturn ty x) (baseToReturn ty y)
    IR.Pair x y -> IR.Pair (baseToReturn ty x) (baseToReturn ty y)
    IR.Let u a b -> IR.Let u (elimToReturn ty a) (baseToReturn ty b)
    IR.UnitTy -> IR.UnitTy
    IR.Unit -> IR.Unit
    IR.Elim e -> IR.Elim (elimToReturn ty e)

elimToReturn ::
  ty ->
  Core.Elim' IR.T ty val ->
  Core.Elim' IR.T ty (Param.TypedPrim ty val) -- ty' --(TypedPrim ty val)
elimToReturn ty e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn ty e) (baseToReturn ty t)
    IR.Ann u a b c -> IR.Ann u (baseToReturn ty a) (baseToReturn ty b) c

unsafeEval ::
  ( IR.CanEval IR.T IR.T primTy primVal,
    Debug primTy primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal
unsafeEval globals t = case IR.evalTerm (IR.rawLookupFun' globals) t of
  Right v -> v
  Left v -> panic "Failed to eval term"
