{-# LANGUAGE RecordWildCards #-}

module Juvix.Pipeline.Compile
  ( Pipeline,
    toCoreDef,
    isMain,
    convGlobal,
    unsafeEvalGlobal,
  )
where

import Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.IR as IR
import Juvix.Core.IR.Types.Base (Elim', Term', XPi)
import Juvix.Core.IR.Types.Globals
import qualified Juvix.Core.Parameterisation as Parameterisation
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import Juvix.ToCore.Types (CoreDef (..))
import qualified Prelude as P

type Pipeline = Feedback.FeedbackT [] P.String IO

toCoreDef ::
  Alternative f =>
  CoreDef primTy primVal ->
  f (IR.RawGlobal primTy primVal)
toCoreDef (CoreDef g) = pure g
toCoreDef _ = empty

isMain :: RawGlobal' ext primTy primVal -> Bool
isMain (IR.RawGFunction (IR.RawFunction (_ :| ["main"]) _ _ _)) = True
isMain _ = False

unsafeEvalGlobal ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawGlobal primTy primVal ->
  IR.Global primTy primVal
unsafeEvalGlobal globals g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) ->
      GDatatype (Datatype n pos (argEval globals <$> a) l (conEval globals <$> cons))
    RawGDataCon (RawDataCon n t d) ->
      GDataCon $ DataCon n (unsafeEval globals t) (funEval globals <$> d)
    RawGFunction (RawFunction n u t cs) ->
      GFunction $
        Function n u (unsafeEval globals t) (map (funClauseEval globals) cs)
    RawGAbstract (RawAbstract n u t) ->
      GAbstract $ Abstract n u (unsafeEval globals t)

convGlobal ::
  (Show ty, Show val) =>
  ty ->
  IR.RawGlobal ty val ->
  IR.RawGlobal ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
convGlobal ty g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) ->
      RawGDatatype (RawDatatype n pos (argReturn ty <$> a) l (conReturn ty <$> cons))
    RawGDataCon (RawDataCon n t d) ->
      RawGDataCon (RawDataCon n (baseToReturn ty t) (funReturn ty <$> d))
    RawGFunction (RawFunction n u t cs) ->
      RawGFunction (RawFunction n u (baseToReturn ty t) (funClauseReturn ty <$> cs))
    RawGAbstract (RawAbstract n u t) ->
      RawGAbstract (RawAbstract n u (baseToReturn ty t))

argReturn ::
  ty ->
  IR.RawDataArg ty val ->
  IR.RawDataArg ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
argReturn ty arg@RawDataArg {rawArgType} =
  arg {rawArgType = baseToReturn ty rawArgType}

argEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawDataArg primTy primVal ->
  IR.DataArg primTy primVal
argEval globals arg@RawDataArg {rawArgName, rawArgUsage, rawArgType} =
  DataArg rawArgName rawArgUsage (unsafeEval globals rawArgType)

conReturn ::
  ty ->
  IR.RawDataCon ty val ->
  IR.RawDataCon ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
conReturn ty con@RawDataCon {rawConType, rawConDef} =
  con {rawConType = baseToReturn ty rawConType, rawConDef = funReturn ty <$> rawConDef}

conEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawDataCon primTy primVal ->
  IR.DataCon primTy primVal
conEval globals con@RawDataCon {rawConName, rawConType, rawConDef} =
  DataCon rawConName (unsafeEval globals rawConType) (funEval globals <$> rawConDef)

funReturn ::
  ty ->
  IR.RawFunction ty val ->
  IR.RawFunction ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
funReturn ty (RawFunction name usage term clauses) =
  RawFunction name usage (baseToReturn ty term) (funClauseReturn ty <$> clauses)

funEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawFunction primTy primVal ->
  IR.Function primTy primVal
funEval globals (RawFunction name usage term clauses) =
  Function name usage (unsafeEval globals term) (funClauseEval globals <$> clauses)

funClauseReturn ::
  ty ->
  IR.RawFunClause ty val ->
  IR.RawFunClause ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
funClauseReturn ty (RawFunClause tel patts term catchall) =
  RawFunClause (telescopeReturn ty tel) (map (pattEval ty) patts) (baseToReturn ty term) catchall

funClauseEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.RawFunClause primTy primVal ->
  IR.FunClause primTy primVal
funClauseEval globals (RawFunClause tel patts rhs catchall) =
  FunClause
    (telescopeEval globals tel)
    patts
    rhs
    Nothing -- TODO:-- | @Δ ⊢ t@.  The type of the rhs under @clauseTel@.
    catchall
    Nothing --TODO

telescopeReturn ::
  ty ->
  RawTelescope IR.NoExt ty val ->
  RawTelescope IR.NoExt ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
telescopeReturn ty = fmap f
  where
    f t@RawTeleEle{rawTy, rawExtension} = t { rawTy = baseToReturn ty rawTy, rawExtension = rawExtension } 

telescopeEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show primTy,
    Show primVal,
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.Arg primVal)
  ) =>
  IR.RawGlobals primTy primVal ->
  RawTelescope IR.NoExt primTy primVal ->
  Telescope IR.NoExt IR.NoExt primTy primVal
telescopeEval globals ts = f <$> ts
  where
    f rawT@RawTeleEle {..} =
      TeleEle
        { name = rawName,
          usage = rawUsage,
          ty = unsafeEval globals rawTy,
          extension = rawExtension
        }

pattEval ::
  ty ->
  IR.Pattern ty val ->
  IR.Pattern ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) 
pattEval ty patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map (pattEval ty) ps)
    IR.PPair x y -> IR.PPair (pattEval ty x) (pattEval ty y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn ty t)
    -- TODO
    IR.PPrim p -> IR.PPrim (CoreApp.Return (ty :| []) p)

baseToReturn ::
  ty ->
  Term' IR.NoExt ty val ->
  Term' IR.NoExt ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) 
baseToReturn ty t =
  case t of
    IR.Star u -> IR.Star u
    IR.PrimTy p -> IR.PrimTy p
    IR.Prim p -> IR.Prim (CoreApp.Return (ty :| []) p)
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
  Elim' IR.NoExt ty val ->
  Elim' IR.NoExt ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) -- ty' --(TypedPrim ty val)
elimToReturn ty e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn ty e) (baseToReturn ty t)
    IR.Ann u a b c -> IR.Ann u (baseToReturn ty a) (baseToReturn ty b) c

unsafeEval ::
  ( IR.CanEval IR.NoExt IR.NoExt primTy primVal,
    Show (Parameterisation.Arg primTy),
    Show (Parameterisation.ApplyErrorExtra primTy),
    Show (Parameterisation.ApplyErrorExtra primVal),
    Show (Parameterisation.Arg primVal),
    Show primTy,
    Show primVal
  ) =>
  IR.RawGlobals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal
unsafeEval globals t = case pTraceShow ("unsafeEval", t) IR.evalTerm (IR.rawLookupFun' globals) t of
  Right v -> pTraceShow ("Can eval!", v) v
  Left v -> pTraceShow ("Can't eval", v) panic "Failed to eval term"
