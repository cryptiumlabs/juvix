-- | Datatype declarations are typechecked here. Usages are passed along.
module Juvix.Core.IR.CheckDatatype
  ( typeCheckAllCons,
    checkDataType,
  )
where

import Juvix.Core.Base.Types as Core
import Juvix.Core.IR.CheckTerm hiding (T)
import qualified Juvix.Core.IR.Evaluator as Eval
import Juvix.Core.IR.Types (T, pattern VStar)
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

-- | check all constructors of a datatype
typeCheckAllCons ::
  ( HasThrowTC' T extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT T primTy primVal,
    Eval.EvalPatSubst T primTy primVal,
    Eval.EvalPatSubst T primTy (TypedPrim primTy primVal)
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- |
  Telescope T extT primTy primVal ->
  -- | Positivity of its parameters
  [Core.Pos] ->
  RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  GlobalsT' T extT primTy primVal ->
  -- | The constructors to be checked
  [RawDataCon' extT primTy primVal] ->
  TypeCheck T primTy primVal m [Core.RawGlobal' extT primTy primVal]
typeCheckAllCons param tel pos rtel globals =
  mapM (typeCheckConstructor param tel pos rtel globals)

typeCheckConstructor ::
  forall extT primTy primVal m.
  ( HasThrowTC' T extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT T primTy primVal,
    Eval.EvalPatSubst T primTy primVal,
    Eval.EvalPatSubst T primTy (TypedPrim primTy primVal)
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  Telescope T extT primTy primVal ->
  -- | Positivity of its parameters
  [Core.Pos] ->
  RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  GlobalsT' T extT primTy primVal ->
  -- | The constructor to be checked
  RawDataCon' extT primTy primVal ->
  TypeCheck T primTy primVal m (Core.RawGlobal' extT primTy primVal)
typeCheckConstructor param tel _lpos rtel globals con = do
  let cname = Core.rawConName con
      conTy = Core.rawConType con
      (name, t) = teleToType rtel conTy
  -- FIXME replace 'lift' with whatever capability does
  typechecked <- lift $ typeTerm param t (Annotation mempty (VStar 0))
  evaled <- lift $ liftEval $ Eval.evalTerm (Eval.lookupFun @T globals) typechecked
  checkConType tel param evaled
  let (_, target) = typeToTele (name, t)
  -- FIXME replace 'lift'
  lift $ checkDeclared cname rtel target
  -- put (addSig sig n (ConSig vt))
  return $ Core.RawGDataCon con

teleToType ::
  RawTelescope extT primTy primVal ->
  Core.Term' extT primTy primVal ->
  (Maybe GlobalName, Core.Term' extT primTy primVal)
teleToType [] t = (Nothing, t)
teleToType (hd : tel) t2 =
  ( Just (rawName hd),
    Pi
      (rawUsage hd)
      (rawTy hd)
      (snd (teleToType tel t2))
      (rawExtension hd)
  )

typeToTele ::
  (Maybe GlobalName, Core.Term' ext primTy primVal) ->
  (RawTelescope ext primTy primVal, Core.Term' ext primTy primVal)
typeToTele (n, t) = ttt (n, t) []
  where
    ttt ::
      (Maybe GlobalName, Core.Term' ext primTy primVal) ->
      RawTelescope ext primTy primVal ->
      (RawTelescope ext primTy primVal, Core.Term' ext primTy primVal)
    ttt (Just n, Pi usage t' t2 ext) tel =
      ttt
        (Nothing, t2)
        ( tel
            <> [ RawTeleEle
                   { rawName = n,
                     rawUsage = usage,
                     rawTy = t',
                     rawExtension = ext
                   }
               ]
        )
    ttt x tel = (tel, snd x)

-- | checkDataType checks the datatype by checking all arguments.
-- The data constructors are checked by another function.
checkDataType ::
  ( Eval.CanEval T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Core.ValueAll Eq T primTy primVal,
    Core.NeutralAll Eq T primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope T extT primTy primVal ->
  -- | name of the datatype
  GlobalName ->
  Param.Parameterisation primTy primVal ->
  -- | the list of args to be checked.
  [Core.RawDataArg' extT primTy primVal] ->
  m ()
checkDataType tel dtName param =
  mapM_ (checkDataTypeArg tel dtName param . Core.rawArgType)

-- | checkDataTypeArg checks an argument of the datatype
checkDataTypeArg ::
  ( Eval.CanEval T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Show primTy,
    Show primVal,
    Show extT,
    ShowExt extT primTy primVal,
    Core.ValueAll Eq T primTy primVal,
    Core.NeutralAll Eq T primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope T extT primTy primVal ->
  -- | name of the datatype
  GlobalName ->
  -- | targeted backend/parameterisation
  Param.Parameterisation primTy primVal ->
  -- | the arg to be checked.
  Core.Term' extT primTy primVal ->
  m ()
checkDataTypeArg tel dtName param (Pi _ t1 t2 _) = do
  _ <- typeTerm param t1 (Annotation mempty (VStar 0))
  checkDataTypeArg tel dtName param t2
checkDataTypeArg _ _ _ (Star' _ _) = return ()
-- the arg can only be of star type of function type
checkDataTypeArg _ _ _ arg = throwTC $ DatatypeError arg

-- | type check a constructor
checkConType ::
  ( Eval.CanEval T extT primTy primVal,
    Eq primTy,
    Eq primVal,
    Core.ValueAll Eq T primTy primVal,
    Core.NeutralAll Eq T primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope T extT primTy primVal ->
  Param.Parameterisation primTy primVal ->
  -- | the expression that is left to be checked.
  Core.Value' T primTy (TypedPrim primTy primVal) ->
  TypeCheck T primTy primVal m ()
checkConType tel param e =
  case e of
    -- the constructor could be a function type
    VPi' _ _ t2 _ ->
      -- no need to check function argument because
      -- it's been type checked in typeCheckConstructor (?)
      checkConType tel param t2
    -- or a star type
    Core.VStar' _ _ -> return ()
    -- a constructor cannot be any other type
    _ -> lift $ throwTC $ ConTypeError e

-- | check that the data type and the parameter arguments
-- are written down like declared in telescope
checkDeclared ::
  (HasThrow "typecheckError" (TypecheckError' T extT primTy primVal) m) =>
  GlobalName ->
  RawTelescope extT primTy primVal ->
  Core.Term' extT primTy primVal ->
  m ()
checkDeclared name tel tg@(Core.Elim' (Core.App' (Free (Global n) _) term _) _) =
  if n == name
    then do
      checkParams tel term -- check parameters
    else throwTC $ DeclError tg name tel
checkDeclared name tel tg@(Core.Elim' (Core.Free' (Global n) _) _) =
  if n == name && null tel
    then return ()
    else throwTC $ DeclError tg name tel
checkDeclared name tel tg =
  throwTC $ DeclError tg name tel

-- check parameters
checkParams ::
  (HasThrow "typecheckError" (TypecheckError' T extT primTy primVal) m) =>
  RawTelescope extT primTy primVal ->
  Core.Term' extT primTy primVal ->
  m ()
checkParams tel@(hd : tl) para@(Elim elim _) =
  let n = rawName hd
   in case elim of
        Free (Global n') _ | n == n' -> return ()
        App (Free (Global n') _) term _ | n == n' -> checkParams tl term
        _ -> throwTC $ ParamError n para
checkParams [] _ = return ()
checkParams (hd : _) exp =
  throwTC $ ParamError (rawName hd) exp
