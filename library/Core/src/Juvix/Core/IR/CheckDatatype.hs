-- | Datatype declarations are typechecked here. Usages are passed along.
module Juvix.Core.IR.CheckDatatype
  ( module Juvix.Core.IR.CheckDatatype,
  )
where

import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
-- import SPos ( sposConstructor )

import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import Juvix.Core.IR.Types (NoExt, pattern VStar)
import Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library

-- | check all constructors of a datatype
typeCheckAllCons ::
  ( HasThrowTC' NoExt extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply primVal,
    Param.CanApply (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT NoExt primTy primVal,
    Eval.EvalPatSubst NoExt primTy primVal,
    Eval.EvalPatSubst NoExt primTy (TypedPrim primTy primVal)
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- | 
  Telescope NoExt extT primTy primVal ->
  -- | Positivity of its parameters
  [IR.Pos] ->
  RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  GlobalsT' NoExt extT primTy primVal ->
  -- | The constructors to be checked
  [RawDataCon' extT primTy primVal] ->
  TypeCheck NoExt primTy primVal m [IR.RawGlobal' extT primTy primVal]
typeCheckAllCons param tel pos rtel globals =
  mapM (typeCheckConstructor param tel pos rtel globals)

typeCheckConstructor :: forall extT primTy primVal m.
  ( HasThrowTC' NoExt extT primTy primVal m,
    Eq primTy,
    Eq primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply primVal,
    Param.CanApply (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy (TypedPrim primTy primVal),
    Eval.NoExtensions extT primTy primVal,
    Eval.CanEval extT NoExt primTy primVal,
    Eval.EvalPatSubst NoExt primTy primVal,
    Eval.EvalPatSubst NoExt primTy (TypedPrim primTy primVal)
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  Telescope NoExt extT primTy primVal ->
  -- | Positivity of its parameters
  [IR.Pos] ->
  RawTelescope extT primTy primVal ->
  -- | a hashmap of global names and their info
  GlobalsT' NoExt extT primTy primVal ->
  -- | The constructor to be checked
  RawDataCon' extT primTy primVal ->
  TypeCheck NoExt primTy primVal m (IR.RawGlobal' extT primTy primVal)
typeCheckConstructor param tel lpos rtel globals con = do
  let cname = IR.rawConName con
      conTy = IR.rawConType con
      (name, t) = teleToType rtel conTy
  -- FIXME replace 'lift' with whatever capability does
  typechecked <- lift $ typeTerm param t (Annotation mempty (VStar 0))
  evaled <- lift $ liftEval $ Eval.evalTerm (Eval.lookupFun @NoExt globals) typechecked
  checkConType tel param evaled
  let (_, target) = typeToTele (name, t)
  -- FIXME replace 'lift'
  lift $ checkDeclared cname rtel target
  -- put (addSig sig n (ConSig vt))
  return $ IR.RawGDataCon con

teleToType ::
  RawTelescope extT primTy primVal ->
  IR.Term' extT primTy primVal ->
  (Maybe Name, IR.Term' extT primTy primVal)
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
  (Maybe Name, IR.Term' ext primTy primVal) ->
  (RawTelescope ext primTy primVal, IR.Term' ext primTy primVal)
typeToTele (n, t) = ttt (n, t) []
  where
    ttt ::
      (Maybe Name, IR.Term' ext primTy primVal) ->
      RawTelescope ext primTy primVal ->
      (RawTelescope ext primTy primVal, IR.Term' ext primTy primVal)
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
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval NoExt extT primTy primVal,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primVal,
    Eq primTy,
    Eq primVal,
    IR.ValueAll Eq NoExt primTy primVal,
    IR.NeutralAll Eq NoExt primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    HasThrow "typecheckError" (TypecheckError' NoExt extT primTy primVal) m
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope NoExt extT primTy primVal ->
  -- | name of the datatype
  GlobalName ->
  Param.Parameterisation primTy primVal ->
  -- | the list of args to be checked.
  [IR.RawDataArg' extT primTy primVal] ->
  m ()
checkDataType tel dtName param =
  mapM_ (checkDataTypeArg tel dtName param . IR.rawArgType)

-- | checkDataTypeArg checks an argument of the datatype
checkDataTypeArg ::
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval NoExt extT primTy primVal,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primVal,
    Eq primTy,
    Eq primVal,
    IR.ValueAll Eq NoExt primTy primVal,
    IR.NeutralAll Eq NoExt primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    HasThrow "typecheckError" (TypecheckError' NoExt extT primTy primVal) m
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope NoExt extT primTy primVal ->
  -- | name of the datatype
  GlobalName ->
  -- | targeted backend/parameterisation
  Param.Parameterisation primTy primVal ->
  -- | the arg to be checked.
  IR.Term' extT primTy primVal ->
  m ()
checkDataTypeArg tel dtName param (Pi _ t1 t2 _) = do
  _ <- typeTerm param t1 (Annotation mempty (VStar 0))
  checkDataTypeArg tel dtName param t2
checkDataTypeArg _ _ _ (Star' _ _) = return ()
-- the arg can only be of star type of function type
checkDataTypeArg _ _ _ arg = throwTC $ DatatypeError arg

-- | type check a constructor
checkConType ::
  ( Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval NoExt extT primTy primVal,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T NoExt) primTy primVal primVal,
    Eq primTy,
    Eq primVal,
    IR.ValueAll Eq NoExt primTy primVal,
    IR.NeutralAll Eq NoExt primTy primVal,
    CanTC' extT primTy primVal m,
    Param.CanApply (TypedPrim primTy primVal),
    HasThrow "typecheckError" (TypecheckError' NoExt extT primTy primVal) m
  ) =>
  -- | an env that contains the parameters of the datatype
  Telescope NoExt extT primTy primVal ->
  Param.Parameterisation primTy primVal ->
  -- | the expression that is left to be checked.
  -- TODO may need to be Value' NoExt primTy (TypedPrim primTy primVal)?
  IR.Value' NoExt primTy (TypedPrim primTy primVal) ->
  TypeCheck NoExt primTy primVal m ()
checkConType tel param e =
  case e of
    -- the constructor could be a function type
    VPi' _ _ t2 _ ->
      checkConType
        tel
        param
        t2
    -- or application?
    IR.VNeutral' app _ ->
      pure () -- will be handled by checkDeclared
    -- or a star type
    IR.VStar' _ _ -> return ()
    -- a constructor cannot be any other type
    _ -> lift $ throwTC $ ConTypeError e

-- | check that the data type and the parameter arguments
-- are written down like declared in telescope
checkDeclared ::
  (HasThrow "typecheckError" (TypecheckError' NoExt extT primTy primVal) m) =>
  GlobalName ->
  RawTelescope extT primTy primVal ->
  IR.Term' extT primTy primVal ->
  m ()
checkDeclared name tel tg@(IR.Elim' (IR.App' (Free (Global n) _) term _) _) =
  if n == name
    then do
      checkParams tel term -- check parameters
    else throwTC $ DeclError tg name tel
checkDeclared name tel tg@(IR.Elim' (IR.Free' (Global n) _) _) =
  if n == name && null tel
    then return ()
    else throwTC $ DeclError tg name tel
checkDeclared name tel tg =
  throwTC $ DeclError tg name tel

-- check parameters
checkParams ::
  (HasThrow "typecheckError" (TypecheckError' NoExt extT primTy primVal) m) =>
  RawTelescope extT primTy primVal ->
  IR.Term' extT primTy primVal ->
  m ()
checkParams tel@(hd : tl) para@(Elim elim _) =
  let n = rawName hd
   in case elim of
        Free n' _ ->
          if n == n'
            then return ()
            else throwTC $ ParamVarNError tel n n'
        App (Free n' _) term _ ->
          if n == n'
            then checkParams tl term
            else throwTC $ ParamVarNError tel n n'
        _ -> throwTC $ ParamError para
checkParams [] _ = return ()
checkParams _ exps =
  throwTC $ ParamError exps
