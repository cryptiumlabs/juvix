-- | Datatype declarations are typechecked here. Usages are passed along.
module Juvix.Core.IR.CheckDatatype
  ( module Juvix.Core.IR.CheckDatatype,
  )
where

import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
-- import SPos ( sposConstructor )

import Juvix.Core.IR.Typechecker.Types as Typed
import Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Globals as IR
import qualified Juvix.Core.Parameterisation as Param
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import Juvix.Core.IR.Types (NoExt)
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts

typeCheckConstructor ::
  ( HasThrow "typecheckError"
      (TypecheckError' extV0 ext primTy primVal)
      (TypeCheck ext primTy primVal m),
    HasState "typeSigs" s m,
    Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m,
    Param.CanApply primTy,
    Param.CanApply (TypedPrim primTy primVal)
  ) =>
  -- | The targeted parameterisation
  Param.Parameterisation primTy primVal ->
  -- | The constructor name
  GlobalName ->
  -- | Positivity of its parameters
  [IR.Pos] ->
  RawTelescope ext primTy primVal ->
  -- | The term to be checked
  (IR.Name, IR.Term' ext primTy primVal) ->
  TypeCheck ext primTy primVal m [Global' extV extT primTy primVal]
typeCheckConstructor param name pos tel (n, ty) = do
  sig <- get @"typeSigs" -- get signatures
  let (n, t) = teleToType tel ty
      numberOfParams = length tel
  -- _ <- checkConType 0 [] [] numberOfParams t
  let (_, target) = typeToTele (n, t)
  checkDeclared name tel target
  -- vt <- eval [] tt
  -- put (addSig sig n (ConSig vt))
  return undefined --TODO return the list of globals

teleToType ::
  RawTelescope ext primTy primVal ->
  IR.Term' ext primTy primVal ->
  (Maybe Name, IR.Term' ext primTy primVal)
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

-- | an env that binds fresh generic values (in Int) to variables.
type GenEnv = [(Name, Int)]

-- | update generic value env
updateGenEnv ::
  GenEnv ->
  Name ->
  Int ->
  GenEnv
updateGenEnv genEnv name k = (name, k) : genEnv

updateTel ::
  Telescope extV extT primTy primVal ->
  Name ->
  Value' ext primTy primVal ->
  Telescope extV extT primTy primVal
updateTel tel n v = undefined --(n, v) : tel

-- | checkDataType takes 5 arguments.
checkDataType ::
  (HasThrow "typecheckError" (TypecheckError' extV ext primTy primVal) m) =>
  -- | the next fresh generic value.
  Int ->
  -- | an env that binds fresh generic values to variables.
  Telescope extV extT primTy primVal ->
  -- | an env that binds the type value corresponding to these generic values.
  Telescope extV extT primTy primVal ->
  -- | the length of the telescope, or the no. of parameters.
  Int ->
  -- | the expression that is left to be checked.
  IR.Term' ext primTy primVal ->
  m ()
checkDataType k rho gamma p (Pi x t1 t2 _) = undefined
-- _ <-
--   if k < p -- if k < p then we're checking the parameters
--     then checkType k rho gamma t1 -- checks params are valid types
--     else checkSType k rho gamma t1 -- checks arguments Θ are Star types
--   v_t1 <- eval rho t1
--   checkDataType (k + 1) (updateTel rho x (VGen k)) (updateTel gamma x v_t1) p t2
-- check that the data type is of type Star
checkDataType _k _rho _gamma _p (Star _ _) = return ()
checkDataType _k _rho _gamma _p e =
  throwTC $ DatatypeError e

-- | checkConType check constructor type
checkConType ::
  ( HasThrow "typecheckError" (TypecheckError' extV ext primTy primVal) m,
    Param.CanApply primTy,
    Param.CanApply primVal,
    Eval.CanEval extT extG primTy primVal,
    Eq primTy,
    Eq primVal,
    CanTC' ext primTy primVal m
   ) =>
  -- | the next fresh generic value.
  Int ->
  -- | an env that binds fresh generic values to variables.
  GenEnv ->
  -- | an env that binds the type value corresponding to these generic values.
  Telescope extV extT primTy primVal ->
  -- | the length of the telescope, or the no. of parameters.
  Int ->
  -- | a hashmap of global names and raw globals 
  RawGlobals' NoExt primTy primVal ->
  -- | the expression that is left to be checked.
  IR.Term' ext primTy primVal ->
  m ()
checkConType k rho gamma p globals e =
  case e of
    Pi x t1 t2 _ -> do
      --TODO type check e (so that usage is checked)?
      if k < p
        then-- params were already checked by checkDataType
          return ()
        else-- check that arguments ∆ are star types
        case Eval.evalTerm (Eval.rawLookupFun' globals) t1 of
          Right (VStar' _ _) -> return ()
          -- arguments can be a primitive type
          Right (VPrimTy' _ _) -> return ()
          -- if it's not star or prim type, then this constructor is not valid
          Right _ -> throwTC $ ConFnTypeError e t1
          -- if it can't evaluate, it's not valid
          Left err -> return ()-- TODO throwTC $ EvalError err
      -- v1 <- Eval.evalTerm (Eval.lookupFun globals ) t2
      checkConType
        (k + 1)
        rho --(updateGenEnv rho name k)
        gamma --(updateTel gamma x v1)
        p
        globals
        t2
    -- the constructor's type is of type Star(the same type as the data type).
    _ -> case e of
      Star' _ _ -> return ()
      _ -> throwTC $ ConTypeError e

-- check that the data type and the parameter arguments
-- are written down like declared in telescope
checkDeclared ::
  (HasThrow "typecheckError" (TypecheckError' extV ext primTy primVal) m) =>
  GlobalName ->
  RawTelescope ext primTy primVal ->
  IR.Term' ext primTy primVal ->
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
  (HasThrow "typecheckError" (TypecheckError' extV ext primTy primVal) m) =>
  RawTelescope ext primTy primVal ->
  IR.Term' ext primTy primVal ->
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
