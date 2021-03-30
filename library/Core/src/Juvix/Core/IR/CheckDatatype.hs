-- | Datatype declarations are typechecked here. Usages are passed along.
module Juvix.Core.IR.CheckDatatype
  ( module Juvix.Core.IR.CheckDatatype,
  )
where

import Juvix.Core.IR.CheckTerm
import qualified Juvix.Core.IR.Evaluator as Eval
-- import SPos ( sposConstructor )
import Juvix.Core.IR.Types.Base as IR
import Juvix.Core.IR.Types.Globals as IR
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import Prelude (error)

typeCheckConstructor ::
  HasState "typeSigs" s IO =>
  Name ->
  [IR.Pos] ->
  RawTelescope ext primTy primVal ->
  (IR.Name, IR.Term' ext primTy primVal) ->
  TypeCheck ext primTy primVal IO ()
typeCheckConstructor name pos tel (n, ty) = do
  sig <- get @"typeSigs" -- get signatures
  let (n, t) = teleToType tel ty
      params = length tel
  -- _ <- checkConType 0 [] [] params t
  let (_, target) = typeToTele (n, t)
  -- checkTarget name tel target
  -- vt <- eval [] tt
  -- sposConstructor name 0 pos vt -- strict positivity check
  -- put (addSig sig n (ConSig vt))
  return ()

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

-- | checkDataType takes 5 arguments.
checkDataType ::
  -- | the next fresh generic value.
  Int ->
  -- an env that binds fresh generic values to variables.
  Telescope extV extT primTy primVal ->
  -- an env that binds the type value corresponding to these generic values.
  Telescope extV extT primTy primVal ->
  -- the length of the telescope, or the no. of parameters.
  Int ->
  -- the expression that is left to be checked.
  IR.Term' ext primTy primVal ->
  TypeCheck ext primTy primVal IO (IR.Datatype' extV extT primTy primVal)
checkDataType k rho gamma p (Pi x t1 t2 _) = undefined
-- _ <-
--   if k < p -- if k < p then we're checking the parameters
--     then checkType k rho gamma t1 -- checks params are valid types
--     else checkSType k rho gamma t1 -- checks arguments Θ are Star types
--   v_t1 <- eval rho t1
--   checkDataType (k + 1) (updateEnv rho x (VGen k)) (updateEnv gamma x v_t1) p t2
-- check that the data type is of type Star
checkDataType _k _rho _gamma _p (Star _ _) = undefined
checkDataType _k _rho _gamma _p e =
  error $ "checkDataType: " -- <> show e <> "doesn't target Star."
    --TODO show instance? more proper error throwing?

-- | checkConType check constructor type
checkConType ::
  Int -> -- the next fresh generic value.
  -- an env that binds fresh generic values to variables.
  Telescope extV extT primTy primVal ->
  -- an env that binds the type value corresponding to these generic values.
  Telescope extV extT primTy primVal ->
  -- the length of the telescope, or the no. of parameters.
  Int ->
  -- the expression that is left to be checked.
  IR.Term' ext primTy primVal ->
  TypeCheck ext primTy primVal IO ()
checkConType k rho gamma p e = undefined
--   case e of
--     Pi x t1 t2 -> do
--       if k < p
--         then return () -- params were already checked by checkDataType
--         else checkSType k rho gamma t1 -- check that arguments ∆ are stypes
--       v_t1 <- eval rho t1
--       checkConType
--         (k + 1)
--         (updateEnv rho x (VGen k))
--         (updateEnv gamma x v_t1)
--         p
--         t2
--     -- the constructor's type is of type Star(the same type as the data type).
--     _ -> checkExpr k rho gamma e VStar

-- -- check that the data type and the parameter arguments
-- -- are written down like declared in telescope
-- checkTarget :: Name -> RawTelescope ext primTy primVal -> IR.Term' ext primTy primVal -> TypeCheck ext primTy primVal IO ()
-- checkTarget name tel tg@(App (Def n) al) =
--   if n == name
--     then do
--       let pn = length tel
--           params = take pn al
--       checkParams tel params -- check parameters
--     else error $
--          "checkTarget: target mismatch " <> show tg <> ". Input name is " <>
--          show name <>
--          ". Input telescope is " <>
--          show tel
-- checkTarget name tel tg@(Def n) =
--   if n == name && null tel
--     then return ()
--     else error $
--          "checkTarget: target mismatch" <> show tg <> ". Input name is " <>
--          show name <>
--          ". Input telescope is " <>
--          show tel
-- checkTarget name tel tg =
--   error $
--   "checkTarget: target mismatch" <> show tg <> ". Input name is " <> show name <>
--   ". Input telescope is " <>
--   show tel

-- -- check parameters
-- checkParams :: RawTelescope ext primTy primVal -> [IR.Term' ext primTy primVal] -> TypeCheck ext primTy primVal IO ()
-- checkParams [] [] = return ()
-- checkParams tel@((n, _t):tl) (Var n':el) =
--   if n == n'
--     then checkParams tl el
--     else error $
--          "checkParams: target parameter mismatch. The input telescope is " <>
--          show tel <>
--          ". One of the name in the telescope is " <>
--          show n <> -- using show to wrap n with "
--          ", which does not match the input expression's variable name: " <>
--          show n'
-- checkParams _ exps =
--   error $
--     "checkParams: target parameter mismatch. The input expression"
--     <> show exps
--     <> "isn't a variable (Var)."
