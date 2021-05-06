module Juvix.ToCore.FromFrontend.Transform.HR (transformTermHR) where

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Usage as Usage
import Juvix.ToCore.FromFrontend.Transform.Helpers
  ( ReduceEff,
    getParamConstant,
    getSpecialSig,
    lookupSigWithSymbol,
    parseVarArg,
    parseVarPat,
    toElim,
  )
import Juvix.ToCore.FromFrontend.Transform.Usage
import Juvix.ToCore.Types
  ( Error (..),
    Special (..),
    throwFF,
  )
import Prelude (error)

-- | Transform S-expression form into Human Readable form
-- N.B. doesn't deal with pattern variables since HR doesn't have them.
-- 'transformTermIR' does that.
transformTermHR ::
  (Show primTy, Show primVal, ReduceEff primTy primVal m) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformTermHR _ (Sexp.Atom a@Sexp.N {}) =
  HR.Prim <$> getParamConstant a
transformTermHR q (Sexp.Atom Sexp.A {atomName}) =
  toName <$> lookupSigWithSymbol (Just q) atomName
  where
    toName = HR.Elim . HR.Var . maybe atomName fst
transformTermHR q p@(name Sexp.:> form)
  -- Unimplemented cases
  -- 1. _refinement_
  --    - TODO :: the name will only become relevant (outside of arrows)
  --              when refinements are supported
  -- 2. _universe names_
  --    - TODO :: for universe polymorphism
  -- 3. _declaim_
  --    - TODO :: We ignore the declration currently
  | named ":record-no-pun" = do
    throwFF $ RecordUnimplemented p
  | named ":refinement" = throwFF $ RefinementsUnimplemented p
  | named ":let-type" = throwFF $ ExprUnimplemented p
  | named ":list" = throwFF $ ListUnimplemented p
  | named "case" = do
    traceM "Is it CASE?"
    pTraceShowM (p)
    --   throwFF $ ExprUnimplemented p
    transformApplication q p
  | named ":u" = throwFF $ UniversesUnimplemented p
  -- Rest
  | named ":custom-arrow" = transformArrow form
  | named ":let-match" = transformSimpleLet form
  | named ":primitive" = transformPrim form
  | named ":declaim" = transformTermHR q (Sexp.cadr form) -- skip declar
  | named ":lambda" = transformSimpleLambda form
  | named ":progn" = transformTermHR q (Sexp.car form)
  | named ":paren" = transformTermHR q (Sexp.car form)
  | named ":tuple",
    Just xs <- Sexp.toList form =
    makeTuple <$> traverse (transformTermHR q) xs
  | otherwise = do
    transformApplication q p
  where
    named = Sexp.isAtomNamed name

    makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
    makeTuple [] = HR.Unit
    makeTuple [t] = t
    makeTuple (t : ts) = HR.Pair t (makeTuple ts)
    transformArrow _f@(Sexp.List [π, xa, b]) = go π "" xa b
      where
        -- we never generate this form, so we should be able to erase this!?
        -- FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b

        go π x a b =
          HR.Pi <$> transformUsage q π
            <*> pure x
            <*> transformTermHR q a
            <*> transformTermHR q b
    transformArrow _ = error "malformed custom arrow"

    transformSimpleLet e@(Sexp.List [name, Sexp.List [arg, cbody], body])
      | Just Sexp.A {atomName} <- Sexp.atomFromT name,
        Just xs <- Sexp.toList arg = do
        args <- traverse parseVarArg xs
        cbody <- transformTermHR q cbody
        rhs <- toElim (Sexp.Cons (Sexp.atom ":let-match=") e) $ foldr HR.Lam cbody args
        HR.Let Usage.Omega atomName rhs <$> transformTermHR q body
    transformSimpleLet (Sexp.List [_name, fun, _body]) =
      throwFF $ ExprUnimplemented fun
    transformSimpleLet _ = error "malformed let"

    transformSimpleLambda (Sexp.List [args, body])
      | Just pats <- Sexp.toList args >>= NonEmpty.nonEmpty =
        foldr HR.Lam <$> transformTermHR q body <*> traverse parseVarPat pats
    -- TODO: Avoid throwing error
    transformSimpleLambda _ = error "malformed lambda"
    transformPrim (Sexp.List [parm])
      | Just Sexp.A {atomName = p} <- Sexp.atomFromT parm = do
        param <- ask @"param"
        maybe (throwFF $ UnknownPrimitive p) pure $
          primTy param p <|> primVal param p
      where
        primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
        primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
    -- TODO: Avoid throwing error
    transformPrim _ = error "malformed prim"
transformTermHR _ Sexp.Nil = error "malformed term HR"

pattern NamedArgTerm ::
  Symbol -> HR.Term primTy primVal -> HR.Term primTy primVal
pattern NamedArgTerm x ty <-
  HR.Elim (HR.Ann _ (HR.Elim (HR.Var (x :| []))) ty _)

-- TODO
transformApplication ::
  ( Show primVal,
    Show primTy,
    ReduceEff primTy primVal m
  ) =>
  NameSymbol.Mod ->
  Sexp.T ->
  m (HR.Term primTy primVal)
transformApplication q (f Sexp.:> args)
  | Just xs <- Sexp.toList args = do
    getSpecialSig q f >>= flip go xs
  where
    go Nothing xs = do
      f' <- toElim f =<< transformTermHR q f
      HR.Elim . foldl HR.App f' <$> traverse (transformTermHR q) xs
    go (Just s) xs = case s of
      ArrowS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (ArrowS (Just π))) [a, b]
      ArrowS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Pi π x a <$> transformTermHR q b
      PairS Nothing -> do
        ~[π, a, b] <- nargs s 3 xs
        π <- transformUsage q π
        go (Just (PairS (Just π))) [a, b]
      PairS (Just π) -> do
        ~[xa, b] <- nargs s 2 xs
        (x, a) <- namedArg q xa
        HR.Sig π x a <$> transformTermHR q b
      ColonS -> do
        ~[a, b] <- nargs s 2 xs
        a <- transformTermHR q a
        b <- transformTermHR q b
        -- FIXME metavars for usage & universe
        pure $ HR.Elim $ HR.Ann (Usage.SNat 1) a b 0
      TypeS -> do
        ~[i] <- nargs s 1 xs
        HR.Star <$> transformUniverse i
      OmegaS ->
        throwFF UnexpectedOmega
    nargs s n xs
      | length xs == n = pure xs
      | otherwise = throwFF $ WrongNumberBuiltinArgs s n args

    namedArg q e =
      transformTermHR q e >>= \case
        NamedArgTerm x ty -> pure (NameSymbol.fromSymbol x, ty)
        ty -> pure ("" :| [], ty)
    transformUniverse (Sexp.Atom Sexp.N {atomNum = i}) | i >= 0 = pure $ fromIntegral i
    transformUniverse e = throwFF $ NotAUniverse e
transformApplication _ _ = error "malformed application"
