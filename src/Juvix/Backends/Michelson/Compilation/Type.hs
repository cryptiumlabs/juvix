-- |
-- - Functions for representation of types in the Michelson backend.
module Juvix.Backends.Michelson.Compilation.Type where

import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Core.ErasedAnn.Types as J
import Juvix.Library hiding (Type)
import qualified Michelson.Untyped as M

{-
 - Convert parameterised core types to their equivalent in Michelson.
 -}
typeToType ::
  forall m.
  (HasThrow "compilationError" Types.CompilationError m) =>
  Types.Type ->
  m M.Type
typeToType ty =
  case ty of
    J.SymT _ -> throw @"compilationError" Types.InvalidInputType
    J.Star _ -> throw @"compilationError" Types.InvalidInputType
    J.PrimTy (Types.PrimTy mTy) -> pure mTy
    -- TODO ∷ Integrate usage information into this
    J.Pi _usages argTy retTy -> do
      argTy <- typeToType argTy
      retTy <- typeToType retTy
      pure (M.Type (M.TLambda argTy retTy) "")

-- Drop n arguments from a lambda type.
dropNArgs ::
  forall m.
  (HasThrow "compilationError" Types.CompilationError m) =>
  Types.Type ->
  Int ->
  m Types.Type
dropNArgs ty 0 = pure ty
dropNArgs ty n =
  case ty of
    J.Pi _ _ retTy -> dropNArgs retTy (n - 1)
    _ -> throw @"compilationError" Types.InvalidInputType

{-
 - Closure packing:
 - No free variables - ()
 - Free variables: nested pair of free variables in order, finally ().
 -}
closureType :: [(Symbol, M.Type)] -> M.Type
closureType [] = M.Type M.TUnit ""
closureType ((_, x) : xs) = M.Type (M.TPair "" "" x (closureType xs)) ""

{-
 - Lambda types: (closure type, argument type) -> (return type)
 -}

lamType :: [(Symbol, M.Type)] -> [(Symbol, M.Type)] -> M.Type -> M.Type
lamType argsPlusClosures extraArgs retTy =
  M.Type
    ( M.TLambda
        ( M.Type
            ( M.TPair
                ""
                ""
                (closureType argsPlusClosures)
                (closureType extraArgs)
            )
            ""
        )
        retTy
    )
    ""

{- TODO: Figure out how to add nice annotations without breaking equality comparisons. -}

typesFromPi ::
  HasThrow "compilationError" Types.CompilationError f =>
  J.Type Types.PrimTy Types.PrimVal ->
  f [M.Type]
typesFromPi (J.Pi _usage aType rest) = (:) <$> typeToType aType <*> typesFromPi rest
typesFromPi _ = pure []

returnTypeFromPi ::
  HasThrow "compilationError" Types.CompilationError m =>
  J.Type Types.PrimTy Types.PrimVal ->
  m M.Type
returnTypeFromPi (J.Pi _usage _ rest) = returnTypeFromPi rest
returnTypeFromPi x = typeToType x
