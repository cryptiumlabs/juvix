{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Parameterisation where

import qualified Juvix.Core.Application as App
import Juvix.Core.IR.Types (BoundVar, GlobalName, NoExt)
import qualified Juvix.Core.HR.Pretty as HR
import Juvix.Library
import Juvix.Library.HashMap (HashMap)
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.NameSymbol as NameSymbol
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Prelude (String)

-- | @[A, B, ..., Z]@ represents the type
-- @π A -> ρ B -> ... -> Z@ for any usages @π@, @ρ@
newtype PrimType primTy = PrimType {getPrimType :: NonEmpty primTy}
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type Builtins p = HashMap NameSymbol.T p

data Parameterisation primTy primVal = Parameterisation
  { hasType :: primVal -> PrimType primTy -> Bool,
    builtinTypes :: Builtins primTy,
    builtinValues :: Builtins primVal,
    parseTy :: Token.GenTokenParser String () Identity -> Parser primTy,
    parseVal :: Token.GenTokenParser String () Identity -> Parser primVal,
    reservedNames :: [String],
    reservedOpNames :: [String],
    stringTy :: Text -> primTy -> Bool,
    stringVal :: Text -> Maybe primVal,
    intTy :: Integer -> primTy -> Bool,
    intVal :: Integer -> Maybe primVal,
    floatTy :: Double -> primTy -> Bool,
    floatVal :: Double -> Maybe primVal
  }
  deriving (Generic)

{-# DEPRECATED
  parseTy,
  parseVal,
  reservedNames,
  reservedOpNames
  "TODO: update parser to not use these"
  #-}

data ApplyError' e a
  = ExtraArguments a (NonEmpty (Arg a))
  | InvalidArguments a (NonEmpty (Arg a))
  | Extra e

deriving instance (Eq e, Eq a, Eq (Arg a)) => Eq (ApplyError' e a)

deriving instance (Show e, Show a, Show (Arg a)) => Show (ApplyError' e a)

type ApplyError a = ApplyError' (ApplyErrorExtra a) a

type instance PP.Ann (ApplyError' _ _) = HR.PPAnn

instance
  ( PP.PrettyText e,
    HR.ToPPAnn (PP.Ann e),
    PP.PrettySyntax (Arg a),
    HR.ToPPAnn (PP.Ann (Arg a)),
    PP.PrettySyntax a,
    HR.ToPPAnn (PP.Ann a)
  ) =>
  PP.PrettyText (ApplyError' e a)
  where
  prettyT = \case
    ExtraArguments f xs ->
      PP.sepIndent' [
        (False, "Function"),
        (True, pretty0 f),
        (False, "applied to extra arguments"),
        (True, PP.sep $ PP.punctuate "," $ fmap pretty0 xs)
      ]
    InvalidArguments f xs ->
      PP.sepIndent' [
        (False, "Function"),
        (True, pretty0 f),
        (False, "applied to invalid arguments"),
        (True, PP.sep $ PP.punctuate "," $ fmap pretty0 xs)
      ]
    Extra e -> HR.toPPAnn <$> PP.prettyT e
    where
      pretty0 = fmap HR.toPPAnn . PP.pretty0


class CanApply a where
  type ApplyErrorExtra a
  type ApplyErrorExtra a = Void

  type Arg a
  type Arg a = a

  pureArg :: a -> Maybe (Arg a)
  default pureArg :: (Arg a ~ a) => a -> Maybe (Arg a)
  pureArg = Just

  freeArg :: Proxy a -> GlobalName -> Maybe (Arg a)
  freeArg _ _ = Nothing

  boundArg :: Proxy a -> BoundVar -> Maybe (Arg a)
  boundArg _ _ = Nothing

  arity :: a -> Natural
  apply :: a -> NonEmpty (Arg a) -> Either (ApplyError a) a

mapApplyErr ::
  ( ApplyErrorExtra a ~ ApplyErrorExtra b,
    Arg a ~ a,
    Arg b ~ b
  ) =>
  (a -> b) ->
  Either (ApplyError a) a ->
  Either (ApplyError b) b
mapApplyErr wrap = bimap wrap' wrap
  where
    wrap' (ExtraArguments f xs) =
      ExtraArguments (wrap f) (map wrap xs)
    wrap' (InvalidArguments f xs) =
      InvalidArguments (wrap f) (map wrap xs)
    wrap' (Extra e) = Extra e

applyMaybe :: CanApply a => a -> NonEmpty (Arg a) -> Maybe a
applyMaybe f xs = either (const Nothing) Just $ apply f xs

apply1 :: CanApply a => a -> Arg a -> Either (ApplyError a) a
apply1 f x = apply f (x :| [])

apply1Maybe :: CanApply a => a -> Arg a -> Maybe a
apply1Maybe f x = applyMaybe f (x :| [])

type TypedPrim' ext ty val = App.Return' ext (PrimType ty) val

type TypedPrim ty val = TypedPrim' NoExt ty val


data PPAnn' primTy
  = PAArrow
  | PAPunct
  | PATy (PP.Ann primTy)

type PPAnn primTy = Last (PPAnn' primTy)

type instance PP.Ann (PrimType primTy) = PPAnn primTy

instance PP.PrettySyntax primTy => PP.PrettySyntax (PrimType primTy) where
  pretty' tys =
    PP.parensP' PAPunct PP.Outer $
      PP.sepA (PP.punctuateA arr (map pretty1 tys))
    where
      arr = pure $ PP.annotate' PAArrow "→"
      pretty1 =
        fmap (fmap $ Last . Just . PATy) .
        PP.withPrec (PP.Infix 0) . PP.pretty'

instance HR.ToPPAnn (PP.Ann ty) => HR.ToPPAnn (PPAnn ty) where
  toPPAnn a = a >>= \case
    PAArrow -> pure HR.ATyCon
    PAPunct -> pure HR.APunct
    PATy a' -> HR.toPPAnn a'
