{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types to support partial application and polymorphic primitives.
module Juvix.Core.Application
  ( Return' (..),
    DeBruijn (..),
    IsParamVar (..),
    Arg' (..),
    Arg,
    pattern BoundArg,
    pattern FreeArg,
    Take (..),
    argToTake,
    takeToReturn,
  )
where

import Data.Bifoldable
import Data.Bitraversable
import qualified Juvix.Core.HR.Pretty as HR
import qualified Juvix.Core.IR.Types as IR
import Juvix.Library
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage

-- | A primitive along with its type, and possibly some arguments.
data Return' ext ty term
  = -- | Partially applied primitive holding the arguments already given.
    Cont
      { -- | Head of application, a fully evaluated term.
        fun :: Take ty term,
        -- | Arguments to the function.
        args :: [Arg' ext ty term],
        -- | Number of arguments still expected.
        numLeft :: Natural
      }
  | -- | A primitive with no arguments.
    Return
      { -- | Type of the return term.
        retType :: ty,
        -- | The term itself.
        retTerm :: term
      }
  deriving (Generic, Functor, Foldable, Traversable)

deriving instance
  (Show (ParamVar ext), Show ty, Show term) =>
  Show (Return' ext ty term)

deriving instance
  (Eq (ParamVar ext), Eq ty, Eq term) =>
  Eq (Return' ext ty term)

instance Bifunctor (Return' ext) where
  bimap = bimapDefault

instance Bifoldable (Return' ext) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Return' ext) where
  bitraverse f g = \case
    Cont s ts n ->
      Cont <$> bitraverse f g s
        <*> traverse (bitraverse f g) ts
        <*> pure n
    Return a s ->
      Return <$> f a <*> g s

-- | The representation of variables used in IR.Term' ext
class IsParamVar ext where
  type ParamVar ext :: Type

  -- Create a reference to a free variable.
  freeVar :: Proxy ext -> IR.GlobalName -> Maybe (ParamVar ext)

  -- Create a reference to a bound variable.
  boundVar :: Proxy ext -> IR.BoundVar -> Maybe (ParamVar ext)

-- | Representation of De Bruijn indexing.
data DeBruijn
  = -- | Reference to a bound variable.
    BoundVar IR.BoundVar
  | -- | Reference to a free variable.
    FreeVar IR.GlobalName
  deriving (Show, Eq, Generic)

instance IsParamVar IR.NoExt where
  type ParamVar IR.NoExt = DeBruijn
  freeVar _ = Just . FreeVar
  boundVar _ = Just . BoundVar

-- | Arguments to a function.
data Arg' ext ty term
  = -- | A variable to a term.
    VarArg (ParamVar ext)
  | -- | A fully evaluated term.
    TermArg (Take ty term)
  deriving (Generic, Functor, Foldable, Traversable)

-- | Simplification for 'Arg'' without any extensions.
type Arg = Arg' IR.NoExt

-- | Pattern synonym for bound arguments in DeBruijn terms.
pattern BoundArg ::
  (ParamVar ext ~ DeBruijn) => IR.BoundVar -> Arg' ext ty term
pattern BoundArg i = VarArg (BoundVar i)

-- | Pattern synonym for free arguments in DeBruijn terms.
pattern FreeArg ::
  (ParamVar ext ~ DeBruijn) => IR.GlobalName -> Arg' ext ty term
pattern FreeArg x = VarArg (FreeVar x)

{-# COMPLETE TermArg, BoundArg, FreeArg #-}

deriving instance
  (Show (ParamVar ext), Show ty, Show term) =>
  Show (Arg' ext ty term)

deriving instance
  (Eq (ParamVar ext), Eq ty, Eq term) =>
  Eq (Arg' ext ty term)

instance Bifunctor (Arg' ext) where bimap = bimapDefault

instance Bifoldable (Arg' ext) where bifoldMap = bifoldMapDefault

instance Bitraversable (Arg' ext) where
  bitraverse _ _ (VarArg x) = pure $ VarArg x
  bitraverse f g (TermArg t) = TermArg <$> bitraverse f g t

-- | An argument to a partially applied primitive, which must be fully-applied
-- itself.
data Take ty term = Take
  { -- | Usage annotation for quantified types.
    usage :: Usage.T,
    -- | The type of the term.
    type' :: ty,
    -- | The term itself.
    term :: term
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Bifunctor Take where
  bimap = bimapDefault

instance Bifoldable Take where
  bifoldMap = bifoldMapDefault

instance Bitraversable Take where
  bitraverse f g (Take π a s) = Take π <$> f a <*> g s

-- | Translate an 'Arg'' to a 'Take'. Only fully evaluated arguments are
-- returned, all others will result in an @empty@.
argToTake :: Alternative f => Arg' ext ty term -> f (Take ty term)
argToTake (TermArg t) = pure t
argToTake _ = empty

-- | Translate an 'Arg'' to a 'Return''.
argToReturn :: Alternative f => Arg' ext ty term -> f (Return' ext' ty term)
argToReturn = fmap takeToReturn . argToTake

-- | Translate a 'Take' into a 'Return''.
takeToReturn :: Take ty term -> Return' ext ty term
takeToReturn (Take {type', term}) = Return {retType = type', retTerm = term}

data PPAnn' ty term
  = APunct
  | ATyAnn (PP.Ann ty)
  | ATmAnn (PP.Ann term)
  | AVar

type PPAnn ty term = Last (PPAnn' ty term)

instance
  (HR.ToPPAnn (PP.Ann ty), HR.ToPPAnn (PP.Ann term)) =>
  HR.ToPPAnn (PPAnn ty term)
  where
  toPPAnn a =
    a >>= \case
      APunct -> pure HR.APunct
      ATyAnn ann -> HR.toPPAnn ann
      ATmAnn ann -> HR.toPPAnn ann
      AVar -> pure HR.AName

type instance PP.Ann (Take ty term) = PPAnn ty term

instance
  (PP.PrettySyntax ty, PP.PrettySyntax term) =>
  PP.PrettySyntax (Take ty term)
  where
  pretty' (Take {usage, term, type'}) = prettyTyped usage term type'

prettyTyped ::
  ( PP.PrecReader m,
    PP.PrettySyntax ty,
    PP.PrettySyntax term
  ) =>
  Usage.T ->
  term ->
  ty ->
  m (PP.Doc (Last (PPAnn' ty term)))
prettyTyped π tm ty =
  PP.parens' APunct
    <$> PP.hangA
      PP.indentWidth
      ( PP.hsepA
          [ PP.noAnn <$> PP.pretty' π,
            ppunct "|",
            fmap (Last . Just . ATmAnn) <$> PP.pretty' tm
          ]
      )
      ( PP.hsepA
          [ ppunct ":",
            fmap (Last . Just . ATyAnn) <$> PP.pretty' ty
          ]
      )
  where
    ppunct = pure . PP.annotate' APunct

type instance PP.Ann (Arg' _ ty term) = PPAnn ty term

instance
  (PP.PrettySyntax ty, PP.PrettySyntax term, PP.PrettySyntax (ParamVar ext)) =>
  PP.PrettySyntax (Arg' ext ty term)
  where
  pretty' = \case
    VarArg x -> PP.annotate' AVar . PP.noAnn <$> PP.pretty' x
    TermArg t -> PP.pretty' t

type instance PP.Ann (Return' _ ty term) = PPAnn ty term

instance
  (PP.PrettySyntax ty, PP.PrettySyntax term, PP.PrettySyntax (ParamVar ext)) =>
  PP.PrettySyntax (Return' ext ty term)
  where
  pretty' = \case
    Cont {fun, args} -> PP.app' APunct (PP.pretty' fun) (map PP.pretty' args)
    Return {retTerm, retType} -> prettyTyped Usage.Omega retTerm retType

type instance PP.Ann DeBruijn = ()

instance PP.PrettySyntax DeBruijn where
  pretty' = \case
    BoundVar i -> pure $ PP.show i
    FreeVar x -> PP.pretty' x
