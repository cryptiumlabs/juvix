{-# LANGUAGE StandaloneDeriving #-}

module Juvix.Backends.Plonk.IR where

import Juvix.Backends.Plonk.Circuit
import Juvix.Library
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

data UnOp f a where
  UDup :: UnOp f f
  UIsZero :: UnOp f Bool
  UNot :: UnOp f Bool
  UShL :: Int -> UnOp f f
  UShR :: Int -> UnOp f f
  URotL :: Int -> UnOp f f
  URotR :: Int -> UnOp f f
  UAssertEq :: UnOp f Bool
  UAssertIt :: UnOp f Bool

-- TODO: setpub
-- TODO: setpriv
-- TODO: mload
-- TODO: mstore

data BinOp f a where
  BAdd :: BinOp f f
  BSub :: BinOp f f
  BMul :: BinOp f f
  BDiv :: BinOp f f
  BMod :: BinOp f f
  BExp :: BinOp f f
  BAnd :: BinOp f Bool
  BOr :: BinOp f Bool
  BXor :: BinOp f Bool

-- | Comparing operators
data CompOp f where
  CGt :: CompOp f
  CGte :: CompOp f
  CLt :: CompOp f
  CLte :: CompOp f
  CEq :: CompOp f

-- | Intermediate representation of (arithmetic) expressions over a field @f@
-- with variable names/indices coming from @i@. @ty@ is the resulting value.
data IR i f ty where
  IConst :: f -> IR i f f
  IVar :: i -> IR i f f
  IUnOp :: UnOp f ty -> IR i f ty -> IR i f ty
  IBinOp :: BinOp f ty -> IR i f ty -> IR i f ty -> IR i f ty
  ICompOp :: CompOp f -> IR i f f -> IR i f f -> IR i f Bool
  IAcc :: [IR i f ty] -> [ty] -> IR i f ty
  IIf :: IR i f Bool -> IR i f ty -> IR i f ty -> IR i f ty
  IECAdd :: IR i f (f, f) -> IR i f (f, f) -> IR i f (f, f)

deriving instance (Show i, Show f, Show ty) => Show (IR i f ty)

deriving instance (Show f) => Show (CompOp f)

deriving instance (Show f) => Show (BinOp f a)

deriving instance (Show f) => Show (UnOp f a)

instance Pretty (BinOp f a) where
  pretty op = case op of
    BAdd -> text "+"
    BSub -> text "-"
    BMul -> text "*"
    BAnd -> text "&"
    BOr -> text "|"
    BXor -> text "^"
    BDiv -> text "/"
    BMod -> text "%"
    BExp -> text "^"

instance Pretty (CompOp f) where
  pretty op = case op of
    CGt -> text ">"
    CGte -> text ">="
    CLt -> text "<"
    CLte -> text "<="
    CEq -> text "=="

instance Pretty (UnOp f a) where
  pretty op = case op of
    UNot -> text "!"
    UDup -> text "dup"
    UIsZero -> text "0?"
    UShL _ -> text "<<"
    UShR _ -> text ">>"
    URotL _ -> text "<<<"
    URotR _ -> text ">>>"
    UAssertEq -> text "=?"
    UAssertIt -> text "==?"

opPrecedence :: BinOp f a -> Int
opPrecedence BOr = 5
opPrecedence BXor = 5
opPrecedence BAnd = 5
opPrecedence BSub = 6
opPrecedence BAdd = 6
opPrecedence BDiv = 7
opPrecedence BMod = 7
opPrecedence BMul = 8
opPrecedence BExp = 8

instance (Pretty f, Pretty i, Pretty ty) => Pretty (IR i f ty) where
  pretty = prettyPrec 0
    where
      prettyPrec :: Int -> IR i f ty -> Doc
      prettyPrec p e =
        case e of
          IVar v -> pretty v
          IConst l -> pretty l
          IUnOp op e1 -> parens (pretty op <+> pretty e1)
          IBinOp op e1 e2 ->
            parensPrec (opPrecedence op) p $
              prettyPrec (opPrecedence op) e1
                <+> pretty op
                <+> prettyPrec (opPrecedence op) e2
          -- ICompOp op e1 e2 -> notImplemented
          -- IAcc _ _ -> notImplemented
          -- IECAdd _ _ -> notImplemented
          IIf b true false ->
            parensPrec 4 p (text "if" <+> pretty b <+> text "then" <+> pretty true <+> text "else" <+> pretty false)

-------------------------------------------------------------------------------
-- Evaluator
-------------------------------------------------------------------------------

-- | Evaluate arithmetic expressions directly, given an environment
evalIR ::
  (Bits f, Integral f, Show f) =>
  -- | variable lookup
  (i -> vars -> Maybe f) ->
  -- | expression to evaluate
  IR i f ty ->
  -- | input values
  vars ->
  -- | resulting value
  ty
evalIR lookupVar expr vars = case expr of
  IConst f -> f
  IVar i -> case lookupVar i vars of
    Just v -> v
    Nothing -> panic "TODO: incorrect var lookup"
  IUnOp op e1 -> case op of
    UNot -> not $ evalIR lookupVar e1 vars
    x -> panic $ "Not implemented!" <> show x
  IBinOp op e1 e2 ->
    evalIR lookupVar e1 vars `apply` evalIR lookupVar e2 vars
    where
      apply = case op of
        BAdd -> (+)
        BSub -> (-)
        BMul -> (*)
        BDiv -> div
        BMod -> mod
        BExp -> (^)
        BAnd -> (&&)
        BOr -> (||)
        BXor -> \x y -> (x || y) && not (x && y)
  -- ICompOp _ _ _ -> notImplemented
  -- IAcc _ _ -> notImplemented
  -- IECAdd _ _ -> notImplemented
  IIf b true false ->
    if evalIR lookupVar b vars
      then evalIR lookupVar true vars
      else evalIR lookupVar false vars
