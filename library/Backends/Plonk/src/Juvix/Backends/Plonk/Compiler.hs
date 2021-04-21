{-# LANGUAGE LambdaCase #-}

module Juvix.Backends.Plonk.Compiler where

import Data.List ((!!))
import qualified Data.Map as Map
import Juvix.Backends.Plonk.Builder as P
import Juvix.Backends.Plonk.Circuit as P
import Juvix.Backends.Plonk.IR as P
import Juvix.Backends.Plonk.Types as P
import qualified Juvix.Core.ErasedAnn.Types as Ann
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol

-- translate case statements to conditionals (nested)
-- inline lambdas
-- convert datatypes to some field element representation
-- etc

compileBinOp :: (Show f, Integral f) => Map NameSymbol.T Wire -> BinOp f a -> [FFAnnTerm f] -> IRM f (Either Wire (AffineCircuit Wire f))
compileBinOp m op args = do
  let e1 = args !! 0
  let e2 = args !! 1
  e1Out <- addVar <$> compileTerm e1 m mempty
  e2Out <- addVar <$> compileTerm e2 m mempty
  case op of
    BAdd -> pure . Right $ Add e1Out e2Out
    BMul -> do
      tmp1 <- mulToImm (Right e1Out) (Right e2Out)
      pure . Left $ tmp1
    BExp -> do
      let (Ann.Ann _ _ (Ann.Prim (PConst exponent))) = e2
      tmp1 <- mulToImm (Right e1Out) (Right e1Out)
      Left <$> foldrM f tmp1 (replicate (fromIntegral $ exponent - 2) 0)
      where
        f n tmp = mulToImm (Left tmp) (Right e1Out)
    -- SUB(x, y) = x + (-y)
    BSub -> pure . Right $ Add e1Out (ScalarMul (-1) e2Out)
    BAnd -> do
      tmp1 <- mulToImm (Right e1Out) (Right e2Out)
      pure . Left $ tmp1
    BOr -> do
      -- OR(input1, input2) = (input1 + input2) - (input1 * input2)
      tmp1 <- imm
      emit $ MulGate e1Out e2Out tmp1
      pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-1) (Var tmp1))
    BXor -> do
      -- XOR(input1, input2) = (input1 + input2) - 2 * (input1 * input2)
      tmp1 <- imm
      emit $ MulGate e1Out e2Out tmp1
      pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-2) (Var tmp1))

compileCompOp :: (Integral f, Show f) => Map NameSymbol.T Wire -> CompOp f -> [FFAnnTerm f] -> IRM f (Either Wire (AffineCircuit Wire f))
compileCompOp m op args = do
  let lhs = args !! 0
  let rhs = args !! 1
  case op of
    CEq -> do
      lhsSubRhs <- compileBinOp m BSub [lhs, rhs]
      eqInWire <- addWire lhsSubRhs
      eqFreeWire <- imm
      eqOutWire <- imm
      emit $ EqualGate eqInWire eqFreeWire eqOutWire
      -- eqOutWire == 0 if lhs == rhs, so we need to return 1 -
      -- neqOutWire instead.
      pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (Var eqOutWire))

compilePrim :: (Integral f, Show f) => PrimVal f -> Map NameSymbol.T Wire -> [FFAnnTerm f] -> IRM f (Either Wire (AffineCircuit Wire f))
compilePrim p m args = case p of
  P.PConst n -> pure . Right $ ConstGate n
  P.PAdd -> compileBinOp m BAdd args
  P.PSub -> compileBinOp m BSub args
  P.PMul -> compileBinOp m BMul args
  P.PExp -> compileBinOp m BExp args
  -- P.PEq -> compileBinOp m CEq args
  P.PEq -> compileCompOp m CEq args
  P.PAssertEq -> compileCompOp m CEq args
  n -> panic $ show n

compileTerm :: (Integral f, Show f) => FFAnnTerm f -> Map NameSymbol.T Wire -> [FFAnnTerm f] -> IRM f (Either Wire (AffineCircuit Wire f))
compileTerm _term@(Ann.Ann _ _ t) m a =
  case t of
    Ann.Prim p -> compilePrim p m a
    Ann.Var symbol -> case Map.lookup symbol m of
      Just v -> pure . Left $ v
      Nothing -> panic $ "Unable to find variable " <> show symbol
    Ann.AppM fun@(Ann.Ann _ _ v) args -> compileTerm fun m args
    -- case v of
    -- Ann.Prim p -> compilePrim p m args
    Ann.LamM _ args b -> do
      m' <- do
        pairs <-
          traverse
            ( \a -> do
                w <- P.freshInput -- TODO: Should this be input?
                pure (a, w)
            )
            args
        pure $ foldl' (\acc (k, v) -> Map.insert k v acc) m pairs
      compileTerm b m' mempty
    p@(Ann.PairM _a1 _a2) -> panic $ show p <> "notImplemented"
    u@Ann.UnitM -> panic $ show u <> "notImplemented"

compileTermWithWire :: (Integral f, Show f) => FFAnnTerm f -> IRM f Wire
compileTermWithWire term = do
  compileOut <- compileTerm term mempty mempty
  case compileOut of
    Left wire -> pure wire
    Right circ -> do
      wire <- freshOutput
      emit $ MulGate (ConstGate 1) circ wire
      pure wire
-- do
-- traverse (P.deref <$> P.freshInput) args
-- v <- lambda c a b ty
-- consVal v ty
-- pure v
