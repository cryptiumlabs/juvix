{-# LANGUAGE DeriveAnyClass #-}

module Juvix.Backends.Plonk.Circuit where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Juvix.Library
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

-- | Arithmetic circuits without multiplication, i.e. circuits
-- describe affine transformations.
data AffineCircuit i f
  = Add (AffineCircuit i f) (AffineCircuit i f)
  | ScalarMul f (AffineCircuit i f)
  | ConstGate f
  | Var i
  deriving (Read, Eq, Show, Generic, FromJSON, ToJSON)

fetchVars :: AffineCircuit Wire f -> [Wire]
fetchVars (Var i) = [i]
fetchVars (ConstGate _) = []
fetchVars (ScalarMul _ c) = fetchVars c
fetchVars (Add l r) = fetchVars l ++ fetchVars r

-- | Convert non-mul circuit to a vector representing the evaluation
-- function. We use a @Map@ to represent the potentially sparse vector.
affineCircuitToAffineMap ::
  (Num f, Ord i) =>
  -- | circuit to translate
  AffineCircuit i f ->
  -- | constant part and non-constant part
  (f, Map i f)
affineCircuitToAffineMap = \case
  Var i -> (0, Map.singleton i 1)
  Add l r -> (constLeft + constRight, Map.unionWith (+) vecLeft vecRight)
    where
      (constLeft, vecLeft) = affineCircuitToAffineMap l
      (constRight, vecRight) = affineCircuitToAffineMap r
  ScalarMul scalar expr -> (scalar * constExpr, fmap (scalar *) vecExpr)
    where
      (constExpr, vecExpr) = affineCircuitToAffineMap expr
  ConstGate f -> (f, Map.empty)

-- | Evaluate the arithmetic circuit without mul-gates on the given
-- input. Variable map is assumed to have all the variables referred
-- to in the circuit. Failed lookups are currently treated as 0.
evalAffineCircuit ::
  Num f =>
  -- | lookup function for variable mapping
  (i -> vars -> Maybe f) ->
  -- | variables
  vars ->
  -- | circuit to evaluate
  AffineCircuit i f ->
  f
evalAffineCircuit lookupVar vars = \case
  ConstGate f -> f
  Var i -> fromMaybe 0 $ lookupVar i vars
  Add l r -> evalAffineCircuit lookupVar vars l + evalAffineCircuit lookupVar vars r
  ScalarMul scalar expr -> evalAffineCircuit lookupVar vars expr * scalar

-- | Generate enough roots for a circuit
generateRoots ::
  Applicative m =>
  m f ->
  ArithCircuit f ->
  m [[f]]
generateRoots _ (ArithCircuit []) =
  pure []
generateRoots takeRoot (ArithCircuit (gate : gates)) =
  case gate of
    MulGate {} ->
      (\r rs -> [r] : rs)
        <$> takeRoot
        <*> generateRoots takeRoot (ArithCircuit gates)
    EqualGate {} ->
      (\r0 r1 rs -> [r0, r1] : rs)
        <$> takeRoot
        <*> takeRoot
        <*> generateRoots takeRoot (ArithCircuit gates)

data Wire
  = InputWire Int
  | IntermediateWire Int
  | OutputWire Int
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Pretty Wire where
  pretty (InputWire v) = text "input_" <> pretty v
  pretty (IntermediateWire v) = text "imm_" <> pretty v
  pretty (OutputWire v) = text "output_" <> pretty v

newtype ArithCircuit f = ArithCircuit [Gate Wire f]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Show f => Pretty (ArithCircuit f) where
  pretty (ArithCircuit gs) = vcat . map pretty $ gs

data Gate i f
  = RangeGate
      { rangeVar :: AffineCircuit i f,
        rangeNumBits :: Integer,
        rangeO :: i
      }
  | LookupGate
      { lookupA :: Integer,
        lookupB :: Integer,
        lookupC :: Integer,
        lookupD :: Maybe Integer,
        lookupPi :: f,
        lookupO :: i
      }
  | MulGate
      { mulL :: AffineCircuit i f,
        mulR :: AffineCircuit i f,
        mulO :: i
      }
  | BoolGate
      { boolVar :: AffineCircuit i f,
        boolO :: i
      }
  | LogicGate
      { logicL :: i,
        logicR :: i,
        logicO :: i
      }
  | EqualGate
      { eqI :: i,
        eqM :: i,
        eqO :: i
      }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance (Pretty i, Show f) => Pretty (Gate i f) where
  pretty (MulGate l r o) =
    hsep
      [ pretty o,
        text ":=",
        parens (pretty l),
        text "*",
        parens (pretty r)
      ]

instance (Pretty i, Show f) => Pretty (AffineCircuit i f) where
  pretty = prettyPrec 0
    where
      prettyPrec :: (Pretty i, Show f) => Int -> AffineCircuit i f -> Doc
      prettyPrec p e =
        case e of
          Var v ->
            pretty v
          ConstGate f ->
            text $ show f
          ScalarMul f e1 ->
            text (show f) <+> text "*" <+> parensPrec 7 p (prettyPrec p e1)
          Add e1 e2 ->
            parensPrec 6 p $
              prettyPrec 6 e1
                <+> text "+"
                <+> prettyPrec 6 e2

parensPrec :: Int -> Int -> Doc -> Doc
parensPrec opPrec p = if p > opPrec then parens else identity

-- | Evaluate an arithmetic circuit on a given environment containing
-- the inputs. Outputs the entire environment (outputs, intermediate
-- values and inputs).
evalArithCircuit ::
  forall f vars.
  (Bits f, Fractional f) =>
  -- | lookup a value at a wire
  (Wire -> vars -> Maybe f) ->
  -- | update a value at a wire
  (Wire -> f -> vars -> vars) ->
  -- | circuit to evaluate
  ArithCircuit f ->
  -- | input variables
  vars ->
  -- | input and output variables
  vars
evalArithCircuit lookupVar updateVar (ArithCircuit gates) vars =
  foldl' (evalGate lookupVar updateVar) vars gates

-- | Evaluate a single gate
evalGate ::
  (Bits f, Fractional f) =>
  -- | lookup a value at a wire
  (i -> vars -> Maybe f) ->
  -- | update a value at a wire
  (i -> f -> vars -> vars) ->
  -- | context before evaluation
  vars ->
  -- | gate
  Gate i f ->
  -- | context after evaluation
  vars
evalGate lookupVar updateVar vars gate =
  case gate of
    MulGate l r outputWire ->
      let lval = evalAffineCircuit lookupVar vars l
          rval = evalAffineCircuit lookupVar vars r
          res = lval * rval
       in updateVar outputWire res vars
    EqualGate i m outputWire ->
      case lookupVar i vars of
        Nothing ->
          panic "evalGate: the impossible happened"
        Just inp ->
          let res = if inp == 0 then 0 else 1
              mid = if inp == 0 then 0 else recip inp
           in updateVar outputWire res $
                updateVar m mid vars
