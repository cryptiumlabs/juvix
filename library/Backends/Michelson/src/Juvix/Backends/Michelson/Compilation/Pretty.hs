module Juvix.Backends.Michelson.Compilation.Pretty
  ( TyAnn' (..),
    TyAnn,
    TDoc,
    ValAnn' (..),
    ValAnn,
    VDoc,
  )
where

import Data.ByteString (unpack)
import Data.String (String)
import Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Core.Application as App
import qualified Juvix.Core.ErasedAnn.Types as CoreErased
import qualified Juvix.Core.IR.Types as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Library hiding (Option, Type, const)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.PrettyPrint as PP
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.Text as M
import qualified Michelson.TypeCheck as M
import qualified Michelson.Typed as MT
import qualified Michelson.Untyped as M
import qualified Michelson.Untyped.Instr as Instr
import qualified Michelson.Untyped.Type as M
import Numeric (showHex)
import Text.Show (showString)

data TyAnn'
  = TAPunct
  | TATyCon
  deriving (Eq, Ord, Show, Generic)

type TyAnn = Last TyAnn'

type TDoc = PP.Doc TyAnn

type instance PP.Ann PrimTy = TyAnn

tycon :: TDoc -> TDoc
tycon = PP.annotate' TATyCon

ptycon :: Applicative f => TDoc -> f TDoc
ptycon = pure . tycon

appT = PP.app' TAPunct

instance PP.PrettySyntax PrimTy where
  pretty' = \case
    PrimTy ty -> PP.pretty' ty
    Pair -> ptycon "pair"
    Lambda -> ptycon "lambda"
    Map -> ptycon "map"
    BigMap -> ptycon "big_map"
    Option -> ptycon "option"
    List -> ptycon "list"
    Set -> ptycon "set"
    ContractT -> ptycon "contract"
    Application t0 ts -> appT (PP.pretty' t0) (map PP.pretty' ts)

type instance PP.Ann M.Type = TyAnn

instance PP.PrettySyntax M.Type where
  pretty' (M.Type ty _) = case ty of
    M.TKey -> ptycon "key"
    M.TUnit -> ptycon "unit"
    M.TSignature -> ptycon "signature"
    M.TChainId -> ptycon "chain_id"
    M.TOption ty -> appT (ptycon "option") [PP.pretty' ty]
    M.TList ty -> appT (ptycon "list") [PP.pretty' ty]
    M.TSet ty -> appT (ptycon "set") [PP.pretty' ty]
    M.TOperation -> ptycon "operation"
    M.TContract ty -> appT (ptycon "contract") [PP.pretty' ty]
    M.TPair _ _ a b -> appT (ptycon "pair") [PP.pretty' a, PP.pretty' b]
    M.TOr _ _ a b -> appT (ptycon "or") [PP.pretty' a, PP.pretty' b]
    M.TLambda a b -> appT (ptycon "lambda") [PP.pretty' a, PP.pretty' b]
    M.TMap a b -> appT (ptycon "map") [PP.pretty' a, PP.pretty' b]
    M.TBigMap a b -> appT (ptycon "big_map") [PP.pretty' a, PP.pretty' b]
    M.TInt -> ptycon "int"
    M.TNat -> ptycon "nat"
    M.TString -> ptycon "string"
    M.TBytes -> ptycon "bytes"
    M.TMutez -> ptycon "mutez"
    M.TBool -> ptycon "bool"
    M.TKeyHash -> ptycon "key_hash"
    -- M.TBls12381Fr -> ptycon "bls12_381_fr"
    -- M.TBls12381G1 -> ptycon "bls12_381_g1"
    -- M.TBls12381G2 -> ptycon "bls12_381_g2"
    M.TTimestamp -> ptycon "timestamp"
    M.TAddress -> ptycon "address"

-- M.TNever -> ptycon "never"

data ValAnn'
  = VAPunct
  | VAInst
  | VAConst
  | VATyCon
  | VAKeyword

type ValAnn = Last ValAnn'

tyToValAnn :: TyAnn -> ValAnn
tyToValAnn = fmap \case
  TAPunct -> VAPunct
  TATyCon -> VATyCon

type VDoc = PP.Doc ValAnn

type instance PP.Ann RawPrimVal = ValAnn

type instance PP.Ann (M.Value' _) = ValAnn

type instance PP.Ann (Instr.InstrAbstract _) = ValAnn

punct :: VDoc -> VDoc
punct = PP.annotate' VAPunct

ppunct :: Applicative f => VDoc -> f VDoc
ppunct = pure . punct

keyword :: VDoc -> VDoc
keyword = PP.annotate' VAKeyword

pkeyword :: Applicative f => VDoc -> f VDoc
pkeyword = pure . keyword

inst :: VDoc -> VDoc
inst = PP.annotate' VAInst

pinst :: Applicative f => VDoc -> f VDoc
pinst = pure . inst

const :: VDoc -> VDoc
const = PP.annotate' VAConst

pconst :: Applicative f => VDoc -> f VDoc
pconst = pure . const

pconsts :: (Applicative f, Show a) => a -> f VDoc
pconsts = pconst . PP.show

appV = PP.app' VAPunct

unders :: PP.PrecReader m => Int -> m VDoc -> m VDoc
unders n doc = appV doc $ replicate n $ ppunct "_"

instance PP.PrettySyntax RawPrimVal where
  pretty' = \case
    Constant k -> PP.pretty' k
    Inst i -> PP.pretty' i
    AddN -> pinst "ADD"
    AddI -> pinst "ADD"
    AddTimeStamp -> pinst "ADD"
    AddMutez -> pinst "ADD"
    NegN -> pinst "NEG"
    NegI -> pinst "NEG"
    SubN -> pinst "SUB"
    SubI -> pinst "SUB"
    SubMutez -> pinst "SUB"
    SubTimeStamp -> pinst "SUB"
    MulI -> pinst "MUL"
    MulN -> pinst "MUL"
    MulMutez -> pinst "MUL"
    EDivI -> pinst "MUL"
    EDivN -> pinst "MUL"
    EDivMutez -> pinst "MUL"
    ORI -> pinst "OR"
    OrB -> pinst "OR"
    AndI -> pinst "AND"
    AndB -> pinst "AND"
    XorI -> pinst "XOR"
    XorB -> pinst "XOR"
    NotI -> pinst "NOT"
    NotB -> pinst "NOT"
    CompareI -> pinst "COMPARE"
    CompareS -> pinst "COMPARE"
    CompareP -> pinst "COMPARE"
    CompareTimeStamp -> pinst "COMPARE"
    CompareMutez -> pinst "COMPARE"
    CompareBytes -> pinst "COMPARE"
    CompareHash -> pinst "COMPARE"
    SizeMap -> pinst "SIZE"
    SizeSet -> pinst "SIZE"
    SizeList -> pinst "SIZE"
    SizeBytes -> pinst "SIZE"
    SizeS -> pinst "SIZE"
    MemSet -> pinst "MEM"
    MemMap -> pinst "MEM"
    UpdateSet -> pinst "UPDATE"
    UpdateMap -> pinst "UPDATE"
    UpdateBMap -> pinst "UPDATE"
    GetMap -> pinst "GET"
    GetBMap -> pinst "GET"
    Right' -> pinst "RIGHT"
    Left' -> pinst "LEFT"
    Nil -> unders 1 $ pinst "NIL"
    Cons -> pinst "CONS"
    None -> unders 1 $ pinst "NONE"
    EmptyS -> unders 1 $ pinst "EMPTY_SET"
    EmptyM -> unders 2 $ pinst "EMPTY_MAP"
    EmptyBM -> unders 2 $ pinst "EMPTY_BIG_MAP"
    Cast -> unders 1 $ pinst "CAST"
    Contract -> pinst "CONTRACT"
    CreateContract -> pinst "CREATE_CONTRACT"
    Loop -> pinst "LOOP"
    Iter -> pinst "ITER"
    MapOp -> pinst "MAP"

instance
  (PP.PrettySyntax op, PP.Ann op ~ ValAnn) =>
  PP.PrettySyntax (M.Value' op)
  where
  pretty' = \case
    M.ValueInt i -> pconsts i
    M.ValueString str -> pconsts $ M.unMText str
    M.ValueBytes (M.InternalByteString bs) -> pconst $ showBytes $ unpack bs
    M.ValueUnit -> pconst "Unit"
    M.ValueTrue -> pconst "True"
    M.ValueFalse -> pconst "False"
    M.ValuePair x y -> appV (pconst "Pair") [PP.pretty' x, PP.pretty' y]
    M.ValueLeft x -> appV (pconst "Left") [PP.pretty' x]
    M.ValueRight y -> appV (pconst "Right") [PP.pretty' y]
    M.ValueSome x -> appV (pconst "Some") [PP.pretty' x]
    M.ValueNone -> pconst "None"
    M.ValueNil -> pconst "Nil"
    M.ValueSeq vs -> prettyBlock vs
    M.ValueMap kvs -> prettyBlock kvs
    M.ValueLambda ops -> prettyBlock ops

type instance PP.Ann (M.Elt _) = ValAnn

instance
  (PP.PrettySyntax op, PP.Ann op ~ ValAnn) =>
  PP.PrettySyntax (M.Elt op)
  where
  pretty' (M.Elt k v) = appV (pconst "Elt") [PP.pretty' k, PP.pretty' v]

showBytes :: [Word8] -> VDoc
showBytes str =
  PP.string $ "0x" <> foldr (\x s -> showHex x . s) identity str ""

prettyTV ::
  (PP.PrettySyntax a, PP.Ann a ~ TyAnn, PP.PrecReader m) => a -> m VDoc
prettyTV = fmap (fmap tyToValAnn) . PP.pretty'

instance
  (PP.PrettySyntax op, PP.Ann op ~ ValAnn) =>
  PP.PrettySyntax (Instr.InstrAbstract op)
  where
  pretty' = \case
    Instr.EXT _ext -> ppunct "{}" -- FIXME
    Instr.DROPN n -> appV (pinst "DROP") [pconsts n]
    Instr.DROP -> pinst "DROP"
    Instr.DUP _ -> pinst "DUP"
    Instr.SWAP -> pinst "SWAP"
    Instr.DIG n -> appV (pinst "DIG") [pconsts n]
    Instr.DUG n -> appV (pinst "DUG") [pconsts n]
    Instr.PUSH _ ty x -> appV (pinst "DUG") [prettyTV ty, PP.pretty' x]
    Instr.SOME _ _ -> pinst "SOME"
    Instr.NONE _ _ ty -> appV (pinst "NONE") [prettyTV ty]
    Instr.UNIT _ _ -> pinst "UNIT"
    Instr.IF_NONE none some ->
      appV (pinst "IF_NONE") [prettyBlock none, prettyBlock some]
    Instr.PAIR _ _ _ _ -> pinst "PAIR"
    -- Instr.PAIRN _ n -> appV (pinst "PAIR") [pconsts n]
    -- Instr.UNPAIRN _ 2 -> pinst "UNPAIR"
    -- Instr.UNPAIRN _ n -> appV (pinst "UNPAIR") [pconsts n]
    Instr.CAR _ _ -> pinst "CAR"
    Instr.CDR _ _ -> pinst "CDR"
    Instr.LEFT _ _ _ _ ty -> appV (pinst "LEFT") [prettyTV ty]
    Instr.RIGHT _ _ _ _ ty -> appV (pinst "RIGHT") [prettyTV ty]
    Instr.IF_LEFT left right ->
      appV (pinst "IF_LEFT") [prettyBlock left, prettyBlock right]
    Instr.NIL _ _ ty -> appV (pinst "NIL") [prettyTV ty]
    Instr.CONS _ -> pinst "CONS"
    Instr.IF_CONS cons nil ->
      appV (pinst "IF_CONS") [prettyBlock cons, prettyBlock nil]
    Instr.SIZE _ -> pinst "SIZE"
    Instr.EMPTY_SET _ _ ty -> appV (pinst "EMPTY_SET") [prettyTV ty]
    Instr.EMPTY_MAP _ _ kty vty ->
      appV (pinst "EMPTY_MAP") [prettyTV kty, prettyTV vty]
    Instr.EMPTY_BIG_MAP _ _ kty vty ->
      appV (pinst "EMPTY_BIG_MAP") [prettyTV kty, prettyTV vty]
    Instr.MAP _ ops -> appV (pinst "MAP") [prettyBlock ops]
    Instr.ITER ops -> appV (pinst "ITER") [prettyBlock ops]
    Instr.MEM _ -> pinst "MEM"
    Instr.GET _ -> pinst "GET"
    -- Instr.GETN _ n -> appV (pinst "GET") [pconsts n]
    Instr.UPDATE _ -> pinst "UPDATE"
    Instr.IF tru fls ->
      appV (pinst "IF") [prettyBlock tru, prettyBlock fls]
    Instr.LOOP ops -> appV (pinst "LOOP") [prettyBlock ops]
    Instr.LOOP_LEFT ops -> appV (pinst "LOOP_LEFT") [prettyBlock ops]
    Instr.LAMBDA _ a b ops ->
      appV (pinst "LAMBDA") [prettyTV a, prettyTV b, prettyBlock ops]
    Instr.EXEC _ -> pinst "EXEC"
    Instr.APPLY _ -> pinst "APPLY"
    Instr.DIP ops -> appV (pinst "DIP") [prettyBlock ops]
    Instr.DIPN n ops -> appV (pinst "DIP") [pconsts n, prettyBlock ops]
    Instr.FAILWITH -> pinst "FAILWITH"
    Instr.CAST _ ty -> appV (pinst "FAILWITH") [prettyTV ty]
    Instr.RENAME _ -> pinst "RENAME"
    Instr.PACK _ -> pinst "PACK"
    Instr.UNPACK _ _ ty -> appV (pinst "UNPACK") [prettyTV ty]
    Instr.CONCAT _ -> pinst "CONCAT"
    Instr.SLICE _ -> pinst "SLICE"
    Instr.ISNAT _ -> pinst "ISNAT"
    Instr.ADD _ -> pinst "ADD"
    Instr.SUB _ -> pinst "SUB"
    Instr.MUL _ -> pinst "MUL"
    Instr.EDIV _ -> pinst "EDIV"
    Instr.ABS _ -> pinst "ABS"
    Instr.NEG _ -> pinst "NEG"
    Instr.LSL _ -> pinst "LSL"
    Instr.LSR _ -> pinst "LSR"
    Instr.OR _ -> pinst "OR"
    Instr.AND _ -> pinst "AND"
    Instr.XOR _ -> pinst "XOR"
    Instr.NOT _ -> pinst "NOT"
    Instr.COMPARE _ -> pinst "COMPARE"
    Instr.EQ _ -> pinst "EQ"
    Instr.NEQ _ -> pinst "NEQ"
    Instr.LT _ -> pinst "LT"
    Instr.GT _ -> pinst "GT"
    Instr.LE _ -> pinst "LE"
    Instr.GE _ -> pinst "GE"
    Instr.INT _ -> pinst "INT"
    Instr.SELF _ _ -> pinst "SELF"
    Instr.CONTRACT _ _ ty -> appV (pinst "SELF") [prettyTV ty]
    Instr.TRANSFER_TOKENS _ -> pinst "TRANSFER_TOKENS"
    Instr.SET_DELEGATE _ -> pinst "SET_DELEGATE"
    Instr.CREATE_CONTRACT _ _ k ->
      appV (pinst "CREATE_CONTRACT") [prettyContract k]
    Instr.IMPLICIT_ACCOUNT _ -> pinst "IMPLICIT_ACCOUNT"
    Instr.NOW _ -> pinst "NOW"
    Instr.AMOUNT _ -> pinst "AMOUNT"
    Instr.BALANCE _ -> pinst "BALANCE"
    Instr.CHECK_SIGNATURE _ -> pinst "CHECK_SIGNATURE"
    Instr.SHA256 _ -> pinst "SHA256"
    Instr.SHA512 _ -> pinst "SHA512"
    Instr.BLAKE2B _ -> pinst "BLAKE2B"
    Instr.HASH_KEY _ -> pinst "HASH_KEY"
    Instr.SOURCE _ -> pinst "SOURCE"
    Instr.SENDER _ -> pinst "SENDER"
    Instr.ADDRESS _ -> pinst "ADDRESS"
    Instr.CHAIN_ID _ -> pinst "CHAIN_ID"

-- |
-- As one of:
--
-- @{ a; b; c; }@
-- @
--   { a;
--     b;
--     c;
--   }
-- @
prettyBlock ::
  ( PP.PrettySyntax a,
    PP.Ann a ~ ValAnn,
    PP.PrecReader m,
    Foldable t
  ) =>
  t a ->
  m VDoc
prettyBlock ops =
  PP.sepA
    [ PP.hsepA
        [ ppunct "{",
          PP.sepA $ PP.punctuateA (ppunct ";") $ map PP.pretty' $ toList ops
        ],
      ppunct "}"
    ]

prettyContract ::
  (PP.PrettySyntax op, PP.Ann op ~ ValAnn, PP.PrecReader m) =>
  M.Contract' op ->
  m VDoc
prettyContract (M.Contract (M.ParameterType param _) storage code) =
  PP.vcatA
    [ PP.sepA [pkeyword "parameter", PP.hcatA [prettyTV param, ppunct ";"]],
      PP.sepA [pkeyword "storage", PP.hcatA [prettyTV storage, ppunct ";"]],
      prettyBlock code
    ]

type instance PP.Ann Instr.ExpandedOp = ValAnn

instance PP.PrettySyntax Instr.ExpandedOp where
  pretty' = \case
    Instr.PrimEx i -> PP.pretty' i
    Instr.SeqEx is -> prettyBlock is
    Instr.WithSrcEx _ i -> PP.pretty' i
