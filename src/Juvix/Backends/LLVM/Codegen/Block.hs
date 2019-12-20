-- |
-- - Has the code necessary to generate LLVM Code
module Juvix.Backends.LLVM.Codegen.Block where

import Data.ByteString.Short
import Juvix.Backends.LLVM.Codegen.Shared
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.ParameterAttribute as ParameterAttribute
import qualified LLVM.AST.Type as Type
import Prelude (String)

--------------------------------------------------------------------------------
-- Codegen Operations
--------------------------------------------------------------------------------

fresh ∷ (HasState "count" b m, Enum b) ⇒ m b
fresh = do
  i ← get @"count"
  put @"count" (succ i)
  pure i

resetCount ∷ (HasState "count" s m, Num s) ⇒ m ()
resetCount = put @"count" 0

emptyBlock ∷ Int → BlockState
emptyBlock i = BlockState i [] Nothing

createBlocks ∷
  ( HasState "blocks" (Map.HashMap Name BlockState) m,
    HasThrow "err" Errors m
  ) ⇒
  m [BasicBlock]
createBlocks = do
  sortedBlocks ← sortBlocks . Map.toList <$> get @"blocks"
  traverse makeBlock sortedBlocks

makeBlock ∷ HasThrow "err" Errors f ⇒ (Name, BlockState) → f BasicBlock
makeBlock (l, BlockState i s t) = maketerm t >>| BasicBlock l (reverse s)
  where
    maketerm (Just x) = pure x
    maketerm Nothing = throw @"err" (BlockLackingTerminator i)

sortBlocks ∷ [(a, BlockState)] → [(a, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName ∷ IsString p ⇒ p
entryBlockName = "entry"

emptyCodegen ∷ Types.CodegenState
emptyCodegen = Types.CodegenState
  { Types.currentBlock = mkName entryBlockName,
    Types.blocks = Map.empty,
    Types.symTab = Map.empty,
    Types.typTab = Map.empty,
    Types.varTab = Map.empty,
    Types.count = 0,
    Types.names = Map.empty,
    Types.blockCount = 1,
    Types.moduleAST = emptyModule "EAC"
  }

execEnvState ∷ Codegen a → SymbolTable → CodegenState
execEnvState (Types.CodeGen m) a = execState (runExceptT m) (emptyCodegen {Types.symTab = a})

evalEnvState ∷ Codegen a → SymbolTable → Either Errors a
evalEnvState (Types.CodeGen m) a = evalState (runExceptT m) (emptyCodegen {Types.symTab = a})

--------------------------------------------------------------------------------
-- Module Level
--------------------------------------------------------------------------------

emptyModule ∷ ShortByteString → Module
emptyModule label = AST.defaultModule {moduleName = label}

addDefn ∷ HasState "moduleDefinitions" [Definition] m ⇒ Definition → m ()
addDefn d = modify @"moduleDefinitions" (<> [d])

addType ∷ HasState "moduleDefinitions" [Definition] m ⇒ Name → Type → m ()
addType name typ =
  addDefn (TypeDefinition name (Just typ))

defineGen ∷
  HasState "moduleDefinitions" [Definition] m ⇒
  Bool →
  Type →
  Symbol →
  [(Type, Name)] →
  [BasicBlock] →
  m Operand
defineGen isVarArgs retty label argtys body = do
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { Global.parameters = params,
        -- Figure out which is best!
        Global.callingConvention = CC.GHC,
        Global.returnType = retty,
        Global.basicBlocks = body,
        Global.name = internName label
      }
  return
    $ ConstantOperand
    $ C.GlobalReference
      (Types.pointerOf (FunctionType retty (fst <$> argtys) isVarArgs))
      (internName label)
  where
    params = ((\(ty, nm) → Parameter ty nm []) <$> argtys, isVarArgs)

define,
  defineVarArgs ∷
    HasState "moduleDefinitions" [Definition] m ⇒
    Type →
    Symbol →
    [(Type, Name)] →
    [BasicBlock] →
    m Operand
define = defineGen False
defineVarArgs = defineGen True

makeFunction ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symTab" Types.SymbolTable m
  ) ⇒
  Symbol →
  [(Type.Type, Name.Name)] →
  m ()
makeFunction name args = do
  entry ← addBlock name
  _ ← setBlock entry
  traverse_
    (\(typ, nam) → assign (nameToSymbol nam) (local typ nam))
    args

defineFunctionGen ∷
  Types.Define m ⇒ Bool → Type → Symbol → [(Type, Name)] → m a → m Operand
defineFunctionGen bool retty name args body = do
  oldSymTab ← get @"symTab"
  -- flush the blocks so we can have clean block for functions
  put @"blocks" Map.empty
  resetCount
  functionOperand ←
    (makeFunction name args >> body >> createBlocks) >>= defineGen bool retty name args
  -- TODO ∷ figure out if LLVM functions can leak out of their local scope
  put @"symTab" oldSymTab
  -- flush out blocks after functions
  -- comment for debugging!
  put @"blocks" Map.empty
  resetCount
  assign name functionOperand
  pure functionOperand

defineFunction,
  defineFunctionVarArgs ∷
    Define m ⇒ Type → Symbol → [(Type, Name)] → m a → m Operand
defineFunction = defineFunctionGen False
defineFunctionVarArgs = defineFunctionGen True

--------------------------------------------------------------------------------
-- Block Stack
--------------------------------------------------------------------------------

entry ∷ (HasState "currentBlock" Name m) ⇒ m Name
entry = get @"currentBlock"

getBlock ∷ (HasState "currentBlock" Name m) ⇒ m Name
getBlock = entry

addBlock ∷ NewBlock m ⇒ Symbol → m Name
addBlock bname = do
  bls ← get @"blocks"
  ix ← get @"blockCount"
  nms ← get @"names"
  let new = emptyBlock ix

      (qname, supply) = uniqueName bname nms

      name = internName qname

  put @"blocks" (Map.insert name new bls)
  put @"blockCount" (succ ix)
  put @"names" supply
  return name

setBlock ∷ HasState "currentBlock" Name m ⇒ Name → m ()
setBlock bName = put @"currentBlock" bName

modifyBlock ∷
  ( HasState "blocks" (Map.T Name v) m,
    HasState "currentBlock" Name m
  ) ⇒
  v →
  m ()
modifyBlock new = do
  active ← get @"currentBlock"
  modify @"blocks" (Map.insert active new)

current ∷
  ( HasState "blocks" (Map.T Name b) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  m b
current = do
  c ← get @"currentBlock"
  b ← get @"blocks"
  case Map.lookup c b of
    Just x → return x
    Nothing → throw @"err" (NoSuchBlock (show c))

externf ∷ Externf m ⇒ Name → m Operand
externf name = getvar (nameToSymbol name)

nameToSymbol ∷ Name → Symbol
nameToSymbol (UnName n) = (intern (filter (/= '\"') (show n)))
nameToSymbol (Name n) = (intern (filter (/= '\"') (show n)))

local ∷ Type → Name → Operand
local = LocalReference

instr ∷ RetInstruction m ⇒ Type → Instruction → m Operand
instr typ ins = do
  n ← fresh
  let ref = UnName n
  blk ← current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  pure (local typ ref)

unnminstr ∷
  ( HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Instruction →
  m ()
unnminstr ins = do
  blk ← current
  let i = stack blk
  modifyBlock (blk {stack = (Do ins) : i})

terminator ∷
  ( HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Named Terminator →
  m (Named Terminator)
terminator trm = do
  blk ← current
  modifyBlock (blk {term = Just trm})
  return trm

--------------------------------------------------------------------------------
-- External linking
--------------------------------------------------------------------------------

external ∷ (HasState "moduleDefinitions" [Definition] m) ⇒ Type → String → [(Type, Name)] → m Operand
external retty label argtys = do
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { Global.parameters = ((\(ty, nm) → Parameter ty nm []) <$> argtys, False),
        Global.callingConvention = CC.Fast, -- TODO: Do we always want this?
        Global.returnType = retty,
        Global.basicBlocks = [],
        Global.name = mkName label,
        Global.linkage = L.External
      }
  return
    $ ConstantOperand
    $ C.GlobalReference
      (Types.pointerOf (FunctionType retty (fst <$> argtys) False))
      (mkName label)

--------------------------------------------------------------------------------
-- Memory management
--------------------------------------------------------------------------------

-- malloc & free need to be defined once and then can be called normally with `externf`

defineMalloc ∷ External m ⇒ m ()
defineMalloc = do
  let name = "malloc"
  op ← external (Types.pointerOf Type.i8) name [(Types.size_t, "size")]
  assign name op

defineFree ∷ External m ⇒ m ()
defineFree = do
  let name = "free"
  op ← external voidTy name [(Types.pointerOf Type.i8, "type")]
  assign name op

malloc ∷ Call m ⇒ Integer → Type → m Operand
malloc size type' = do
  malloc ← externf "malloc"
  voidPtr ←
    instr (Types.pointerOf Type.i8) $
      callConvention
        CC.Fast
        malloc
        (emptyArgs [Operand.ConstantOperand (C.Int Types.size_t_int size)])
  bitCast voidPtr type'

free ∷ Call m ⇒ Operand → m ()
free thing = do
  free ← externf "free"
  casted ← bitCast thing (Types.pointerOf Type.i8)
  unnminstr (callConvention CC.Fast free (emptyArgs [casted]))

--------------------------------------------------------------------------------
-- Integer Operations
--------------------------------------------------------------------------------

sdiv,
  udiv,
  add,
  sub,
  mul ∷
    RetInstruction m ⇒ Type → Operand → Operand → m Operand
sdiv t a b = instr t SDiv
  { exact = False,
    operand0 = a,
    operand1 = b,
    metadata = []
  }
udiv t a b = instr t UDiv
  { exact = False,
    operand0 = a,
    operand1 = b,
    metadata = []
  }
add t a b = instr t Add
  { -- no signed warp
    nsw = False,
    -- no unSigned warp
    nuw = False,
    operand0 = a,
    operand1 = b,
    metadata = []
  }
sub t a b = instr t Sub
  { -- no signed warp
    nsw = False,
    -- no unSigned warp
    nuw = False,
    operand0 = a,
    operand1 = b,
    metadata = []
  }
mul t a b = instr t Mul
  { -- no signed warp
    nsw = False,
    -- no unSigned warp
    nuw = False,
    operand0 = a,
    operand1 = b,
    metadata = []
  }

icmp ∷
  RetInstruction m ⇒ IntPred.IntegerPredicate → Operand → Operand → m Operand
icmp iPred op1 op2 = instr Type.i1 $ ICmp iPred op1 op2 []

--------------------------------------------------------------------------------
-- Floating Point Operations
--------------------------------------------------------------------------------

-- Floating Point operations

fdiv,
  fadd,
  fsub,
  fmul ∷
    RetInstruction m ⇒ Type → Operand → Operand → m Operand
fdiv t a b = instr t $ FDiv noFastMathFlags a b []
fadd t a b = instr t $ FAdd noFastMathFlags a b []
fsub t a b = instr t $ FSub noFastMathFlags a b []
fmul t a b = instr t $ FMul noFastMathFlags a b []

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

ret ∷ Instruct m ⇒ Operand → m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNull ∷ Instruct m ⇒ m (Named Terminator)
retNull = terminator $ Do $ Ret Nothing []

cbr ∷ Instruct m ⇒ Operand → Name → Name → m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

br ∷ Instruct m ⇒ Name → m (Named Terminator)
br val = terminator $ Do $ Br val []

phi ∷ RetInstruction m ⇒ Type → [(Operand, Name)] → m Operand
phi ty incoming = instr ty $ Phi ty incoming []

switch ∷ Instruct m ⇒ Operand → Name → [(C.Constant, Name)] → m (Named Terminator)
switch val default' dests = terminator $ Do $ Switch val default' dests []

generateIf ∷
  ( RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) ⇒
  Type →
  Operand →
  m Operand →
  m Operand →
  m Operand
generateIf ty cond tr fl = do
  ifThen ← addBlock "if.then"
  ifElse ← addBlock "if.else"
  ifExit ← addBlock "if.exit"
  -- %entry
  ------------------
  test ← icmp IntPred.EQ cond (ConstantOperand (C.Int 2 1))
  _ ← cbr test ifThen ifElse
  -- if.then
  ------------------
  setBlock ifThen
  t ← tr
  _ ← br ifExit
  ifThen ← getBlock
  -- if.else
  ------------------
  setBlock ifElse
  f ← fl
  _ ← br ifExit
  ifElse ← getBlock
  -- if.exit
  ------------------
  setBlock ifExit
  phi ty [(t, ifThen), (f, ifElse)]

--------------------------------------------------------------------------------
-- Effects
--------------------------------------------------------------------------------

emptyArgs ∷ Functor f ⇒ f a1 → f (a1, [a2])
emptyArgs = fmap (\x → (x, []))

callConvention ∷
  CC.CallingConvention →
  Operand →
  [(Operand, [ParameterAttribute.ParameterAttribute])] →
  Instruction
callConvention convention fn args = Call
  { functionAttributes = [],
    tailCallKind = Nothing,
    callingConvention = convention,
    returnAttributes = [],
    function = Right fn,
    arguments = args,
    metadata = []
  }

callVoid ∷
  RetInstruction m ⇒
  Operand →
  [(Operand, [ParameterAttribute.ParameterAttribute])] →
  m ()
callVoid fn args = unnminstr $
  Call
    { functionAttributes = [],
      tailCallKind = Nothing,
      callingConvention = CC.GHC,
      returnAttributes = [],
      function = Right fn,
      arguments = args,
      metadata = []
    }

call ∷
  RetInstruction m ⇒
  Type →
  Operand →
  [(Operand, [ParameterAttribute.ParameterAttribute])] →
  m Operand
call typ fn args = instr typ $
  Call
    { functionAttributes = [],
      tailCallKind = Nothing,
      callingConvention = CC.GHC,
      returnAttributes = [],
      function = Right fn,
      arguments = args,
      metadata = []
    }

-- TODO :: is the pointerOf on the ty needed
-- the LLVM8 testing on newKledi shows it being the same type back
-- however that would be incorrect?!
alloca ∷ RetInstruction m ⇒ Type → m Operand
alloca ty = instr (pointerOf ty) $ Alloca ty Nothing 0 []

load ∷ RetInstruction m ⇒ Type → Operand → m Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

store ∷ Instruct m ⇒ Operand → Operand → m ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

--------------------------------------------------------------------------------
-- Casting Operations
--------------------------------------------------------------------------------

bitCast ∷ RetInstruction m ⇒ Operand → Type → m Operand
bitCast op typ = instr typ $ BitCast op typ []

ptrToInt ∷ RetInstruction m ⇒ Operand → Type → m Operand
ptrToInt op typ = instr typ $ AST.PtrToInt op typ []

trunc ∷ RetInstruction m ⇒ Operand → Type → m Operand
trunc op typ = instr typ $ Trunc op typ []

--------------------------------------------------------------------------------
-- Pointer Operations
--------------------------------------------------------------------------------

-- | 'getElementPtr' gets an index of a struct or an array as a pointer
-- Takes a minimal data type to emulate named arguments
getElementPtr ∷ RetInstruction m ⇒ MinimalPtr → m Operand
getElementPtr (Minimal address indices type') =
  instr type' $
    GetElementPtr
      { inBounds = True,
        metadata = [],
        address = address,
        indices = indices
      }

loadElementPtr ∷ RetInstruction m ⇒ MinimalPtr → m Operand
loadElementPtr minimal = do
  ptr ←
    getElementPtr
      (minimal {Types.type' = pointerOf (Types.type' minimal)})
  load (Types.type' minimal) ptr

constant32List ∷ Functor f ⇒ f Integer → f Operand
constant32List = fmap (ConstantOperand . C.Int 32)

--------------------------------------------------------------------------------
-- Sum Type Declarations
--------------------------------------------------------------------------------
argsGen ∷ [Name.Name]
argsGen = (mkName . ("_" <>) . show) <$> ([1 ..] ∷ [Integer])

variantCreationName ∷ Symbol → Symbol
variantCreationName = (<> "_%func")

-- | Generic logic to create a variant, used in 'createVariantAllocaFunction'
-- and 'createVariantGen'
variantCreation ∷
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    Integral a,
    Foldable t
  ) ⇒
  Type →
  Symbol →
  Word32 →
  t Operand →
  a →
  (Type → m Operand) →
  m Operand
variantCreation sumTyp variantName tag args offset allocFn = do
  typTable ← get @"typTab"
  sum ← allocFn sumTyp
  getEle ← getElementPtr $
    -- The pointerOf is now correct!
    Minimal
      { Types.type' = Types.pointerOf (Type.IntegerType tag),
        Types.address' = sum,
        Types.indincies' = constant32List [0, 0]
      }
  store
    getEle
    (ConstantOperand (C.Int tag (toInteger offset)))
  -- TODO ∷ remove the ! call here
  let varType = typTable Map.! variantName
  casted ← bitCast sum (Types.pointerOf varType)
  foldM_
    ( \i inst → do
        ele ←
          getElementPtr $
            -- The pointerOf is now correct!
            Minimal
              { Types.type' = Types.pointerOf (intoStructTypeErr varType i),
                Types.address' = casted,
                Types.indincies' = constant32List [0, i]
              }
        store ele inst
        pure (succ i)
    )
    1 -- not 0, as 0 is reserved for the tag that was set
    args
  pure casted

-- TODO ∷ Remove repeat code!!!
-- TODO ∷ use, so far the createVariant is only used

-- | creates a variant creation definition function
createVariantAllocaFunction ∷
  ( Define m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) ⇒
  Symbol →
  [Type] →
  m Operand
createVariantAllocaFunction variantName argTypes = do
  varTable ← get @"varTab"
  typTable ← get @"typTab"
  case Map.lookup variantName varTable of
    Nothing →
      throw @"err" (NoSuchVariant (show variantName))
    Just
      ( S
          { sum' = sumName,
            offset = offset,
            tagSize' = tag
          }
        ) →
        case Map.lookup sumName typTable of
          Nothing →
            throw @"err" (DoesNotHappen ("type " <> show sumName <> "does not exist"))
          Just sumTyp →
            let varCName = variantCreationName variantName
                args = zip argTypes argsGen
             in defineFunction sumTyp varCName args $ do
                  argsName ← traverse (externf . snd) args
                  casted ← variantCreation sumTyp variantName tag argsName offset alloca
                  _ ← ret casted
                  createBlocks

createVariantGen ∷
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) ⇒
  Symbol →
  t Operand →
  (Type → m Operand) →
  m Operand
createVariantGen variantName args allocFn = do
  varTable ← get @"varTab"
  typTable ← get @"typTab"
  case Map.lookup variantName varTable of
    Nothing →
      throw @"err" (NoSuchVariant (show variantName))
    Just
      ( S
          { sum' = sumName,
            offset = offset,
            tagSize' = tag
          }
        ) →
        case Map.lookup sumName typTable of
          Nothing →
            throw @"err" (DoesNotHappen ("type " <> show sumName <> "does not exist"))
          Just sumTyp →
            variantCreation sumTyp variantName tag args offset allocFn

-- | Creates a variant by calling alloca
allocaVariant ∷
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) ⇒
  Symbol →
  t Operand →
  m Operand
allocaVariant variantName args = createVariantGen variantName args alloca

-- | Creates a variant by calling malloc
mallocVariant ∷
  ( Call m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) ⇒
  Symbol →
  t Operand →
  Integer →
  m Operand
mallocVariant variantName args size = createVariantGen variantName args (malloc size)

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign ∷ (HasState "symTab" SymbolTable m) ⇒ Symbol → Operand → m ()
assign var x = do
  modify @"symTab" (Map.insert var x)

getvar ∷
  ( HasState "symTab" SymbolTable m,
    HasThrow "err" Errors m
  ) ⇒
  Symbol →
  m Operand
getvar var = do
  syms ← get @"symTab"
  case Map.lookup var syms of
    Just x →
      return x
    Nothing →
      throw @"err"
        ( VariableNotInScope $
            "Local variable not in scope:"
              <> "\n syms: "
              <> show syms
              <> "\n var: "
              <> show var
        )

internName ∷ Symbol → Name
internName = mkName . unintern
