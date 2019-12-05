{-# LANGUAGE NamedFieldPuns #-}

-- |
-- - Has the code necessary to generate LLVM Code
module Juvix.Backends.LLVM.Codegen.Block where

import Data.ByteString.Short
import Juvix.Backends.LLVM.Codegen.Shared
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import Juvix.Library.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
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
  pure (succ i)

emptyBlock ∷ Int → BlockState
emptyBlock i = BlockState i [] Nothing

createBlocks ∷ HasState "blocks" (HashMap Name BlockState) f ⇒ f [BasicBlock]
createBlocks = fmap makeBlock . sortBlocks . Map.toList <$> get @"blocks"

makeBlock ∷ (Name, BlockState) → BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = undefined -- error $ "Block has no terminator: " <> show l

sortBlocks ∷ [(a, BlockState)] → [(a, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName ∷ IsString p ⇒ p
entryBlockName = "entry"

--------------------------------------------------------------------------------
-- Module Level
--------------------------------------------------------------------------------

emptyModule ∷ ShortByteString → Module
emptyModule label = AST.defaultModule {moduleName = label}

addDefn ∷ HasState "moduleDefinitions" [Definition] m ⇒ Definition → m ()
addDefn d = modify @"moduleDefinitions" (<> [d])

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
      (PointerType (FunctionType retty (fst <$> argtys) isVarArgs) (AddrSpace 0))
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
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  Symbol →
  [(Type.Type, Name.Name)] →
  m ()
makeFunction name args = do
  entry ← addBlock name
  _ ← setBlock entry
  -- Maybe not needed?
  traverse_
    ( \(typ, nam) → do
        var ← alloca typ
        store var (local typ nam)
        assign (nameToSymbol nam) var
    )
    args

defineFunctionGen ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
    HasState "moduleDefinitions" [Definition] m,
    HasState "names" Names m,
    HasState "symtab" (HashMap Symbol Operand) m
  ) ⇒
  Bool →
  Type →
  Symbol →
  [(Type, Name)] →
  m a →
  m Operand
defineFunctionGen bool retty name args body =
  (makeFunction name args >> body >> createBlocks) >>= defineGen bool retty name args

defineFunction,
  defineFunctionVarArgs ∷
    ( HasThrow "err" Errors m,
      HasState "blockCount" Int m,
      HasState "blocks" (HashMap Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name m,
      HasState "moduleDefinitions" [Definition] m,
      HasState "names" Names m,
      HasState "symtab" (HashMap Symbol Operand) m
    ) ⇒
    Type →
    Symbol →
    [(Type, Name)] →
    m a →
    m Operand
defineFunction = defineFunctionGen False
defineFunctionVarArgs = defineFunctionGen True

--------------------------------------------------------------------------------
-- Block Stack
--------------------------------------------------------------------------------

entry ∷ (HasState "currentBlock" Name m) ⇒ m Name
entry = get @"currentBlock"

getBlock ∷ (HasState "currentBlock" Name m) ⇒ m Name
getBlock = entry

addBlock ∷
  ( HasState "blockCount" Int m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "names" Names m
  ) ⇒
  Symbol →
  m Name
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
  ( HasState "blocks" (HashMap Name v) m,
    HasState "currentBlock" Name m
  ) ⇒
  v →
  m ()
modifyBlock new = do
  active ← get @"currentBlock"
  modify @"blocks" (Map.insert active new)

current ∷
  ( HasState "blocks" (HashMap Name b) m,
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

externf ∷
  ( HasState "symtab" SymbolTable m,
    HasThrow "err" Errors m
  ) ⇒
  Name →
  m Operand
externf name = getvar (nameToSymbol name)

nameToSymbol ∷ Name → Symbol
nameToSymbol (UnName n) = (intern (show n))
nameToSymbol (Name n) = (intern (show n))

local ∷ Type → Name → Operand
local = LocalReference

instr ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  Instruction →
  m Operand
instr typ ins = do
  n ← fresh
  let ref = UnName n
  blk ← current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  pure (local typ ref)

unnminstr ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
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
  ( HasState "blocks" (HashMap Name BlockState) m,
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
      (PointerType (FunctionType retty (fst <$> argtys) False) (AddrSpace 0))
      (mkName label)

--------------------------------------------------------------------------------
-- Memory management
--------------------------------------------------------------------------------

-- malloc & free need to be defined once and then can be called normally with `externf`

defineMalloc ∷
  ∀ m.
  ( HasState "moduleDefinitions" [Definition] m,
    HasState "symtab" (HashMap Symbol Operand) m
  ) ⇒
  m ()
defineMalloc = do
  let name = "malloc"
  op ← external voidStarTy name [(size_t, "size")]
  modify @"symtab" (Map.insert name op)
  pure ()

defineFree ∷
  ∀ m.
  ( HasState "moduleDefinitions" [Definition] m,
    HasState "symtab" (HashMap Symbol Operand) m
  ) ⇒
  m ()
defineFree = do
  let name = "free"
  op ← external voidTy name []
  modify @"symtab" (Map.insert name op)
  pure ()

--------------------------------------------------------------------------------
-- Integer Operations
--------------------------------------------------------------------------------

sdiv,
  udiv,
  add,
  sub,
  mul ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (HashMap Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name m
    ) ⇒
    Type →
    Operand →
    Operand →
    m Operand
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
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  IntPred.IntegerPredicate →
  Operand →
  Operand →
  m Operand
icmp iPred op1 op2 = instr Type.i1 $ ICmp iPred op1 op2 []

--------------------------------------------------------------------------------
-- Floating Point Operations
--------------------------------------------------------------------------------

-- Floating Point operations

fdiv,
  fadd,
  fsub,
  fmul ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (HashMap Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name m
    ) ⇒
    Type →
    Operand →
    Operand →
    m Operand
fdiv t a b = instr t $ FDiv noFastMathFlags a b []
fadd t a b = instr t $ FAdd noFastMathFlags a b []
fsub t a b = instr t $ FSub noFastMathFlags a b []
fmul t a b = instr t $ FMul noFastMathFlags a b []

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

ret ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNull ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  m (Named Terminator)
retNull = terminator $ Do $ Ret Nothing []

cbr ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Name →
  Name →
  m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

br ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Name →
  m (Named Terminator)
br val = terminator $ Do $ Br val []

phi ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  [(Operand, Name)] →
  m Operand
phi ty incoming = instr ty $ Phi ty incoming []

switch ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Name →
  [(C.Constant, Name)] →
  m (Named Terminator)
switch val default' dests = terminator $ Do $ Switch val default' dests []

generateIf ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
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
  -- Entry
  test ← icmp IntPred.EQ cond (ConstantOperand (C.Int 2 1))
  _ ← cbr test ifThen ifElse
  -- if.then
  setBlock ifThen
  -- Should be fine if we have the correct effects
  t ← tr
  _ ← br ifExit
  ifThen ← getBlock
  -- if.else
  setBlock ifElse
  f ← fl
  _ ← br ifExit
  ifElse ← getBlock
  -- if.exit
  setBlock ifExit
  phi ty [(t, ifThen), (f, ifElse)]

--------------------------------------------------------------------------------
-- Effects
--------------------------------------------------------------------------------

emptyArgs ∷ Functor f ⇒ f a1 → f (a1, [a2])
emptyArgs = fmap (\x → (x, []))

call ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
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

alloca ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  m Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

load ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  Operand →
  m Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

store ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Operand →
  m ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

--------------------------------------------------------------------------------
-- Casting Operations
--------------------------------------------------------------------------------

bitCast ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Operand →
  Type →
  m Operand
bitCast op typ = instr typ $ BitCast op typ []

ptrToInt ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Operand →
  Type →
  m Operand
ptrToInt op typ = instr typ $ AST.PtrToInt op typ []

--------------------------------------------------------------------------------
-- Pointer Operations
--------------------------------------------------------------------------------

-- | 'getElementPtr' gets an index of a struct or an array as a pointer
-- Takes a minimal data type to emulate named arguments
getElementPtr ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  MinimalPtr →
  m Operand
getElementPtr (Minimal address indices type') =
  instr type' $
    GetElementPtr
      { inBounds = True,
        metadata = [],
        address = address,
        indices = indices
      }

loadElementPtr ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  MinimalPtr →
  m Operand
loadElementPtr minimal = do
  ptr ← getElementPtr minimal
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

-- TODO ∷ use, so far the createVariant is only used

-- | creates a variant creation definition function
createVariantAllocaFunction ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
    HasState "moduleDefinitions" [Definition] m,
    HasState "names" Names m,
    HasState "symtab" (HashMap Symbol Operand) m,
    HasState "typTab" (HashMap Symbol Type) m,
    HasState "varTab" (HashMap Symbol SumInfo) m
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
             in defineFunction sumTyp varCName args $
                  do
                    sum ← alloca sumTyp
                    getEle ← getElementPtr $
                      Minimal
                        { Types.type' = sumTyp,
                          Types.address' = sum,
                          Types.indincies' = constant32List [0, 0]
                        }
                    store
                      getEle
                      (ConstantOperand (C.Int tag (toInteger offset)))
                    -- TODO ∷ remove the ! call here
                    let varType = typTable Map.! variantName
                    casted ← bitCast sum varType
                    foldM_
                      ( \i (_type', name) →
                          do
                            inst ← externf name
                            ele ←
                              getElementPtr $
                                Minimal
                                  { Types.type' = varType,
                                    Types.address' = casted,
                                    Types.indincies' = constant32List [0, i]
                                  }
                            store ele inst
                            pure (succ i)
                      )
                      1 -- not 0, as 0 is reserved for the tag that was set
                      args
                    _ ← ret casted
                    createBlocks

-- TODO ∷ Remove repeat code!!!

-- | creates a variant
createVariant ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) ⇒
  Symbol →
  t Operand →
  m Operand
createVariant variantName args = do
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
          Just sumTyp → do
            sum ← alloca sumTyp
            getEle ← getElementPtr $
              Minimal
                { Types.type' = sumTyp,
                  Types.address' = sum,
                  Types.indincies' = constant32List [0, 0]
                }
            store
              getEle
              (ConstantOperand (C.Int tag (toInteger offset)))
            -- TODO ∷ remove the ! call here
            let varType = typTable Map.! variantName
            casted ← bitCast sum varType
            foldM_
              ( \i inst → do
                  ele ←
                    getElementPtr $
                      Minimal
                        { Types.type' = varType,
                          Types.address' = casted,
                          Types.indincies' = constant32List [0, i]
                        }
                  store ele inst
                  pure (succ i)
              )
              1 -- not 0, as 0 is reserved for the tag that was set
              args
            pure casted

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign ∷
  (HasState "symtab" SymbolTable m) ⇒
  Symbol →
  Operand →
  m ()
assign var x = do
  modify @"symtab" (Map.insert var x)

getvar ∷
  ( HasState "symtab" SymbolTable m,
    HasThrow "err" Errors m
  ) ⇒
  Symbol →
  m Operand
getvar var = do
  syms ← get @"symtab"
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
