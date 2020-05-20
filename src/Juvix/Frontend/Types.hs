{-# LANGUAGE TemplateHaskell #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/cryptiumlabs/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types where

import Control.Lens
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Product, Sum, Type)

data TopLevel
  = Type Type
  | ModuleOpen ModuleOpen
  | Signature Signature
  | Module Module
  | Function Function
  | TypeClass
  | TypeClassInstance
  deriving (Show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Type
  = Typ
      { typeUsage :: Maybe Usage,
        typeName :: !Symbol,
        typeArgs :: [Symbol],
        typeForm :: TypeSum
      }
  deriving (Show)

data TypeSum
  = Alias Alias
  | Data Data
  deriving (Show)

-- | 'Data' is the data declaration in the Juvix language
data Data
  = Arrowed
      { dataArrow :: Expression,
        dataAdt :: Adt
      }
  | NonArrowed
      { dataAdt :: Adt
      }
  deriving (Show)

newtype Alias
  = AliasDec
      {aliasType' :: Expression}
  deriving (Show)

--------------------------------------------------
-- Arrows
--------------------------------------------------
data NamedType
  = NamedType
      { nameRefineName :: !Name,
        namedRefineRefine :: Expression
      }
  deriving (Show)

-- TODO ∷ change TypeName to TypeNameModule
data TypeRefine
  = TypeRefine
      { typeRefineName :: Expression,
        typeRefineRefinement :: Expression
      }
  deriving (Show)

--------------------------------------------------
-- Types Misc
--------------------------------------------------

data Name
  = Implicit !Symbol
  | Concrete !Symbol
  deriving (Show)

data ArrowSymbol
  = ArrowUse Usage.T
  | ArrowExp Usage
  deriving (Show)

-- TODO ∷ finish this type!
newtype UniverseExpression
  = UniverseExpression Symbol
  deriving (Show)

--------------------------------------------------
-- ADTs
--------------------------------------------------

data Adt
  = Sum (NonEmpty Sum)
  | Product Product
  deriving (Show)

data Sum
  = S
      { sumConstructor :: !Symbol,
        sumValue :: !(Maybe Product)
      }
  deriving (Show)

data Product
  = Record !Record
  | Arrow Expression
  | ADTLike [Expression]
  deriving (Show)

data Record
  = Record'
      { recordFields :: NonEmpty NameType,
        recordFamilySignature :: Maybe Expression
      }
  deriving (Show)

data NameType
  = NameType
      { nameTypeSignature :: Expression,
        nameTypeName :: !Name
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- Functions And Modules
--------------------------------------------------------------------------------

-- | 'Function' is a normal signature with a name arguments and a body
-- that may or may not have a guard before it
newtype Function
  = Func (FunctionLike Expression)
  deriving (Show)

-- | 'Module' is like function, however it allows multiple top levels
newtype Module
  = Mod (FunctionLike (NonEmpty TopLevel))
  deriving (Show)

-- | 'FunctionLike' is the generic version for both modules and functions
data FunctionLike a
  = Like
      { functionLikedName :: Symbol,
        functionLikeArgs :: [Arg],
        functionLikeBody :: GuardBody a
      }
  deriving (Show)

-- | 'GuardBody' determines if a form is a guard or a body
data GuardBody a
  = Body a
  | Guard (Cond a)
  deriving (Show)

newtype ModuleOpen
  = Open ModuleName
  deriving (Show)

data ModuleOpenExpr
  = OpenExpress
      { moduleOpenExprModuleN :: ModuleName,
        moduleOpenExprExpr :: Expression
      }
  deriving (Show)

-- Very similar to name, but match instead of symbol
data Arg
  = ImplicitA MatchLogic
  | ConcreteA MatchLogic
  deriving (Show)

type ModuleName = NameSymb

newtype Cond a
  = C (NonEmpty (CondLogic a))
  deriving (Show)

data CondLogic a
  = CondExpression
      { condLogicPred :: Expression,
        condLogicBody :: a
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

data Signature
  = Sig
      { signatureName :: Symbol,
        signatureUsage :: Maybe Usage,
        signatureArrowType :: Expression,
        signatureConstraints :: [Expression]
      }
  deriving (Show)

type Usage = Expression

--------------------------------------------------------------------------------
-- Type Classes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = Cond (Cond Expression)
  | Constant Constant
  | Let Let
  | Match Match
  | Name NameSymb
  | OpenExpr ModuleOpenExpr
  | Lambda Lambda
  | Application Application
  | Block Block
  | Infix Infix
  | ExpRecord ExpRecord
  | Do Do
  | -- Added due to merge
    ArrowE ArrowExp
  | NamedTypeE NamedType
  | RefinedE TypeRefine
  | UniverseName UniverseExpression
  deriving (Show)

data ArrowExp
  = Arr'
      { arrowExpLeft :: Expression,
        arrowExpUsage :: Usage,
        arrowExpRight :: Expression
      }
  deriving (Show)

data Constant
  = Number Numb
  | String String'
  deriving (Show)

data Numb
  = Integer' Integer
  | Double' Double
  deriving (Show)

newtype String'
  = Sho Text
  deriving (Show)

newtype Block
  = Bloc
      {blockExpr :: Expression}
  deriving (Show)

data Lambda
  = Lamb
      { lambdaArgs :: NonEmpty MatchLogic,
        lambdaBody :: Expression
      }
  deriving (Show)

data Application
  = App
      { applicationName :: Expression,
        applicationArgs :: NonEmpty Expression
      }
  deriving (Show)

newtype Do
  = Do' (NonEmpty DoBody)
  deriving (Show)

data DoBody
  = DoBody
      { doBodyName :: Maybe NameSymb,
        doBodyExpr :: Expression
      }
  deriving (Show)

newtype ExpRecord
  = ExpressionRecord
      { expRecordFields :: NonEmpty (NameSet Expression)
      }
  deriving (Show)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

-- TODO ∷ fix let
data Let
  = Let'
      { letBindings :: NonEmpty Binding,
        letBody :: Expression
      }
  deriving (Show)

data Binding
  = Bind
      { bindingPattern :: MatchLogic,
        bindingBody :: Expression
      }
  deriving (Show)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

data Infix
  = Inf
      { infixLeft :: Expression,
        infixOp :: NameSymb,
        infixRight :: Expression
      }
  deriving (Show)

--------------------------------------------------
-- Matching
--------------------------------------------------

data Match
  = Match'
      { matchOn :: Expression,
        matchBindigns :: NonEmpty MatchL
      }
  deriving (Show)

data MatchL
  = MatchL
      { matchLPattern :: MatchLogic,
        matchLBody :: Expression
      }
  deriving (Show)

-- TODO ∷ add literals to the match
data MatchLogic
  = MatchLogic
      { matchLogicContents :: MatchLogicStart,
        matchLogicNamed :: Maybe NameSymb
      }
  deriving (Show)

data MatchLogicStart
  = MatchCon ConstructorName [MatchLogic]
  | MatchName Symbol
  | MatchConst Constant
  | MatchRecord (NonEmpty (NameSet MatchLogic))
  deriving (Show)

data NameSet t
  = Punned NameSymb
  | NonPunned NameSymb t
  deriving (Show)

type ConstructorName = NameSymb

type NameSymb = NonEmpty Symbol

--------------------------------------------------------------------------------
-- Lens creation
--------------------------------------------------------------------------------

makeLensesWith camelCaseFields ''Data

makeLensesWith camelCaseFields ''Type

makeLensesWith camelCaseFields ''Sum

makeLensesWith camelCaseFields ''Record

makeLensesWith camelCaseFields ''NamedType

makeLensesWith camelCaseFields ''TypeRefine

makeLensesWith camelCaseFields ''CondLogic

makeLensesWith camelCaseFields ''Let

makeLensesWith camelCaseFields ''Match

makeLensesWith camelCaseFields ''MatchL

makeLensesWith camelCaseFields ''MatchLogic

makeLensesWith camelCaseFields ''Binding

makeLensesWith camelCaseFields ''FunctionLike

makeLensesWith camelCaseFields ''Module

makeLensesWith camelCaseFields ''Function

makeLensesWith camelCaseFields ''Lambda

makeLensesWith camelCaseFields ''Application

makeLensesWith camelCaseFields ''Signature

makeLensesWith camelCaseFields ''Block

makeLensesWith camelCaseFields ''Do

makeLensesWith camelCaseFields ''DoBody

makeLensesWith camelCaseFields ''ModuleOpenExpr

makeLensesWith camelCaseFields ''Infix

makeLensesWith camelCaseFields ''ExpRecord

makePrisms ''TypeSum
