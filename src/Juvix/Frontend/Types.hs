{-# LANGUAGE DeriveAnyClass #-}
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
  deriving (Show, Generic, NFData)

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
  deriving (Show, Generic, NFData)

data TypeSum
  = Alias Alias
  | Data Data
  deriving (Show, Generic, NFData)

-- | 'Data' is the data declaration in the Juvix language
data Data
  = Arrowed
      { dataArrow :: Expression,
        dataAdt :: Adt
      }
  | NonArrowed
      { dataAdt :: Adt
      }
  deriving (Show, Generic, NFData)

newtype Alias
  = AliasDec
      {aliasType' :: Expression}
  deriving (Show, Generic)
  deriving newtype (NFData)

--------------------------------------------------
-- Arrows
--------------------------------------------------
data NamedType
  = NamedType
      { nameRefineName :: !Name,
        namedRefineRefine :: Expression
      }
  deriving (Show, Generic, NFData)

-- TODO ∷ change TypeName to TypeNameModule
data TypeRefine
  = TypeRefine
      { typeRefineName :: Expression,
        typeRefineRefinement :: Expression
      }
  deriving (Show, Generic, NFData)

--------------------------------------------------
-- Types Misc
--------------------------------------------------

data Name
  = Implicit !Symbol
  | Concrete !Symbol
  deriving (Show, Generic, NFData)

data ArrowSymbol
  = ArrowUse Usage.T
  | ArrowExp Usage
  deriving (Show, Generic, NFData)

-- TODO ∷ finish this type!
newtype UniverseExpression
  = UniverseExpression Symbol
  deriving (Show, Generic)
  deriving newtype (NFData)

--------------------------------------------------
-- ADTs
--------------------------------------------------

data Adt
  = Sum (NonEmpty Sum)
  | Product Product
  deriving (Show, Generic, NFData)

data Sum
  = S
      { sumConstructor :: !Symbol,
        sumValue :: !(Maybe Product)
      }
  deriving (Show, Generic, NFData)

data Product
  = Record !Record
  | Arrow Expression
  | ADTLike [Expression]
  deriving (Show, Generic, NFData)

data Record
  = Record'
      { recordFields :: NonEmpty NameType,
        recordFamilySignature :: Maybe Expression
      }
  deriving (Show, Generic, NFData)

data NameType
  = NameType
      { nameTypeSignature :: Expression,
        nameTypeName :: !Name
      }
  deriving (Show, Generic, NFData)

--------------------------------------------------------------------------------
-- Functions And Modules
--------------------------------------------------------------------------------

-- | 'Function' is a normal signature with a name arguments and a body
-- that may or may not have a guard before it
newtype Function
  = Func (FunctionLike Expression)
  deriving (Show, Generic)
  deriving newtype (NFData)

-- | 'Module' is like function, however it allows multiple top levels
newtype Module
  = Mod (FunctionLike (NonEmpty TopLevel))
  deriving (Show, Generic)
  deriving newtype (NFData)

-- | 'FunctionLike' is the generic version for both modules and functions
data FunctionLike a
  = Like
      { functionLikedName :: Symbol,
        functionLikeArgs :: [Arg],
        functionLikeBody :: GuardBody a
      }
  deriving (Show, Generic, NFData)

-- | 'GuardBody' determines if a form is a guard or a body
data GuardBody a
  = Body a
  | Guard (Cond a)
  deriving (Show, Generic, NFData)

newtype ModuleOpen
  = Open ModuleName
  deriving (Show, Generic)
  deriving newtype (NFData)

data ModuleOpenExpr
  = OpenExpress
      { moduleOpenExprModuleN :: ModuleName,
        moduleOpenExprExpr :: Expression
      }
  deriving (Show, Generic, NFData)

-- Very similar to name, but match instead of symbol
data Arg
  = ImplicitA MatchLogic
  | ConcreteA MatchLogic
  deriving (Show, Generic, NFData)

type ModuleName = NameSymb

newtype Cond a
  = C (NonEmpty (CondLogic a))
  deriving (Show, Generic)
  deriving newtype (NFData)

data CondLogic a
  = CondExpression
      { condLogicPred :: Expression,
        condLogicBody :: a
      }
  deriving (Show, Generic, NFData)

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
  deriving (Show, Generic, NFData)

type Usage = Expression

--------------------------------------------------------------------------------
-- Type Classes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

-- TODO ∷ add <expression> : <expression> <refine>?
-- to the parser
data Expression
  = Cond (Cond Expression)
  | Constant Constant
  | Let Let
  | LetType LetType
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
  | Parened Expression
  deriving (Show, Generic, NFData)

data ArrowExp
  = Arr'
      { arrowExpLeft :: Expression,
        arrowExpUsage :: Usage,
        arrowExpRight :: Expression
      }
  deriving (Show, Generic, NFData)

data Constant
  = Number Numb
  | String String'
  deriving (Show, Generic, NFData)

data Numb
  = Integer' Integer
  | Double' Double
  deriving (Show, Generic, NFData)

newtype String'
  = Sho Text
  deriving (Show, Generic)
  deriving newtype (NFData)

newtype Block
  = Bloc
      {blockExpr :: Expression}
  deriving (Show, Generic)
  deriving newtype (NFData)

data Lambda
  = Lamb
      { lambdaArgs :: NonEmpty MatchLogic,
        lambdaBody :: Expression
      }
  deriving (Show, Generic, NFData)

data Application
  = App
      { applicationName :: Expression,
        applicationArgs :: NonEmpty Expression
      }
  deriving (Show, Generic, NFData)

newtype Do
  = Do' (NonEmpty DoBody)
  deriving (Show, Generic)
  deriving newtype (NFData)

data DoBody
  = DoBody
      { doBodyName :: Maybe NameSymb,
        doBodyExpr :: Expression
      }
  deriving (Show, Generic, NFData)

newtype ExpRecord
  = ExpressionRecord
      { expRecordFields :: NonEmpty (NameSet Expression)
      }
  deriving (Show, Generic)
  deriving newtype (NFData)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

data Let
  = Let'
      { letBindings :: FunctionLike Expression,
        letBody :: Expression
      }
  deriving (Show, Generic, NFData)

data LetType
  = LetType'
      { letTypeBindings :: Type,
        letTypeBody :: Expression
      }
  deriving (Show, Generic, NFData)

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

data Infix
  = Inf
      { infixLeft :: Expression,
        infixOp :: NameSymb,
        infixRight :: Expression
      }
  deriving (Show, Generic, NFData)

--------------------------------------------------
-- Matching
--------------------------------------------------

data Match
  = Match'
      { matchOn :: Expression,
        matchBindigns :: NonEmpty MatchL
      }
  deriving (Show, Generic, NFData)

data MatchL
  = MatchL
      { matchLPattern :: MatchLogic,
        matchLBody :: Expression
      }
  deriving (Show, Generic, NFData)

-- TODO ∷ add literals to the match
data MatchLogic
  = MatchLogic
      { matchLogicContents :: MatchLogicStart,
        matchLogicNamed :: Maybe NameSymb
      }
  deriving (Show, Generic, NFData)

data MatchLogicStart
  = MatchCon ConstructorName [MatchLogic]
  | MatchName Symbol
  | MatchConst Constant
  | MatchRecord (NonEmpty (NameSet MatchLogic))
  deriving (Show, Generic, NFData)

data NameSet t
  = Punned NameSymb
  | NonPunned NameSymb t
  deriving (Show, Generic, NFData)

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
