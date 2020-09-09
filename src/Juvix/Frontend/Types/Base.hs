{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/cryptiumlabs/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types.Base where

import Control.Lens
import qualified Data.Data as D
import Extensible
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Product, Sum, Type)

type ConstructorName = NameSymb

type NameSymb = NonEmpty Symbol

type ModuleName = NameSymb

extensible
  [d|
    -- we will want an include... but this will have to be deferred to a context phase
    data TopLevel
      = Type Type
      | ModuleOpen ModuleOpen
      | Signature Signature
      | Module Module
      | Function Function
      | TypeClass
      | TypeClassInstance
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------------------------------------
    -- Types
    --------------------------------------------------------------------------------

    data Type
      = Typ
          -- Was a usage but can't alias for now
          { typeUsage :: Maybe Expression,
            typeName' :: !Symbol,
            typeArgs :: [Symbol],
            typeForm :: Data
          }
      deriving (Show, Generic, NFData, D.Data)

    -- | 'Data' is the data declaration in the Juvix language
    data Data
      = Arrowed
          { dataArrow :: Expression,
            dataAdt' :: Adt
          }
      | NonArrowed
          { dataAdt :: Adt
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------
    -- Arrows
    --------------------------------------------------
    data NamedType
      = NamedType'
          { nameRefineName :: !Name,
            namedRefineRefine :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- TODO ∷ change TypeName to TypeNameModule
    data TypeRefine
      = TypeRefine
          { typeRefineName :: Expression,
            typeRefineRefinement :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------
    -- Types Misc
    --------------------------------------------------

    data Name
      = Implicit !Symbol
      | Concrete !Symbol
      deriving (Show, Generic, NFData, D.Data)

    data ArrowSymbol
      = ArrowUse Usage.T
      | -- Was a usage but can't alias for now
        ArrowExp Expression
      deriving (Show, Generic, NFData, D.Data)

    -- Was a new type
    -- TODO ∷ finish this type!
    data UniverseExpression
      = UniverseExpression Symbol
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------
    -- ADTs
    --------------------------------------------------

    -- The types for ADT are not the most
    -- constraining, however are setup to have the
    -- least amount of boilerplate in the latter
    -- stages as possible

    -- a Sum of length one should not have a record
    -- [type Address = Foo {abc : Int}]
    -- this form should be considered illegal unless we wish to permit
    -- named records along with unnamed records.
    -- Ι suspect in the future this will instead be used for Enum
    -- Subsets with refined information
    data Adt
      = Sum (NonEmpty Sum)
      | Product Product
      deriving (Show, Generic, NFData, D.Data)

    data Sum
      = S
          { sumConstructor :: !Symbol,
            sumValue :: !(Maybe Product)
          }
      deriving (Show, Generic, NFData, D.Data)

    -- for when a product is without a sum
    -- only a record can apply
    -- a sum of only one is a named product
    data Product
      = Record !Record
      | Arrow Expression
      | ADTLike [Expression]
      deriving (Show, Generic, NFData, D.Data)

    data Record
      = Record''
          { recordFields :: NonEmpty NameType,
            recordFamilySignature :: Maybe Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    data NameType
      = NameType'
          { nameTypeSignature :: Expression,
            nameTypeName :: !Name
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------------------------------------
    -- Functions And Modules
    --------------------------------------------------------------------------------

    -- was a newtype

    -- | 'Function' is a normal signature with a name arguments and a body
    -- that may or may not have a guard before it
    data Function
      = Func (FunctionLike Expression)
      deriving (Show, Generic, NFData, D.Data)

    -- | 'Module' is like function, however it allows multiple top levels
    data Module
      = Mod (FunctionLike (NonEmpty TopLevel))
      deriving (Show, Generic, NFData, D.Data)

    data ModuleE
      = ModE
          { moduleEBindings :: FunctionLike (NonEmpty TopLevel),
            moduleEBody :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- | 'FunctionLike' is the generic version for both modules and functions
    data FunctionLike a
      = Like
          { functionLikedName :: Symbol,
            functionLikeArgs :: [Arg],
            functionLikeBody :: GuardBody a
          }
      deriving (Show, Generic, NFData, D.Data)

    -- | 'GuardBody' determines if a form is a guard or a body
    data GuardBody a
      = Body a
      | Guard (Cond a)
      deriving (Show, Generic, NFData, D.Data)

    data ModuleOpen
      = Open ModuleName
      deriving (Show, Generic, NFData, D.Data)

    data ModuleOpenExpr
      = OpenExpress
          { moduleOpenExprModuleN :: ModuleName,
            moduleOpenExprExpr :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- Very similar to name, but match instead of symbol
    data Arg
      = ImplicitA MatchLogic
      | ConcreteA MatchLogic
      deriving (Show, Generic, NFData, D.Data)

    data Cond a
      = C (NonEmpty (CondLogic a))
      deriving (Show, Generic, NFData, D.Data)

    data CondLogic a
      = CondExpression
          { condLogicPred :: Expression,
            condLogicBody :: a
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------------------------------------
    -- Signatures
    --------------------------------------------------------------------------------

    data Signature
      = Sig
          { signatureName :: Symbol,
            -- Was a usage but can't alias for now
            signatureUsage :: Maybe Expression,
            signatureArrowType :: Expression,
            signatureConstraints :: [Expression]
          }
      deriving (Show, Generic, NFData, D.Data)

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
      | ModuleE ModuleE
      | LetType LetType
      | Match Match
      | Name NameSymb
      | OpenExpr ModuleOpenExpr
      | Lambda Lambda
      | Application Application
      | Primitive Primitive
      | List List
      | Tuple Tuple
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
      deriving (Show, Generic, NFData, D.Data)

    data Primitive
      = Prim NameSymb
      deriving (Show, Generic, NFData, D.Data)

    data List
      = ListLit [Expression]
      deriving (Show, Generic, NFData, D.Data)

    data Tuple
      = TupleLit [Expression]
      deriving (Show, Generic, NFData, D.Data)

    data ArrowExp
      = Arr'
          { arrowExpLeft :: Expression,
            -- Was a usage but can't alias for now
            arrowExpUsage :: Expression,
            arrowExpRight :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    data Constant
      = Number Numb
      | String String'
      deriving (Show, Generic, NFData, D.Data)

    data Numb
      = Integer' Integer
      | Double' Double
      deriving (Show, Generic, NFData, D.Data)

    data String'
      = Sho Text
      deriving (Show, Generic, NFData, D.Data)

    data Block
      = Bloc
          {blockExpr :: Expression}
      deriving (Show, Generic, NFData, D.Data)

    data Lambda
      = Lamb
          { lambdaArgs :: NonEmpty MatchLogic,
            lambdaBody :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    data Application
      = App
          { applicationName :: Expression,
            applicationArgs :: NonEmpty Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- Was a newtype but extensible adds fields
    data Do
      = Do'' (NonEmpty DoBody)
      deriving (Show, Generic, NFData, D.Data)

    -- promote this to a match!!!
    data DoBody
      = DoBody
          { doBodyName :: Maybe Symbol,
            doBodyExpr :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- TODO ∷ we need includes in here as well!
    -- Was a newtype but extensible adds fields
    data ExpRecord
      = ExpressionRecord
          { expRecordFields :: NonEmpty (NameSet Expression)
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------
    -- Symbol Binding
    --------------------------------------------------

    data Let
      = Let''
          { letBindings :: FunctionLike Expression,
            letBody :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    data LetType
      = LetType''
          { letTypeBindings :: Type,
            letTypeBody :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- TODO ∷ have letSig

    --------------------------------------------------
    -- Symbol Binding
    --------------------------------------------------

    data Infix
      = Inf
          { infixLeft :: Expression,
            infixOp :: NameSymb,
            infixRight :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    --------------------------------------------------
    -- Matching
    --------------------------------------------------

    data Match
      = Match''
          { matchOn :: Expression,
            matchBindigns :: NonEmpty MatchL
          }
      deriving (Show, Generic, NFData, D.Data)

    data MatchL
      = MatchL
          { matchLPattern :: MatchLogic,
            matchLBody :: Expression
          }
      deriving (Show, Generic, NFData, D.Data)

    -- TODO ∷ add literals to the match
    data MatchLogic
      = MatchLogic
          { matchLogicContents :: MatchLogicStart,
            matchLogicNamed :: Maybe Symbol
          }
      deriving (Show, Generic, NFData, D.Data)

    data MatchLogicStart
      = MatchCon ConstructorName [MatchLogic]
      | MatchName Symbol
      | MatchConst Constant
      | MatchRecord (NonEmpty (NameSet MatchLogic))
      deriving (Show, Generic, NFData, D.Data)

    data NameSet t
      = Punned NameSymb
      | NonPunned NameSymb t
      deriving (Show, Generic, NFData, D.Data)
    |]

--------------------------------------------------------------------------------
-- Lens creation
--------------------------------------------------------------------------------

makeLensesWith camelCaseFields ''Data'

makeLensesWith camelCaseFields ''Type'

makeLensesWith camelCaseFields ''Sum'

makeLensesWith camelCaseFields ''Record'

makeLensesWith camelCaseFields ''NamedType'

makeLensesWith camelCaseFields ''TypeRefine'

makeLensesWith camelCaseFields ''CondLogic'

makeLensesWith camelCaseFields ''Let'

makeLensesWith camelCaseFields ''Match'

makeLensesWith camelCaseFields ''MatchL'

makeLensesWith camelCaseFields ''MatchLogic'

makeLensesWith camelCaseFields ''FunctionLike'

makeLensesWith camelCaseFields ''Module'

makeLensesWith camelCaseFields ''Function'

makeLensesWith camelCaseFields ''Lambda'

makeLensesWith camelCaseFields ''Application'

makeLensesWith camelCaseFields ''Signature'

makeLensesWith camelCaseFields ''Block'

makeLensesWith camelCaseFields ''Do'

makeLensesWith camelCaseFields ''DoBody'

makeLensesWith camelCaseFields ''ModuleOpenExpr'

makeLensesWith camelCaseFields ''Infix'

makeLensesWith camelCaseFields ''ExpRecord'
