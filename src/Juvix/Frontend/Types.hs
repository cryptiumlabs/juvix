{-# LANGUAGE TemplateHaskell #-}

module Juvix.Frontend.Types where

import Control.Lens
import Juvix.Library hiding (Product, Sum, Type)

data TopLevel
  = Type
  | ModuleOpen
  | TypeClass
  | TypeClassInstance
  | ModuleSignature
  | Module
  | Signature
  | Function
  deriving (Show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Type
  = Typ
      { typeName ∷ !Symbol,
        typeForm ∷ TypeSum
      }
  deriving (Show)

data TypeSum
  = Alias Alias
  | -- Maybe not needed!?
    NewType NewType
  | Data Data
  deriving (Show)

-- | 'DataDeclar' is the data declaration in the Juvix language
data Data
  = Arrowed
      { dataArrow ∷ ArrowType,
        dataAdt ∷ Adt
      }
  | NonArrowed
      { dataAdt ∷ Adt
      }
  deriving (Show)

data NewType
  = Declare
      { newTypeAlias ∷ !Symbol,
        newTypeType' ∷ TypeRefine
      }
  deriving (Show)

newtype Alias
  = AliasDec
      {aliasType' ∷ TypeRefine}
  deriving (Show)

--------------------------------------------------
-- Arrows
--------------------------------------------------

data ArrowType
  = Refined TypeRefine
  | Arrows ArrowData
  | Parens ArrowType
  deriving (Show)

data ArrowData
  = Arr
    { arrowDataName ∷ !(Maybe Name),
      arrowDataRefine ∷ TypeRefine,
      arrowDataArrow ∷ !ArrowSymbol
    }
  deriving (Show)

--------------------------------------------------
-- Types Misc
--------------------------------------------------

data TypeRefine
  = TypeRefine
    { typeRefineName ∷ !TypeName,
      typeRefineRfeinement ∷ Maybe Expression
    }
  deriving (Show)

data Name
  = Implicit !Symbol
  | Concrete !Symbol
  deriving (Show)

type ArrowSymbol = Natural

-- I think we can do
-- Foo a u#b c ?
data TypeName
  = Final !Symbol
  | Next Symbol TypeName
  | Universe UniverseExpression TypeName
  deriving (Show)


data UniverseExpression
  = UniverseExpression
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
      { sumConstructor ∷ !Symbol,
        sumValue ∷ !(Maybe Product)
      }
  deriving (Show)

data Product
  = Record !Record
  | Arrow !ArrowType
  deriving (Show)

data Record
  = Record'
      { recordFields ∷ NonEmpty NameType,
        recordFamilySignature ∷ !TypeName
      }
  deriving (Show)

data NameType
  = NonErased
      { nameTypeSignature ∷ !ArrowType,
        nameTypeName ∷ !Name
      }
  deriving (Show)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = Cond Cond
  | Number Numb
  | String String'
  | Let Let
  deriving (Show)

newtype Cond
  = C (NonEmpty CondLogic)
  deriving (Show)

data CondLogic
  = CondExpression
     { condLogicPred ∷ Expression,
       condLogicExpr ∷ Expression
     }
  deriving (Show)

data Numb
  = Integer' Integer
  | Double' Double
  | ExponentD Double Integer
  | Exponent Integer Integer
  deriving (Show)

data String' = Sho deriving (Show)


data Let
  = Let'
     { letBindings ∷ NonEmpty Binding,
       letBody ∷ Expression
     }
  deriving (Show)

data Binding = Bind deriving (Show)

--------------------------------------------------------------------------------
-- Lens creation
--------------------------------------------------------------------------------

makeLensesWith camelCaseFields ''Data

makeLensesWith camelCaseFields ''Type

makeLensesWith camelCaseFields ''NewType

makeLensesWith camelCaseFields ''Sum

makeLensesWith camelCaseFields ''Record

makeLensesWith camelCaseFields ''CondLogic

makeLensesWith camelCaseFields ''Let

makePrisms ''TypeSum
