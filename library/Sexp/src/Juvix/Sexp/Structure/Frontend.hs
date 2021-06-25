{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Hardeing S-expressions into a more readable form. Here we use a
-- mixture of record structures and aliases. Each cover a form that we
-- wish to talk about rather than just match away at
--
-- - _The form for transformation follows this structure_
-- #+begin_src haskell
--   -- the data type
--   data Form = ... deriving (Show)
--   is<Form>   :: Sexp.T -> Bool
--   to<Form>   :: Sexp.T -> Maybe <Form>
--   from<Form> :: <Form> -> Sexp.T
-- #+end_src
-- + With the following properties of the forms
--   #+begin_src haskell
--     ∀ s : Sexp.T. is<Form> s = True ⟷ is-just (to<Form> s)
--
--     to<Form> 。 from<Form> = Just
--   #+end_src
-- _TODO_
--  1. Figure out if we can even express a spec system in
--     Haskell... =to<Form>= and =From<From>= have the exact same signature
--  2. replace the repeat code with the =to<Form>= with an abstraction
--  3. put the meta data with the form so we don't have to do it by
--     hand in the code that uses this
--     1. Use =Juvix.Library.LineNum=
--     2. append the =Form= With this
--     3. have to<Form> fill this
--     4. Have extra smart consturctors that are =<form>=, so that we
--        can automatically fill in this meta data
module Juvix.Sexp.Structure.Frontend where

import Juvix.Library hiding (Type)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure.Helpers

-- | @Defun@ is the base defun structure
-- currently it does not have matching
data Defun = Defun
  { defunName :: Sexp.T,
    defunArgs :: Sexp.T,
    defunBody :: Sexp.T
  }
  deriving (Show)

-- | @Type@ is the type declaration structure
data Type = Type
  { -- TODO ∷ we should really have a field the signature currently we
    -- don't really support that without doing it by hand. Maybe our
    -- generator should help here, or we should have a way of talking
    -- about named arguments somehow, and unfolding the slots in a
    -- way....
    typeNameAndSig :: Sexp.T,
    typeArgs :: Sexp.T,
    typeBody :: Sexp.T
  }
  deriving (Show)

-- | @PredAns@ is an abstraction over questions and answers
data PredAns = PredAns {predAnsPredicate :: Sexp.T, predAnsAnswer :: Sexp.T}
  deriving (Show)

-- | @Cond@ here Cond form takes a list of predicate answers
newtype Cond = Cond {condEntailments :: [PredAns]} deriving (Show)

-- | @Signature@ is the signature of the term
data Signature = Signature
  { signatureName :: Sexp.T,
    signatureSig :: Sexp.T
  }
  deriving (Show)

data LetSignature = LetSignature
  { letSignatureName :: Sexp.T,
    letSignatureSig :: Sexp.T,
    letSignatureRest :: Sexp.T
  }
  deriving (Show)

-- | @LetType@ is the let-type form of the language
data LetType = LetType
  { letTypeNameAndSig :: Sexp.T,
    letTypeArgs :: Sexp.T,
    letTypeBody :: Sexp.T,
    letTypeRest :: Sexp.T
  }
  deriving (Show)

-- | @Let@ is the let form of the language
-- it has a name, arguments, body, and the body
data Let = Let
  { letName :: Sexp.T,
    letArgs :: Sexp.T,
    letBody :: Sexp.T,
    letRest :: Sexp.T
  }
  deriving (Show)

data Case = Case
  { caseOn :: Sexp.T,
    caseImplications :: [DeconBody]
  }
  deriving (Show)

-- | @DeconBody@ is an abstraction over a matching body form
data DeconBody = DeconBody
  { deconBodyDeconsturctor :: Sexp.T,
    deconBodyBody :: Sexp.T
  }
  deriving (Show)

newtype Do = Do
  { doStatements :: Sexp.T
  }
  deriving (Show)

data Arrow = Arrow
  { arrowName :: Sexp.T,
    arrowBody :: Sexp.T
  }
  deriving (Show)

data Lambda = Lambda
  { lambdaArgs :: Sexp.T,
    lambdabody :: Sexp.T
  }
  deriving (Show)

-- | @NameBind@ represents a type naming scheme in a record
data NameBind = Pun Punned | NotPun NotPunned
  deriving (Show)

-- | @NotPunned@ represents a punned type name in a record
data NotPunned = NotPunned
  { notPunnedName :: Sexp.T,
    notPunnedValue :: Sexp.T
  }
  deriving (Show)

newtype Punned = Punned
  { punnedName :: Sexp.T
  }
  deriving (Show)

newtype Record = Record
  { recordValue :: [NameBind]
  }
  deriving (Show)

-- | @Infix@ represents an infix function
data Infix = Infix
  { infixOp :: Sexp.T,
    infixLeft :: Sexp.T,
    infixRight :: Sexp.T
  }
  deriving (Show)

newtype Open = Open
  { openName :: Sexp.T
  }
  deriving (Show)

data OpenIn = OpenIn
  { openInName :: Sexp.T,
    openInBody :: Sexp.T
  }
  deriving (Show)

newtype Declare = Declare
  { declareClaim :: Sexp.T
  }
  deriving (Show)

data Declaim = Declaim
  { declaimClaim :: Sexp.T,
    declaimBody :: Sexp.T
  }
  deriving (Show)

-- | @DefModule@ - Stands in for a module declaration
data DefModule = DefModule
  { defModuleName :: Sexp.T,
    defModuleArgs :: Sexp.T,
    defModuleBody :: Sexp.T
  }
  deriving (Show)

-- | @LefModule@ - Stands in for a module let declaration
data LetModule = LetModule
  { letModuleName :: Sexp.T,
    letModuleArgs :: Sexp.T,
    letModuleBody :: Sexp.T,
    letModuleRest :: Sexp.T
  }
  deriving (Show)

data Effect = Effect
  { effectName :: Sexp.T,
    effectOps :: Sexp.T
  }
  deriving (Show)

data DefHandler = DefHandler
  { defHandlerName :: Sexp.T,
    defHandlerOps :: Sexp.T
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Converter functions
-- The format for these are
-- name<Form> :: NameSymbol.T
-- is<Form>   :: Sexp.T -> Bool
-- to<Form>   :: Sexp.T -> Maybe <Form>
-- from<Form> :: <Form> -> Sexp.T
--------------------------------------------------------------------------------

-------------------------------------------
-- Not Generated, due to limited generation
-------------------------------------------

--------------------
-- Name Bind
--------------------

toNameBind :: Sexp.T -> Maybe NameBind
toNameBind sexp =
  fmap Pun (toPunned sexp) <|> fmap NotPun (toNotPunned sexp)

fromNameBind :: NameBind -> Sexp.T
fromNameBind (Pun pun) = fromPunned pun
fromNameBind (NotPun notPun) = fromNotPunned notPun

----------------------------------------
-- Generated
----------------------------------------

----------------------------------------
-- Type
----------------------------------------

nameType :: NameSymbol.T
nameType = "type"

isType :: Sexp.T -> Bool
isType (Sexp.Cons form _) = Sexp.isAtomNamed form nameType
isType _ = False

toType :: Sexp.T -> Maybe Type
toType form
  | isType form =
    case form of
      _nameType Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 ->
        Type sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromType :: Type -> Sexp.T
fromType (Type sexp1 sexp2 sexp3) =
  Sexp.listStar [Sexp.atom nameType, sexp1, sexp2, sexp3]

----------------------------------------
-- LetType
----------------------------------------

nameLetType :: NameSymbol.T
nameLetType = ":let-type"

isLetType :: Sexp.T -> Bool
isLetType (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetType
isLetType _ = False

toLetType :: Sexp.T -> Maybe LetType
toLetType form
  | isLetType form =
    case form of
      _nameLetType Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        LetType sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetType :: LetType -> Sexp.T
fromLetType (LetType sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLetType, sexp1, sexp2, sexp3, sexp4]

----------------------------------------
-- Defun
----------------------------------------

nameDefun :: NameSymbol.T
nameDefun = ":defun"

isDefun :: Sexp.T -> Bool
isDefun (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefun
isDefun _ = False

toDefun :: Sexp.T -> Maybe Defun
toDefun form
  | isDefun form =
    case form of
      _nameDefun Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        Defun sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefun :: Defun -> Sexp.T
fromDefun (Defun sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameDefun, sexp1, sexp2, sexp3]

----------------------------------------
-- Signature
----------------------------------------

nameSignature :: NameSymbol.T
nameSignature = ":defsig"

isSignature :: Sexp.T -> Bool
isSignature (Sexp.Cons form _) = Sexp.isAtomNamed form nameSignature
isSignature _ = False

toSignature :: Sexp.T -> Maybe Signature
toSignature form
  | isSignature form =
    case form of
      _nameSignature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Signature sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSignature :: Signature -> Sexp.T
fromSignature (Signature sexp1 sexp2) =
  Sexp.list [Sexp.atom nameSignature, sexp1, sexp2]

----------------------------------------
-- LetSignature
----------------------------------------

nameLetSignature :: NameSymbol.T
nameLetSignature = ":let-sig"

isLetSignature :: Sexp.T -> Bool
isLetSignature (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetSignature
isLetSignature _ = False

toLetSignature :: Sexp.T -> Maybe LetSignature
toLetSignature form
  | isLetSignature form =
    case form of
      _nameLetSignature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        LetSignature sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetSignature :: LetSignature -> Sexp.T
fromLetSignature (LetSignature sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameLetSignature, sexp1, sexp2, sexp3]

----------------------------------------
-- Let
----------------------------------------

nameLet :: NameSymbol.T
nameLet = "let"

isLet :: Sexp.T -> Bool
isLet (Sexp.Cons form _) = Sexp.isAtomNamed form nameLet
isLet _ = False

toLet :: Sexp.T -> Maybe Let
toLet form
  | isLet form =
    case form of
      _nameLet Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        Let sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLet :: Let -> Sexp.T
fromLet (Let sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLet, sexp1, sexp2, sexp3, sexp4]

----------------------------------------
-- PredAns
----------------------------------------

toPredAns :: Sexp.T -> Maybe PredAns
toPredAns form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      PredAns sexp1 sexp2 |> Just
    _ ->
      Nothing

fromPredAns :: PredAns -> Sexp.T
fromPredAns (PredAns sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

----------------------------------------
-- Cond
----------------------------------------

nameCond :: NameSymbol.T
nameCond = ":cond"

isCond :: Sexp.T -> Bool
isCond (Sexp.Cons form _) = Sexp.isAtomNamed form nameCond
isCond _ = False

toCond :: Sexp.T -> Maybe Cond
toCond form
  | isCond form =
    case form of
      _nameCond Sexp.:> predAns1
        | Just predAns1 <- toPredAns `fromStarList` predAns1 ->
          Cond predAns1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCond :: Cond -> Sexp.T
fromCond (Cond predAns1) =
  Sexp.listStar [Sexp.atom nameCond, fromPredAns `toStarList` predAns1]

----------------------------------------
-- DeconBody
----------------------------------------

toDeconBody :: Sexp.T -> Maybe DeconBody
toDeconBody form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      DeconBody sexp1 sexp2 |> Just
    _ ->
      Nothing

fromDeconBody :: DeconBody -> Sexp.T
fromDeconBody (DeconBody sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

----------------------------------------
-- Case
----------------------------------------

nameCase :: NameSymbol.T
nameCase = "case"

isCase :: Sexp.T -> Bool
isCase (Sexp.Cons form _) = Sexp.isAtomNamed form nameCase
isCase _ = False

toCase :: Sexp.T -> Maybe Case
toCase form
  | isCase form =
    case form of
      _nameCase Sexp.:> sexp1 Sexp.:> deconBody2
        | Just deconBody2 <- toDeconBody `fromStarList` deconBody2 ->
          Case sexp1 deconBody2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromCase :: Case -> Sexp.T
fromCase (Case sexp1 deconBody2) =
  Sexp.listStar [Sexp.atom nameCase, sexp1, fromDeconBody `toStarList` deconBody2]

----------------------------------------
-- Arrow
----------------------------------------

nameArrow :: NameSymbol.T
nameArrow = "%<-"

isArrow :: Sexp.T -> Bool
isArrow (Sexp.Cons form _) = Sexp.isAtomNamed form nameArrow
isArrow _ = False

toArrow :: Sexp.T -> Maybe Arrow
toArrow form
  | isArrow form =
    case form of
      _nameArrow Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Arrow sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromArrow :: Arrow -> Sexp.T
fromArrow (Arrow sexp1 sexp2) =
  Sexp.list [Sexp.atom nameArrow, sexp1, sexp2]

----------------------------------------
-- Lambda
----------------------------------------

nameLambda :: NameSymbol.T
nameLambda = "lambda"

isLambda :: Sexp.T -> Bool
isLambda (Sexp.Cons form _) = Sexp.isAtomNamed form nameLambda
isLambda _ = False

toLambda :: Sexp.T -> Maybe Lambda
toLambda form
  | isLambda form =
    case form of
      _nameLambda Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Lambda sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLambda :: Lambda -> Sexp.T
fromLambda (Lambda sexp1 sexp2) =
  Sexp.list [Sexp.atom nameLambda, sexp1, sexp2]

----------------------------------------
-- Punned
----------------------------------------

toPunned :: Sexp.T -> Maybe Punned
toPunned form =
  case form of
    sexp1 Sexp.:> Sexp.Nil ->
      Punned sexp1 |> Just
    _ ->
      Nothing

fromPunned :: Punned -> Sexp.T
fromPunned (Punned sexp1) =
  Sexp.list [sexp1]

----------------------------------------
-- NotPunned
----------------------------------------

toNotPunned :: Sexp.T -> Maybe NotPunned
toNotPunned form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      NotPunned sexp1 sexp2 |> Just
    _ ->
      Nothing

fromNotPunned :: NotPunned -> Sexp.T
fromNotPunned (NotPunned sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

----------------------------------------
-- Record
----------------------------------------

nameRecord :: NameSymbol.T
nameRecord = ":record"

isRecord :: Sexp.T -> Bool
isRecord (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecord
isRecord _ = False

toRecord :: Sexp.T -> Maybe Record
toRecord form
  | isRecord form =
    case form of
      _nameRecord Sexp.:> nameBind1
        | Just nameBind1 <- toNameBind `fromStarList` nameBind1 ->
          Record nameBind1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecord :: Record -> Sexp.T
fromRecord (Record nameBind1) =
  Sexp.listStar [Sexp.atom nameRecord, fromNameBind `toStarList` nameBind1]

----------------------------------------
-- Infix
----------------------------------------

nameInfix :: NameSymbol.T
nameInfix = ":infix"

isInfix :: Sexp.T -> Bool
isInfix (Sexp.Cons form _) = Sexp.isAtomNamed form nameInfix
isInfix _ = False

toInfix :: Sexp.T -> Maybe Infix
toInfix form
  | isInfix form =
    case form of
      _nameInfix Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        Infix sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromInfix :: Infix -> Sexp.T
fromInfix (Infix sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameInfix, sexp1, sexp2, sexp3]

----------------------------------------
-- OpenIn
----------------------------------------

nameOpenIn :: NameSymbol.T
nameOpenIn = ":open-in"

isOpenIn :: Sexp.T -> Bool
isOpenIn (Sexp.Cons form _) = Sexp.isAtomNamed form nameOpenIn
isOpenIn _ = False

toOpenIn :: Sexp.T -> Maybe OpenIn
toOpenIn form
  | isOpenIn form =
    case form of
      _nameOpenIn Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        OpenIn sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromOpenIn :: OpenIn -> Sexp.T
fromOpenIn (OpenIn sexp1 sexp2) =
  Sexp.list [Sexp.atom nameOpenIn, sexp1, sexp2]

----------------------------------------
-- Open
----------------------------------------

nameOpen :: NameSymbol.T
nameOpen = "open"

isOpen :: Sexp.T -> Bool
isOpen (Sexp.Cons form _) = Sexp.isAtomNamed form nameOpen
isOpen _ = False

toOpen :: Sexp.T -> Maybe Open
toOpen form
  | isOpen form =
    case form of
      _nameOpen Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Open sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromOpen :: Open -> Sexp.T
fromOpen (Open sexp1) =
  Sexp.list [Sexp.atom nameOpen, sexp1]

----------------------------------------
-- Declare
----------------------------------------

nameDeclare :: NameSymbol.T
nameDeclare = "declare"

isDeclare :: Sexp.T -> Bool
isDeclare (Sexp.Cons form _) = Sexp.isAtomNamed form nameDeclare
isDeclare _ = False

toDeclare :: Sexp.T -> Maybe Declare
toDeclare form
  | isDeclare form =
    case form of
      _nameDeclare Sexp.:> sexp1 Sexp.:> Sexp.Nil ->
        Declare sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDeclare :: Declare -> Sexp.T
fromDeclare (Declare sexp1) =
  Sexp.list [Sexp.atom nameDeclare, sexp1]

----------------------------------------
-- Declaim
----------------------------------------

nameDeclaim :: NameSymbol.T
nameDeclaim = ":declaim"

isDeclaim :: Sexp.T -> Bool
isDeclaim (Sexp.Cons form _) = Sexp.isAtomNamed form nameDeclaim
isDeclaim _ = False

toDeclaim :: Sexp.T -> Maybe Declaim
toDeclaim form
  | isDeclaim form =
    case form of
      _nameDeclaim Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Declaim sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDeclaim :: Declaim -> Sexp.T
fromDeclaim (Declaim sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDeclaim, sexp1, sexp2]

----------------------------------------
-- DefModule
----------------------------------------

nameDefModule :: NameSymbol.T
nameDefModule = ":defmodule"

isDefModule :: Sexp.T -> Bool
isDefModule (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefModule
isDefModule _ = False

toDefModule :: Sexp.T -> Maybe DefModule
toDefModule form
  | isDefModule form =
    case form of
      _nameDefModule Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 ->
        DefModule sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefModule :: DefModule -> Sexp.T
fromDefModule (DefModule sexp1 sexp2 sexp3) =
  Sexp.listStar [Sexp.atom nameDefModule, sexp1, sexp2, sexp3]

----------------------------------------
-- LetModule
----------------------------------------

nameLetModule :: NameSymbol.T
nameLetModule = ":let-mod"

isLetModule :: Sexp.T -> Bool
isLetModule (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetModule
isLetModule _ = False

toLetModule :: Sexp.T -> Maybe LetModule
toLetModule form
  | isLetModule form =
    case form of
      _nameLetModule Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        LetModule sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetModule :: LetModule -> Sexp.T
fromLetModule (LetModule sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLetModule, sexp1, sexp2, sexp3, sexp4]

----------------------------------------
-- Effect
----------------------------------------

nameEffect :: NameSymbol.T
nameEffect = ":defeff"

isEffect :: Sexp.T -> Bool
isEffect (Sexp.Cons form _) = Sexp.isAtomNamed form nameEffect
isEffect _ = False

toEffect :: Sexp.T -> Maybe Effect
toEffect form
  | isEffect form =
    case form of
      _nameEffect Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Effect sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromEffect :: Effect -> Sexp.T
fromEffect (Effect sexp1 sexp2) =
  Sexp.list [Sexp.atom nameEffect, sexp1, sexp2]

----------------------------------------
-- DefHandler
----------------------------------------

nameDefHandler :: NameSymbol.T
nameDefHandler = ":defHandler"

isDefHandler :: Sexp.T -> Bool
isDefHandler (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefHandler
isDefHandler _ = False

toDefHandler :: Sexp.T -> Maybe DefHandler
toDefHandler form
  | isDefHandler form =
    case form of
      _nameDefHandler Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        DefHandler sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefHandler :: DefHandler -> Sexp.T
fromDefHandler (DefHandler sexp1 sexp2) =
  Sexp.list [Sexp.atom nameDefHandler, sexp1, sexp2]
