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
module Juvix.Sexp.Structure where

import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Juvix.Sexp.Structure.Helpers

-- | @Defun@ is the base defun structure
-- currently it does not have matching
data Defun = Defun
  { defunName :: Sexp.T,
    defunArgs :: Sexp.T,
    defunBody :: Sexp.T
  }
  deriving (Show)

-- | @Defun-match@ is a matching defun structure
data DefunMatch = DefunMatch
  { defunMatchName :: Sexp.T,
    defunMatchArgs :: [ArgBody]
  }
  deriving (Show)

-- | @ArgBody@ abstracts over the details of arguments and body
data ArgBody = ArgBody {argsBodyArgs :: Sexp.T, argsBodyBody :: Sexp.T}
  deriving (Show)

-- | @PredAns@ is an abstraction over questions and answers
data PredAns = PredAns {predAnsPredicate :: Sexp.T, predAnsAnswer :: Sexp.T}
  deriving (Show)

-- | @Cond@ here Cond form takes a list of predicate answers
newtype Cond = Cond {condEntailments :: [PredAns]} deriving (Show)

-- | @If@ has an pred, then, and else.
data If = If
  { ifPredicate :: Sexp.T,
    ifConclusion :: Sexp.T,
    ifAlternative :: Sexp.T
  }
  deriving (Show)

-- | @IfNoElse@ has a pred and a then.
data IfNoElse = IfNoElse
  { ifNoElsePredicate :: Sexp.T,
    ifNoElseConclusion :: Sexp.T
  }
  deriving (Show)

-- | @ifFull@ is the full range of If with maybe having an else
data IfFull
  = Else If
  | NoElse IfNoElse
  deriving (Show)

data DefunSigMatch = DefunSigMatch
  { defunSigMatchName :: Sexp.T,
    defunSigMatchSig :: Sexp.T,
    defunSigMatchArgs :: [ArgBody]
  }
  deriving (Show)

-- TODO ∷ have a property sexp form!?

-- | @Signature@ is the signature of the term
data Signature = Signature
  { signatureName :: Sexp.T,
    signatureSig :: Sexp.T
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

data LetMatch = LetMatch
  { letMatchName :: Sexp.T,
    -- args are ungrouped, so we have to handle that in the ouptut
    -- structure
    letMatchArgs :: [ArgBody],
    letMatchBody :: Sexp.T
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

newtype RecordNoPunned = RecordNoPunned
  { recordNoPunnedValue :: [NotPunned]
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

toIfFull :: Sexp.T -> Maybe IfFull
toIfFull sexp =
  fmap Else (toIf sexp) <|> fmap NoElse (toIfNoElse sexp)

--------------------
-- Name Bind
--------------------

toNameBind :: Sexp.T -> Maybe NameBind
toNameBind sexp =
  fmap Pun (toPunned sexp) <|> fmap NotPun (toNotPunned sexp)

fromNameBind :: NameBind -> Sexp.T
fromNameBind (Pun pun) = fromPunned pun
fromNameBind (NotPun notPun) = fromNotPunned notPun

--------------------
-- Args Bodys
--------------------

-- these are ungrouned fromArgBodys, where we groupBy2 both ways
fromArgBodys :: [ArgBody] -> Sexp.T
fromArgBodys = Sexp.unGroupBy2 . toStarList fromArgBody

-- these are ungrouned fromArgBodys, where we groupBy2 both ways
toArgBodys :: Sexp.T -> Maybe [ArgBody]
toArgBodys = fromStarList toArgBody . Sexp.groupBy2

matchConstructor :: Sexp.T -> Sexp.T
matchConstructor x = Sexp.list [x]

--------------------
-- NotPunned no Grouping
--------------------
fromNotPunnedGroup :: [NotPunned] -> Sexp.T
fromNotPunnedGroup = Sexp.unGroupBy2 . toStarList fromNotPunned

toNotPunnedGroup :: Sexp.T -> Maybe [NotPunned]
toNotPunnedGroup = fromStarList toNotPunned . Sexp.groupBy2

----------------------------------------
-- ArgBody
----------------------------------------

toArgBody :: Sexp.T -> Maybe ArgBody
toArgBody form =
  case form of
    sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
      ArgBody sexp1 sexp2 |> Just
    _ ->
      Nothing

fromArgBody :: ArgBody -> Sexp.T
fromArgBody (ArgBody sexp1 sexp2) =
  Sexp.list [sexp1, sexp2]

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
      _Defun Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        Defun sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefun :: Defun -> Sexp.T
fromDefun (Defun sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameDefun, sexp1, sexp2, sexp3]

----------------------------------------
-- DefunMatch
----------------------------------------

nameDefunMatch :: NameSymbol.T
nameDefunMatch = ":defun-match"

isDefunMatch :: Sexp.T -> Bool
isDefunMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefunMatch
isDefunMatch _ = False

toDefunMatch :: Sexp.T -> Maybe DefunMatch
toDefunMatch form
  | isDefunMatch form =
    case form of
      _DefunMatch Sexp.:> sexp1 Sexp.:> argBody2
        | Just argBody2 <- toArgBody `fromStarList` argBody2 ->
          DefunMatch sexp1 argBody2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefunMatch :: DefunMatch -> Sexp.T
fromDefunMatch (DefunMatch sexp1 argBody2) =
  Sexp.listStar [Sexp.atom nameDefunMatch, sexp1, fromArgBody `toStarList` argBody2]

----------------------------------------
-- DefunSigMatch
----------------------------------------

nameDefunSigMatch :: NameSymbol.T
nameDefunSigMatch = ":defsig-match"

isDefunSigMatch :: Sexp.T -> Bool
isDefunSigMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameDefunSigMatch
isDefunSigMatch _ = False

toDefunSigMatch :: Sexp.T -> Maybe DefunSigMatch
toDefunSigMatch form
  | isDefunSigMatch form =
    case form of
      _DefunSigMatch Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> argBody3
        | Just argBody3 <- toArgBody `fromStarList` argBody3 ->
          DefunSigMatch sexp1 sexp2 argBody3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDefunSigMatch :: DefunSigMatch -> Sexp.T
fromDefunSigMatch (DefunSigMatch sexp1 sexp2 argBody3) =
  Sexp.listStar [Sexp.atom nameDefunSigMatch, sexp1, sexp2, fromArgBody `toStarList` argBody3]

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
      _Signature Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        Signature sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromSignature :: Signature -> Sexp.T
fromSignature (Signature sexp1 sexp2) =
  Sexp.list [Sexp.atom nameSignature, sexp1, sexp2]

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
      _Let Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> sexp4 Sexp.:> Sexp.Nil ->
        Let sexp1 sexp2 sexp3 sexp4 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLet :: Let -> Sexp.T
fromLet (Let sexp1 sexp2 sexp3 sexp4) =
  Sexp.list [Sexp.atom nameLet, sexp1, sexp2, sexp3, sexp4]

----------------------------------------
-- LetMatch
----------------------------------------

nameLetMatch :: NameSymbol.T
nameLetMatch = ":let-match"

isLetMatch :: Sexp.T -> Bool
isLetMatch (Sexp.Cons form _) = Sexp.isAtomNamed form nameLetMatch
isLetMatch _ = False

toLetMatch :: Sexp.T -> Maybe LetMatch
toLetMatch form
  | isLetMatch form =
    case form of
      _LetMatch Sexp.:> sexp1 Sexp.:> argBodys2 Sexp.:> sexp3 Sexp.:> Sexp.Nil
        | Just argBodys2 <- toArgBodys argBodys2 ->
          LetMatch sexp1 argBodys2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromLetMatch :: LetMatch -> Sexp.T
fromLetMatch (LetMatch sexp1 argBodys2 sexp3) =
  Sexp.list [Sexp.atom nameLetMatch, sexp1, fromArgBodys argBodys2, sexp3]

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
      _Cond Sexp.:> predAns1
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
-- If
----------------------------------------

nameIf :: NameSymbol.T
nameIf = "if"

isIf :: Sexp.T -> Bool
isIf (Sexp.Cons form _) = Sexp.isAtomNamed form nameIf
isIf _ = False

toIf :: Sexp.T -> Maybe If
toIf form
  | isIf form =
    case form of
      _If Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 Sexp.:> Sexp.Nil ->
        If sexp1 sexp2 sexp3 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromIf :: If -> Sexp.T
fromIf (If sexp1 sexp2 sexp3) =
  Sexp.list [Sexp.atom nameIf, sexp1, sexp2, sexp3]

----------------------------------------
-- IfNoElse
----------------------------------------

nameIfNoElse :: NameSymbol.T
nameIfNoElse = "if"

isIfNoElse :: Sexp.T -> Bool
isIfNoElse (Sexp.Cons form _) = Sexp.isAtomNamed form nameIfNoElse
isIfNoElse _ = False

toIfNoElse :: Sexp.T -> Maybe IfNoElse
toIfNoElse form
  | isIfNoElse form =
    case form of
      _IfNoElse Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
        IfNoElse sexp1 sexp2 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromIfNoElse :: IfNoElse -> Sexp.T
fromIfNoElse (IfNoElse sexp1 sexp2) =
  Sexp.list [Sexp.atom nameIfNoElse, sexp1, sexp2]

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
      _Case Sexp.:> sexp1 Sexp.:> deconBody2
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
-- Do
----------------------------------------

nameDo :: NameSymbol.T
nameDo = ":do"

isDo :: Sexp.T -> Bool
isDo (Sexp.Cons form _) = Sexp.isAtomNamed form nameDo
isDo _ = False

toDo :: Sexp.T -> Maybe Do
toDo form
  | isDo form =
    case form of
      _Do Sexp.:> sexp1 ->
        Do sexp1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromDo :: Do -> Sexp.T
fromDo (Do sexp1) =
  Sexp.listStar [Sexp.atom nameDo, sexp1]

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
      _Arrow Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
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
      _Lambda Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> Sexp.Nil ->
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
      _Record Sexp.:> nameBind1
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
-- RecordNoPunned
----------------------------------------

nameRecordNoPunned :: NameSymbol.T
nameRecordNoPunned = ":record-no-pun"

isRecordNoPunned :: Sexp.T -> Bool
isRecordNoPunned (Sexp.Cons form _) = Sexp.isAtomNamed form nameRecordNoPunned
isRecordNoPunned _ = False

toRecordNoPunned :: Sexp.T -> Maybe RecordNoPunned
toRecordNoPunned form
  | isRecordNoPunned form =
    case form of
      _RecordNoPunned Sexp.:> notPunnedGroup1
        | Just notPunnedGroup1 <- toNotPunnedGroup notPunnedGroup1 ->
          RecordNoPunned notPunnedGroup1 |> Just
      _ ->
        Nothing
  | otherwise =
    Nothing

fromRecordNoPunned :: RecordNoPunned -> Sexp.T
fromRecordNoPunned (RecordNoPunned notPunnedGroup1) =
  Sexp.listStar [Sexp.atom nameRecordNoPunned, fromNotPunnedGroup notPunnedGroup1]
