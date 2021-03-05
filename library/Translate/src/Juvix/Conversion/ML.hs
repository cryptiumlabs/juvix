{-# OPTIONS_GHC -Wno-unused-imports #-}

module Juvix.Conversion.ML
  ( op,
  )
where

-- Target ML Syntax

import qualified Data.List.NonEmpty as NonEmpty
import qualified Juvix.Desugar as Desugar
-- for local testing as development only

import qualified Juvix.Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import qualified Juvix.FrontendDesugar.RemoveDo.Types as Target
import Juvix.Library hiding (product, sum)
import qualified Juvix.Library.NameSymbol as NameSym
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

-- :defsig-match
-- type
-- declaration
-- open
op :: Sexp.T -> Target.TopLevel
op x
  | Sexp.isAtomNamed x ":type-class" = Target.TypeClass
  | Sexp.isAtomNamed x ":instance" = Target.TypeClassInstance
op (name Sexp.:> form)
  | Sexp.isAtomNamed name "declare" = Target.Declaration (declaration form)
  | Sexp.isAtomNamed name ":defsig-match" = Target.Function (defunSig form)
  | Sexp.isAtomNamed name "type" = Target.Type (type' form)
op _ = undefined

expression :: Sexp.T -> Target.Expression
expression (name Sexp.:> body)
  | Sexp.isAtomNamed name ":declaim" = Target.DeclarationE (declarationExpression body)
  | Sexp.isAtomNamed name ":infix" = Target.Infix (infix' body)
  | Sexp.isAtomNamed name "case" = Target.Match (match body)
  | Sexp.isAtomNamed name ":u" = Target.UniverseName (universeExpression body)
expression x
  | Just a <- Sexp.atomFromT x =
    atom a

atom :: Sexp.Atom -> Target.Expression
atom Sexp.A {atomName} =
  Target.Name atomName
atom Sexp.N {atomNum} =
  Target.Constant (Target.Number (Target.Integer' atomNum))

----------------------------------------------------------------------
-- Top Level Transformations
----------------------------------------------------------------------

defunSig :: Sexp.T -> Target.Function
defunSig (f Sexp.:> sig Sexp.:> forms)
  | Just fName <- eleToSymbol f,
    Just xs <- Sexp.toList forms >>= NonEmpty.nonEmpty =
    let actualSig
          | sig == Sexp.Nil = Nothing
          | otherwise =
            let (usage, sigWithoutUsage) =
                  case sig of
                    Sexp.List [useName, usage, body]
                      | Sexp.isAtomNamed useName ":usage" ->
                        (Just usage, body)
                    _ -> (Nothing, sig)
                (constraints, sigWithoutConstraints) =
                  case sigWithoutUsage of
                    Sexp.List [constArrow, constraints, body]
                      | Sexp.isAtomNamed constArrow ":=>",
                        Just xs <- Sexp.toList constraints ->
                        (xs, body)
                    _ -> ([], sigWithoutUsage)
             in Just $
                  Target.Sig
                    fName
                    (fmap expression usage)
                    (expression sigWithoutConstraints)
                    (fmap expression constraints)
     in Target.Func fName (fmap functionLike xs) actualSig
defunSig _ = error "malfromed defunSig"

----------------------------------------
-- Declarations
----------------------------------------

declaration :: Sexp.T -> Target.Declaration
declaration = Target.Infixivity . infixDeclar

infixDeclar :: Sexp.T -> Target.InfixDeclar
infixDeclar (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    if  | Sexp.isAtomNamed inf "infix" ->
          Target.NonAssoc atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixl" ->
          Target.AssocL atomName (fromInteger atomNum)
        | Sexp.isAtomNamed inf "infixr" ->
          Target.AssocR atomName (fromInteger atomNum)
        | otherwise -> error "malformed declaration"
infixDeclar _ = error "malformed declaration"

----------------------------------------
-- Types
----------------------------------------
type' :: Sexp.T -> Target.Type
type' (assocName Sexp.:> args Sexp.:> dat)
  | Just symbolList <- toSymbolList args =
    Target.Typ usage symName symbolList (adtF sig (adt dat))
  where
    Assoc {symName, usage, sig} = handleAssocTypeName assocName
    adtF = maybe Target.NonArrowed Target.Arrowed
type' _ = error "malformed type declaration"

-- TODO ∷ fix!, should check for record
adt :: Sexp.T -> Target.Adt
adt (Sexp.List [rec']) =
  Target.Product (product rec')
adt sums =
  Target.Sum (sum sums)

sum :: Sexp.T -> NonEmpty Target.Sum
sum xs = Sexp.foldr f (pure (trans (Sexp.last xs))) (Sexp.butLast xs)
  where
    trans (name Sexp.:> contents)
      | Just n <- eleToSymbol name =
        case contents of
          Sexp.Nil ->
            Target.S n Nothing
          i ->
            Target.S n (Just (product i))
    trans _ = error "malformed sum"
    f ele =
      NonEmpty.cons (trans ele)

product :: Sexp.T -> Target.Product
product (Sexp.List [f@(name Sexp.:> form)])
  | Sexp.isAtomNamed name ":record-d" || Sexp.isAtomNamed name ":" =
    Target.Record (record f)
  | Sexp.isAtomNamed name ":arrow" =
    Target.Arrow (expression form)
product normal
  | Just list <- Sexp.toList normal =
    Target.ADTLike (expression <$> list)
product _ = error "malformed product"

record :: Sexp.T -> Target.Record
record (Sexp.List [name, form, sig])
  | Sexp.isAtomNamed name ":" =
    Target.Record'' (NonEmpty.fromList (recorDHelp form)) (Just (expression sig))
record form' =
  Target.Record'' (NonEmpty.fromList (recorDHelp form)) Nothing
  where
    form = Sexp.cdr form'

recorDHelp :: Sexp.T -> [Target.NameType]
recorDHelp =
  fmap f . groupBy2
  where
    f (n, sig) = Target.NameType' (expression sig) (name n)

name :: Sexp.T -> Target.Name
name (Sexp.List [n, a])
  | Sexp.isAtomNamed n ":implicit",
    Just atom <- eleToSymbol a =
    Target.Implicit atom
name a
  | Just atom <- eleToSymbol a = Target.Concrete atom
name _ = error "malformed record-name-field"

------------------------------
-- Type' Helper
------------------------------
data AssocTypeName
  = Assoc
      { symName :: Symbol,
        usage :: Maybe Target.Expression,
        sig :: Maybe Target.Expression
      }
  deriving (Show, Eq)

-- | @handleAssocTypeName@ takes a type declaration name and gives us
-- back a direct record of it's contents transformed into the target
-- syntax
handleAssocTypeName :: Sexp.T -> AssocTypeName
handleAssocTypeName (name Sexp.:> properties)
  | Just atomName <- eleToSymbol name =
    Assoc
      { symName = atomName,
        usage = fmap expression (assoc ":usage" group),
        sig = fmap expression (assoc ":type" group)
      }
  where
    group = groupBy2 properties
handleAssocTypeName name
  | Just Sexp.A {atomName} <- Sexp.atomFromT name =
    Assoc {symName = NameSym.toSymbol atomName, usage = Nothing, sig = Nothing}
  | otherwise = error "malformed type name"

--------------------------------------------------------------------------------
-- Misc Transformations
--------------------------------------------------------------------------------
----------------------------------------
-- Matching
----------------------------------------

-----------------------------------
-- argument specific bits
-----------------------------------

arg :: Sexp.T -> Target.Arg
arg (Sexp.List [i, t])
  | Sexp.isAtomNamed i ":implicit-a" =
    Target.ImplicitA (matchLogic t)
arg t =
  Target.ConcreteA (matchLogic t)

-----------------------------------
-- specific to match expressions
-----------------------------------
match :: Sexp.T -> Target.Match
match (case' Sexp.:> t Sexp.:> binds)
  | Just xs <- Sexp.toList binds =
    Target.Match'' (expression t) (NonEmpty.fromList (fmap matchL xs))
match _ = error "malfromed match expression"

matchL :: Sexp.T -> Target.MatchL
matchL (Sexp.List [pat, body]) =
  Target.MatchL (matchLogic pat) (expression body)
matchL _ = error "malformed matchL"

---------------------------------------
-- General to all matches
---------------------------------------
matchLogic :: Sexp.T -> Target.MatchLogic
matchLogic (Sexp.List [i, n, start])
  | Sexp.isAtomNamed i ":as",
    Just s <- eleToSymbol n =
    Target.MatchLogic (matchLogicStart start) (Just s)
matchLogic t =
  Target.MatchLogic (matchLogicStart t) Nothing

matchLogicStart :: Sexp.T -> Target.MatchLogicStart
matchLogicStart x
  | Just ele <- eleToSymbol x =
    Target.MatchName ele
  | Just Sexp.N {atomNum} <- Sexp.atomFromT x =
    Target.MatchConst (Target.Number (Target.Integer' atomNum))
matchLogicStart (recordOrCon Sexp.:> rest)
  -- gotta group by 2 in this case
  | Sexp.isAtomNamed recordOrCon ":record-no-pun" =
    Target.MatchRecord (NonEmpty.fromList (fmap (nameSet matchLogic) grouped))
  -- we should have a constructor name in this instance
  | Just Sexp.A {atomName} <- Sexp.atomFromT recordOrCon,
    Just xs <- Sexp.toList rest =
    Target.MatchCon atomName (fmap matchLogic xs)
  where
    grouped = groupBy2 rest
matchLogicStart _ = error "malformed matchLogicStart"

nameSet :: (Sexp.T -> t) -> (Sexp.T, Sexp.T) -> Target.NameSet t
nameSet trans (field, assignedName)
  | Just Sexp.A {atomName} <- Sexp.atomFromT field =
    Target.NonPunned atomName (trans assignedName)
nameSet _ _ = error "bad nameSet"

--------------------------------------------------------------------------------
-- Expression Transformation
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Function Like
------------------------------------------------------------
functionLike :: Sexp.T -> Target.FunctionLike Target.Expression
functionLike (Sexp.List [args, body])
  | Just xs <- Sexp.toList args =
    Target.Like (fmap arg xs) (expression body)
functionLike _ = error "malformed like"

------------------------------------------------------------
-- Others
------------------------------------------------------------

universeExpression :: Sexp.T -> Target.UniverseExpression
universeExpression (Sexp.List [a])
  | Just name <- eleToSymbol a = Target.UniverseExpression name
universeExpression _ = error "malformed universe expression"

declarationExpression :: Sexp.T -> Target.DeclarationExpression
declarationExpression (Sexp.List [d, e]) =
  Target.DeclareExpression (declaration d) (expression e)
declarationExpression _ = error "malformed declaration"

infix' :: Sexp.T -> Target.Infix
infix' (Sexp.List [op, left, right])
  | Just Sexp.A {atomName} <- Sexp.atomFromT op =
    Target.Inf (expression left) atomName (expression right)
infix' _ = error "malformed infix"

--------------------------------------------------------------------------------
-- General Helpers
--------------------------------------------------------------------------------
eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSym.toSymbol atomName)
  | otherwise = Nothing

toSymbolList :: Sexp.T -> Maybe [Symbol]
toSymbolList x = Sexp.toList x >>= traverse eleToSymbol

--------------------------------------------------------------------------------
-- Move to Sexp library
--------------------------------------------------------------------------------

assoc :: Foldable t => NameSym.T -> t (Sexp.T, b) -> Maybe b
assoc tag =
  fmap snd . find (flip Sexp.isAtomNamed tag . fst)

groupBy2 :: Sexp.T -> [(Sexp.T, Sexp.T)]
groupBy2 (a1 Sexp.:> a2 Sexp.:> rest) =
  (a1, a2) : groupBy2 rest
groupBy2 _ = []

--------------------------------------------------------------------------------
-- Helper dev functions
--------------------------------------------------------------------------------

firstDesugar :: ByteString -> Sexp.T
firstDesugar xs =
  case desugarSexp xs of
    x : _ -> x
    _ -> error ""

desugarSexp :: ByteString -> [Sexp.T]
desugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parseOnly xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

ignoreRight :: Either a p -> p
ignoreRight (Right x) = x
ignoreRight (Left _) = error "not right"
