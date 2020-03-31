module Frontend where

import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Parser as Parser
import Juvix.Library hiding (show)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, show)

allParserTests :: T.TestTree
allParserTests =
  T.testGroup
    "Parser Tests"
    [ many1FunctionsParser,
      sigTest1,
      sigTest2,
      fun1,
      fun2,
      sumTypeTest,
      superArrowCase,
      typeTest,
      moduleOpen,
      moduleOpen',
      typeNameNoUniverse,
      simpleNamedCon,
      matchMoreComplex,
      condTest1,
      record1,
      parens1
    ]

--------------------------------------------------------------------------------
-- Parser Checker
--------------------------------------------------------------------------------

parseTasty ::
  (Show a1, Show a2, Eq a1) => T.TestName -> Either a1 a2 -> String -> T.TestTree
parseTasty name x y =
  T.testGroup
    "parse Test"
    [ T.testCase (name <> " parser failed") (isRight x T.@=? True),
      T.testCase
        ("parse: " <> name <> " " <> show x <> " should parse to " <> y)
        (fmap show x T.@=? Right y)
    ]

shouldParseAs ::
  Show a => T.TestName -> Parser a -> ByteString -> String -> T.TestTree
shouldParseAs name parser parseString =
  parseTasty name (parseOnly parser parseString)

--------------------------------------------------------------------------------
-- Parse Many at once
--------------------------------------------------------------------------------

many1FunctionsParser :: T.TestTree
many1FunctionsParser =
  shouldParseAs
    "many1FunctionsParser"
    (many Parser.topLevelSN)
    ( "let foo a b c = (+) (a + b) c\n"
        <> "let bah = foo 1 2 3\n"
        <> "let nah \n"
        <> "  | bah == 5 = 7 \n"
        <> "  | else     = 11"
        <> "let test = \n"
        <> "  let check = nah in \n"
        <> "  case check of \n"
        <> "  | seven -> 11 \n"
        <> "  | eleven -> 7 \n"
        <> "  | f  -> open Fails in \n"
        <> "          print failed; \n"
        <> "          fail"
    )
    "[Function (Func (Like {functionLikedName = foo, functionLikeArgs = [ConcreteA \
    \(MatchLogic {matchLogicContents = MatchName a, matchLogicNamed = Nothing}),ConcreteA \
    \(MatchLogic {matchLogicContents = MatchName b, matchLogicNamed = Nothing}),ConcreteA \
    \(MatchLogic {matchLogicContents = MatchName c, matchLogicNamed = Nothing})], functionLikeBody \
    \= Body (Application (App {applicationName = + :| [], applicationArgs = Infix \
    \(Inf {infixLeft = Name (a :| []), infixOp = + :| [], infixRight = Name (b :| [])}) :| [Name \
    \(c :| [])]}))})),Function (Func (Like {functionLikedName = bah, functionLikeArgs \
    \= [], functionLikeBody = Body (Application (App {applicationName = foo :| [], applicationArgs \
    \= Constant (Number (Integer' 1)) :| [Constant (Number (Integer' 2)),Constant \
    \(Number (Integer' 3))]}))})),Function (Func (Like {functionLikedName = nah, functionLikeArgs \
    \= [], functionLikeBody = Guard (C (CondExpression {condLogicPred = Infix (Inf \
    \{infixLeft = Name (bah :| []), infixOp = == :| [], infixRight = Constant (Number \
    \(Integer' 5))}), condLogicBody = Constant (Number (Integer' 7))} :| [CondExpression \
    \{condLogicPred = Name (else :| []), condLogicBody = Constant (Number (Integer' \
    \11))}]))})),Function (Func (Like {functionLikedName = test, functionLikeArgs \
    \= [], functionLikeBody = Body (Let (Let' {letBindings = Bind {bindingPattern \
    \= MatchLogic {matchLogicContents = MatchName check, matchLogicNamed = Nothing}, \
    \bindingBody = Name (nah :| [])} :| [], letBody = Match (Match' {matchOn = Name \
    \(check :| []), matchBindigns = MatchL {matchLPattern = MatchLogic {matchLogicContents \
    \= MatchName seven, matchLogicNamed = Nothing}, matchLBody = Constant (Number \
    \(Integer' 11))} :| [MatchL {matchLPattern = MatchLogic {matchLogicContents = MatchName \
    \eleven, matchLogicNamed = Nothing}, matchLBody = Constant (Number (Integer' 7))},MatchL \
    \{matchLPattern = MatchLogic {matchLogicContents = MatchName f, matchLogicNamed \
    \= Nothing}, matchLBody = OpenExpr (OpenExpress {moduleOpenExprModuleN = Fails \
    \:| [], moduleOpenExprExpr = Application (App {applicationName = print :| [], applicationArgs \
    \= Do (Do' (DoBody {doBodyName = Nothing, doBodyExpr = Name (failed :| [])} :| [DoBody \
    \{doBodyName = Nothing, doBodyExpr = Name (fail :| [])}])) :| []})})}]})}))}))]"

--------------------------------------------------------------------------------
-- Sig Test
--------------------------------------------------------------------------------

sigTest1 :: T.TestTree
sigTest1 =
  shouldParseAs
    "sigTest1"
    Parser.topLevel
    "sig foo 0 : Int -> Int"
    "Signature (Sig {signatureName = foo, signatureUsage = Just (Constant (Number \
    \(Integer' 0))), signatureArrowType = Arrows (Arr (ArrGen {arrowGenName = Nothing, \
    \arrowGenData = NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine \
    \{typeRefineName = Start (Int :| []) [], typeRefineRefinement = Nothing}}, arrowGenArrow \
    \= ArrowUse w})) (Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Int :| []) [], typeRefineRefinement = Nothing}})), \
    \signatureConstraints = []})"

sigTest2 :: T.TestTree
sigTest2 =
  shouldParseAs
    "sigTest2"
    Parser.topLevel
    "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"
    "Signature (Sig {signatureName = foo, signatureUsage = Just (Constant (Number \
    \(Integer' 0))), signatureArrowType = Arrows (Arr (ArrGen {arrowGenName = Just (Concrete \
    \i), arrowGenData = NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine \
    \{typeRefineName = Start (Int :| []) [], typeRefineRefinement = Just (Infix (Inf {infixLeft \
    \= Name (i :| []), infixOp = > :| [], infixRight = Constant (Number (Integer' 0))}))}}, \
    \arrowGenArrow = ArrowUse w})) (Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Int :| []) [], typeRefineRefinement = Just \
    \(Infix (Inf {infixLeft = Name (i :| []), infixOp = > :| [], infixRight = Constant \
    \(Number (Integer' 1))}))}})), signatureConstraints = []})"

--------------------------------------------------------------------------------
-- Function Testing
--------------------------------------------------------------------------------

fun1 :: T.TestTree
fun1 =
  shouldParseAs
    "fun1"
    Parser.topLevel
    "let f foo@(A b c d) = 3"
    "Function (Func (Like {functionLikedName = f, functionLikeArgs = \
    \[ConcreteA (MatchLogic {matchLogicContents = MatchCon (A :| []) \
    \[MatchLogic {matchLogicContents = MatchName b, matchLogicNamed = Nothing},\
    \MatchLogic {matchLogicContents = MatchName c, matchLogicNamed = Nothing},\
    \MatchLogic {matchLogicContents = MatchName d, matchLogicNamed = Nothing}], \
    \matchLogicNamed = Just (foo :| [])})], \
    \functionLikeBody = Body (Constant (Number (Integer' 3)))}))"

fun2 :: T.TestTree
fun2 =
  shouldParseAs
    "fun2"
    Parser.topLevel
    "let f foo | foo = 2 | else = 3"
    "Function (Func (Like {functionLikedName = f, \
    \functionLikeArgs = \
    \[ConcreteA (MatchLogic \
    \{matchLogicContents = MatchName foo, matchLogicNamed = Nothing})], \
    \functionLikeBody = Guard (C (CondExpression {condLogicPred = Name (foo :| []), \
    \condLogicBody = Constant (Number (Integer' 2))} :| \
    \[CondExpression {condLogicPred = Name (else :| []), \
    \condLogicBody = Constant (Number (Integer' 3))}]))}))"

--------------------------------------------------------------------------------
-- Type tests
--------------------------------------------------------------------------------

--------------------------------------------------
-- adt testing
--------------------------------------------------

sumTypeTest :: T.TestTree
sumTypeTest =
  shouldParseAs
    "sumTypeTest"
    Parser.typeP
    ( "type Foo a b c = | A b : a -> b -> c \n"
        <> "            | B d \n"
        <> "            | C { a : Int, #b : Int } \n"
        <> "            | D { a : Int, #b : Int } : Foo Int (Fooy -> Nada)"
    )
    "Typ {typeUsage = Nothing, typeName = Foo, typeArgs = [a,b,c], typeForm = Data \
    \(NonArrowed {dataAdt = Sum (S {sumConstructor = A, sumValue = Just (Arrow (Arrows \
    \(Arr (ArrGen {arrowGenName = Just (Concrete b), arrowGenData = NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (a :| []) [], typeRefineRefinement \
    \= Nothing}}, arrowGenArrow = ArrowUse w})) (Arrows (Arr (ArrGen {arrowGenName \
    \= Nothing, arrowGenData = NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (b :| []) [], typeRefineRefinement = Nothing}\
    \}, arrowGenArrow = ArrowUse w})) (Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (c :| []) [], typeRefineRefinement = Nothing}})))))} \
    \:| [S {sumConstructor = B, sumValue = Just (Arrow (Refined (NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (d :| []) [], typeRefineRefinement \
    \= Nothing}})))},S {sumConstructor = C, sumValue = Just (Record (Record' {recordFields \
    \= NameType {nameTypeSignature = Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Int :| []) [], typeRefineRefinement = Nothing}}), \
    \nameTypeName = Concrete a} :| [NameType {nameTypeSignature = Refined (NamedRefine \
    \{nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start \
    \(Int :| []) [], typeRefineRefinement = Nothing}}), nameTypeName = Implicit b}], recordFamilySignature \
    \= Nothing}))},S {sumConstructor = D, sumValue = Just (Record (Record' {recordFields \
    \= NameType {nameTypeSignature = Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Int :| []) [], typeRefineRefinement = Nothing}}), \
    \nameTypeName = Concrete a} :| [NameType {nameTypeSignature = Refined (NamedRefine \
    \{nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start \
    \(Int :| []) [], typeRefineRefinement = Nothing}}), nameTypeName = Implicit b}], recordFamilySignature \
    \= Just (NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName \
    \= Start (Foo :| []) [SymbolName (Int :| []),ArrowName (Arrows (Arr (ArrGen {arrowGenName \
    \= Nothing, arrowGenData = NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Fooy :| []) [], typeRefineRefinement = Nothing}}, \
    \arrowGenArrow = ArrowUse w})) (Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Nada :| []) [], typeRefineRefinement = Nothing}})))], \
    \typeRefineRefinement = Nothing}})}))}])})}"

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase :: T.TestTree
superArrowCase =
  shouldParseAs
    "superArrowCase"
    Parser.arrowType
    "( b : Bah -> \n c : B -o Foo) -> Foo a b -> a : Bah a c -o ( HAHAHHA -> foo )"
    "Parens (Paren (ArrGen {arrowGenName = Just (Concrete b), arrowGenData = Arrows \
    \(Arr (ArrGen {arrowGenName = Nothing, arrowGenData = NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (Bah :| []) [], \
    \typeRefineRefinement = Nothing}}, arrowGenArrow = ArrowUse w})) (Arrows (Arr (ArrGen \
    \{arrowGenName = Just (Concrete c), arrowGenData = NamedRefine {nameRefineName = Nothing, \
    \namedRefineRefine = TypeRefine {typeRefineName = Start (B :| []) [], typeRefineRefinement \
    \= Nothing}}, arrowGenArrow = ArrowUse 1})) (Refined (NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (Foo :| []) [], \
    \typeRefineRefinement = Nothing}}))), arrowGenArrow = ArrowUse w})) (Arrows (Arr (ArrGen \
    \{arrowGenName = Nothing, arrowGenData = NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Foo :| []) [SymbolName (a :| []),SymbolName \
    \(b :| [])], typeRefineRefinement = Nothing}}, arrowGenArrow = ArrowUse w})) (Arrows \
    \(Arr (ArrGen {arrowGenName = Just (Concrete a), arrowGenData = NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (Bah :| []) [SymbolName \
    \(a :| []),SymbolName (c :| [])], typeRefineRefinement = Nothing}}, arrowGenArrow \
    \= ArrowUse 1})) (End (Arrows (Arr (ArrGen {arrowGenName = Nothing, arrowGenData = \
    \NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName \
    \= Start (HAHAHHA :| []) [], typeRefineRefinement = Nothing}}, arrowGenArrow = ArrowUse \
    \w})) (Refined (NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine \
    \{typeRefineName = Start (foo :| []) [], typeRefineRefinement = Nothing}}))))))"

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest :: T.TestTree
typeTest =
  shouldParseAs
    "typeTest"
    Parser.typeP
    "type Foo a b c d = | Foo Bea"
    "Typ {typeUsage = Nothing, typeName = Foo, typeArgs = [a,b,c,d], typeForm = NewType \
    \(Declare {newTypeAlias = Foo, newTypeType' = NamedRefine {nameRefineName = Nothing, \
    \namedRefineRefine = TypeRefine {typeRefineName = Start (Bea :| []) [], typeRefineRefinement \
    \= Nothing}}})}"

--------------------------------------------------------------------------------
-- Modules test
--------------------------------------------------------------------------------

moduleOpen :: T.TestTree
moduleOpen =
  shouldParseAs
    "moduleOpen"
    Parser.topLevel
    ( ""
        <> "let Foo Int = \n"
        <> "  type T = Int.t \n"
        <> "  sig bah : T -> T \n"
        <> "  let bah t = Int.(t + 3) \n"
        <> "end"
    )
    "Module (Mod (Like {functionLikedName = Foo, functionLikeArgs = [ConcreteA (MatchLogic \
    \{matchLogicContents = MatchCon (Int :| []) [], matchLogicNamed = Nothing})], functionLikeBody \
    \= Body (Type (Typ {typeUsage = Nothing, typeName = T, typeArgs = [], typeForm = Alias \
    \(AliasDec {aliasType' = NamedRefine {nameRefineName = Nothing, namedRefineRefine \
    \= TypeRefine {typeRefineName = Start (Int :| [t]) [], typeRefineRefinement = Nothing}}})}) \
    \:| [Signature (Sig {signatureName = bah, signatureUsage = Nothing, signatureArrowType \
    \= Arrows (Arr (ArrGen {arrowGenName = Nothing, arrowGenData = NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (T :| []) [], typeRefineRefinement \
    \= Nothing}}, arrowGenArrow = ArrowUse w})) (Refined (NamedRefine {nameRefineName \
    \= Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start (T :| []) [], typeRefineRefinement \
    \= Nothing}})), signatureConstraints = []}),Function (Func (Like {functionLikedName \
    \= bah, functionLikeArgs = [ConcreteA (MatchLogic {matchLogicContents = MatchName \
    \t, matchLogicNamed = Nothing})], functionLikeBody = Body (OpenExpr (OpenExpress {moduleOpenExprModuleN \
    \= Int :| [], moduleOpenExprExpr = Infix (Inf {infixLeft = Name (t :| []), infixOp \
    \= + :| [], infixRight = Constant (Number (Integer' 3))})}))}))])}))"

moduleOpen' :: T.TestTree
moduleOpen' =
  shouldParseAs
    "moduleOpen'"
    Parser.topLevel
    ( ""
        <> "let Bah M = \n"
        <> "  open M"
        <> "  sig bah : Rec \n"
        <> "  let bah t = \n"
        <> "     { a = (t + 3)"
        <> "     , b = expr M.N.t}"
        <> "end"
    )
    "Module (Mod (Like {functionLikedName = Bah, functionLikeArgs = [ConcreteA (MatchLogic \
    \{matchLogicContents = MatchCon (M :| []) [], matchLogicNamed = Nothing})], functionLikeBody \
    \= Body (ModuleOpen (Open (M :| [])) :| [Signature (Sig {signatureName = bah, signatureUsage \
    \= Nothing, signatureArrowType = Refined (NamedRefine {nameRefineName = Nothing, \
    \namedRefineRefine = TypeRefine {typeRefineName = Start (Rec :| []) [], typeRefineRefinement \
    \= Nothing}}), signatureConstraints = []}),Function (Func (Like {functionLikedName \
    \= bah, functionLikeArgs = [ConcreteA (MatchLogic {matchLogicContents = MatchName \
    \t, matchLogicNamed = Nothing})], functionLikeBody = Body (ExpRecord (ExpressionRecord \
    \{expRecordFields = NonPunned (a :| []) (Infix (Inf {infixLeft = Name (t :| []), infixOp \
    \= + :| [], infixRight = Constant (Number (Integer' 3))})) :| [NonPunned (b :| []) (Application \
    \(App {applicationName = expr :| [], applicationArgs = Name (M :| [N,t]) :| []}))]}))}))])}))"

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse :: T.TestTree
typeNameNoUniverse =
  shouldParseAs
    "typeNameNoUniverse"
    Parser.typeNameParser
    "Foo a b c (b -o d) a c u"
    "Start (Foo :| []) [SymbolName (a :| []),SymbolName (b :| []),SymbolName (c :| \
    \[]),ArrowName (Arrows (Arr (ArrGen {arrowGenName = Nothing, arrowGenData = NamedRefine \
    \{nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName = Start \
    \(b :| []) [], typeRefineRefinement = Nothing}}, arrowGenArrow = ArrowUse 1})) (Refined \
    \(NamedRefine {nameRefineName = Nothing, namedRefineRefine = TypeRefine {typeRefineName \
    \= Start (d :| []) [], typeRefineRefinement = Nothing}}))),SymbolName (a :| []),SymbolName \
    \(c :| []),SymbolName (u :| [])]"

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon :: T.TestTree
simpleNamedCon =
  shouldParseAs
    "simpleNamedCon"
    Parser.matchLogic
    "foo@( Hi a b c )"
    "MatchLogic {matchLogicContents = MatchCon (Hi :| []) [MatchLogic {matchLogicContents \
    \= MatchName a, matchLogicNamed = Nothing},MatchLogic {matchLogicContents = MatchName \
    \b, matchLogicNamed = Nothing},MatchLogic {matchLogicContents = MatchName c, matchLogicNamed \
    \= Nothing}], matchLogicNamed = Just (foo :| [])}"

matchMoreComplex :: T.TestTree
matchMoreComplex =
  shouldParseAs
    "matchMoreComplex"
    Parser.matchLogic
    "foo@( Hi nah@{ a = nah , f } b 5 )"
    "MatchLogic {matchLogicContents = MatchCon (Hi :| []) [MatchLogic {matchLogicContents \
    \= MatchRecord (NonPunned (a :| []) (MatchLogic {matchLogicContents = MatchName \
    \nah, matchLogicNamed = Nothing}) :| [Punned (f :| [])]), matchLogicNamed = Just \
    \(nah :| [])},MatchLogic {matchLogicContents = MatchName b, matchLogicNamed = Nothing},MatchLogic \
    \{matchLogicContents = MatchConst (Number (Integer' 5)), matchLogicNamed = Nothing}], \
    \matchLogicNamed = Just (foo :| [])}"

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

condTest1 :: T.TestTree
condTest1 =
  shouldParseAs
    "condTest1"
    Parser.cond
    ( ""
        <> "if  | foo  = a\n"
        <> "    | else = b "
    )
    "C (CondExpression {condLogicPred = Name (foo :| []), condLogicBody = Name (a \
    \:| [])} :| [CondExpression {condLogicPred = Name (else :| []), condLogicBody \
    \= Name (b :| [])}])"

--------------------------------------------------
-- Record
--------------------------------------------------

record1 :: T.TestTree
record1 =
  shouldParseAs
    "record1"
    Parser.expression
    "{a, b = 3+5}"
    "ExpRecord (ExpressionRecord {expRecordFields = Punned (a :| []) :| [NonPunned \
    \(b :| []) (Infix (Inf {infixLeft = Constant (Number (Integer' 3)), infixOp \
    \= + :| [], infixRight = Constant (Number (Integer' 5))}))]})"

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 :: T.TestTree
parens1 =
  shouldParseAs
    "parens1"
    Parser.expression
    "(       ( \n(({a, b = 3+5})))\n)"
    "ExpRecord (ExpressionRecord {expRecordFields = Punned (a :| []) :| [NonPunned \
    \(b :| []) (Infix (Inf {infixLeft = Constant (Number (Integer' 3)), \
    \infixOp = + :| [], infixRight = Constant (Number (Integer' 5))}))]})"

--------------------------------------------------------------------------------
-- Spacer tests
--------------------------------------------------------------------------------

spacerSymb :: Bool
spacerSymb =
  case parse (Parser.spacer Parser.prefixSymbol) "Foo   f" of
    Done f s -> f == "f" && s == "Foo"
    _ -> False

--------------------------------------------------------------------------------
-- validPrefixSymbols
--------------------------------------------------------------------------------

vpsDashFrontFail :: Bool
vpsDashFrontFail =
  isLeft (parseOnly Parser.prefixSymbol "-Foo")

vpsDashMiddle :: Bool
vpsDashMiddle =
  isRight (parseOnly Parser.prefixSymbol "Foo-Foo")
