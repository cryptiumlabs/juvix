module Frontend where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Char8
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
      parens1,
      -- pre-processor tests
      removeNoComment,
      removeNewLineBefore,
      removeSpaceBefore
    ]

--------------------------------------------------------------------------------
-- Parser Checker
--------------------------------------------------------------------------------

space = Char8.char8 ' '

test =
  parseOnly
    (many' Parser.expressionSN)
    "let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
    \= 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
    \foo = 3 let foo = 3 let foo = 3 let foo = 3 "

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
-- Pre-processor test
--------------------------------------------------------------------------------

removeSpaceBefore :: T.TestTree
removeSpaceBefore =
  Parser.removeComments "let foo = 3 \n + \n -- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n\n 4")
    |> T.testCase "test remove comments: let foo = 3 \n + \n -- foo foo foo \n 4"

removeNewLineBefore :: T.TestTree
removeNewLineBefore =
  Parser.removeComments "let foo = 3 \n + \n-- foo foo foo \n 4"
    |> (T.@=? "let foo = 3 \n + \n\n 4")
    |> T.testCase "test remove comments: let foo = 3 \n + \n-- foo foo foo \n 4"

-- TODO :: use quick check!

removeNoComment :: T.TestTree
removeNoComment =
  let str = "let foo = 3 \n + \n \n 4"
   in Parser.removeComments str
        |> (T.@=? str)
        |> T.testCase ("test remove comments: " <> str)

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
    "[Function' (Func' (Like' {functionLikedName = foo, functionLikeArgs = [ConcreteA' \
    \(MatchLogic' {matchLogicContents = MatchName' a (), matchLogicNamed = Nothing, \
    \annMatchLogic = ()}) (),ConcreteA' (MatchLogic' {matchLogicContents = MatchName' \
    \b (), matchLogicNamed = Nothing, annMatchLogic = ()}) (),ConcreteA' (MatchLogic' \
    \{matchLogicContents = MatchName' c (), matchLogicNamed = Nothing, annMatchLogic \
    \= ()}) ()], functionLikeBody = Body' (Application' (App' {applicationName = \
    \Name' (+ :| []) (), applicationArgs = Parened' (Infix' (Inf' {infixLeft = Name' \
    \(a :| []) (), infixOp = + :| [], infixRight = Name' (b :| []) (), annInf = ()}) \
    \()) () :| [Name' (c :| []) ()], annApp = ()}) ()) (), annLike = ()}) ()) (),Function' \
    \(Func' (Like' {functionLikedName = bah, functionLikeArgs = [], functionLikeBody \
    \= Body' (Application' (App' {applicationName = Name' (foo :| []) (), applicationArgs \
    \= Constant' (Number' (Integer'' 1 ()) ()) () :| [Constant' (Number' (Integer'' \
    \2 ()) ()) (),Constant' (Number' (Integer'' 3 ()) ()) ()], annApp = ()}) ()) \
    \(), annLike = ()}) ()) (),Function' (Func' (Like' {functionLikedName = nah, \
    \functionLikeArgs = [], functionLikeBody = Guard' (C' (CondExpression' {condLogicPred \
    \= Infix' (Inf' {infixLeft = Name' (bah :| []) (), infixOp = == :| [], infixRight \
    \= Constant' (Number' (Integer'' 5 ()) ()) (), annInf = ()}) (), condLogicBody \
    \= Constant' (Number' (Integer'' 7 ()) ()) (), annCondExpression = ()} :| [CondExpression' \
    \{condLogicPred = Name' (else :| []) (), condLogicBody = Constant' (Number' (Integer'' \
    \11 ()) ()) (), annCondExpression = ()}]) ()) (), annLike = ()}) ()) (),Function' \
    \(Func' (Like' {functionLikedName = test, functionLikeArgs = [], functionLikeBody \
    \= Body' (Let' (Let''' {letBindings = Like' {functionLikedName = check, functionLikeArgs \
    \= [], functionLikeBody = Body' (Name' (nah :| []) ()) (), annLike = ()}, letBody \
    \= Match' (Match''' {matchOn = Name' (check :| []) (), matchBindigns = MatchL' \
    \{matchLPattern = MatchLogic' {matchLogicContents = MatchName' seven (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' 11 \
    \()) ()) (), annMatchL = ()} :| [MatchL' {matchLPattern = MatchLogic' {matchLogicContents \
    \= MatchName' eleven (), matchLogicNamed = Nothing, annMatchLogic = ()}, matchLBody \
    \= Constant' (Number' (Integer'' 7 ()) ()) (), annMatchL = ()},MatchL' {matchLPattern \
    \= MatchLogic' {matchLogicContents = MatchName' f (), matchLogicNamed = Nothing, \
    \annMatchLogic = ()}, matchLBody = OpenExpr' (OpenExpress' {moduleOpenExprModuleN \
    \= Fails :| [], moduleOpenExprExpr = Do' (Do''' (DoBody' {doBodyName = Nothing, \
    \doBodyExpr = Application' (App' {applicationName = Name' (print :| []) (), applicationArgs \
    \= Name' (failed :| []) () :| [], annApp = ()}) (), annDoBody = ()} :| [DoBody' \
    \{doBodyName = Nothing, doBodyExpr = Name' (fail :| []) (), annDoBody = ()}]) \
    \()) (), annOpenExpress = ()}) (), annMatchL = ()}], annMatch'' = ()}) (), annLet'' \
    \= ()}) ()) (), annLike = ()}) ()) ()]"

--------------------------------------------------------------------------------
-- Sig Test
--------------------------------------------------------------------------------

sigTest1 :: T.TestTree
sigTest1 =
  shouldParseAs
    "sigTest1"
    Parser.topLevel
    "sig foo 0 : Int -> Int"
    "Signature' (Sig' {signatureName = foo, signatureUsage = Just (Constant' (Number' \
    \(Integer'' 0 ()) ()) ()), signatureArrowType = Infix' (Inf' {infixLeft = Name' \
    \(Int :| []) (), infixOp = -> :| [], infixRight = Name' (Int :| []) (), annInf \
    \= ()}) (), signatureConstraints = [], annSig = ()}) ()"

sigTest2 :: T.TestTree
sigTest2 =
  shouldParseAs
    "sigTest2"
    Parser.topLevel
    "sig foo 0 : i : Int{i > 0} -> Int{i > 1}"
    "Signature' (Sig' {signatureName = foo, signatureUsage = Just (Constant' (Number' \
    \(Integer'' 0 ()) ()) ()), signatureArrowType = Infix' (Inf' {infixLeft = Name' \
    \(i :| []) (), infixOp = : :| [], infixRight = Infix' (Inf' {infixLeft = RefinedE' \
    \(TypeRefine' {typeRefineName = Name' (Int :| []) (), typeRefineRefinement = \
    \Infix' (Inf' {infixLeft = Name' (i :| []) (), infixOp = > :| [], infixRight \
    \= Constant' (Number' (Integer'' 0 ()) ()) (), annInf = ()}) (), annTypeRefine \
    \= ()}) (), infixOp = -> :| [], infixRight = RefinedE' (TypeRefine' {typeRefineName \
    \= Name' (Int :| []) (), typeRefineRefinement = Infix' (Inf' {infixLeft = Name' \
    \(i :| []) (), infixOp = > :| [], infixRight = Constant' (Number' (Integer'' \
    \1 ()) ()) (), annInf = ()}) (), annTypeRefine = ()}) (), annInf = ()}) (), annInf \
    \= ()}) (), signatureConstraints = [], annSig = ()}) ()"

--------------------------------------------------------------------------------
-- Function Testing
--------------------------------------------------------------------------------

fun1 :: T.TestTree
fun1 =
  shouldParseAs
    "fun1"
    Parser.topLevel
    "let f foo@(A b c d) = 3"
    "Function' (Func' (Like' {functionLikedName = f, functionLikeArgs = [ConcreteA' \
    \(MatchLogic' {matchLogicContents = MatchCon' (A :| []) [MatchLogic' {matchLogicContents \
    \= MatchName' b (), matchLogicNamed = Nothing, annMatchLogic = ()},MatchLogic' \
    \{matchLogicContents = MatchName' c (), matchLogicNamed = Nothing, annMatchLogic \
    \= ()},MatchLogic' {matchLogicContents = MatchName' d (), matchLogicNamed = Nothing, \
    \annMatchLogic = ()}] (), matchLogicNamed = Just foo, annMatchLogic = \
    \()}) ()], functionLikeBody = Body' (Constant' (Number' (Integer'' 3 ()) ()) \
    \()) (), annLike = ()}) ()) ()"

fun2 :: T.TestTree
fun2 =
  shouldParseAs
    "fun2"
    Parser.topLevel
    "let f foo | foo = 2 | else = 3"
    "Function' (Func' (Like' {functionLikedName = f, functionLikeArgs = [ConcreteA' \
    \(MatchLogic' {matchLogicContents = MatchName' foo (), matchLogicNamed = Nothing, \
    \annMatchLogic = ()}) ()], functionLikeBody = Guard' (C' (CondExpression' {condLogicPred \
    \= Name' (foo :| []) (), condLogicBody = Constant' (Number' (Integer'' 2 ()) \
    \()) (), annCondExpression = ()} :| [CondExpression' {condLogicPred = Name' (else \
    \:| []) (), condLogicBody = Constant' (Number' (Integer'' 3 ()) ()) (), annCondExpression \
    \= ()}]) ()) (), annLike = ()}) ()) ()"

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
    ( "type Foo a b c = | A : b : a -> b -> c \n"
        <> "            | B : d -> Foo \n"
        <> "            | C { a : Int, #b : Int } \n"
        <> "            | D { a : Int, #b : Int } : Foo Int (Fooy -> Nada)"
    )
    "Typ' {typeUsage = Nothing, typeName' = Foo, typeArgs = [a,b,c], typeForm = NonArrowed' \
    \{dataAdt = Sum' (S' {sumConstructor = A, sumValue = Just (Arrow' (Infix' (Inf' \
    \{infixLeft = Name' (b :| []) (), infixOp = : :| [], infixRight = Infix' (Inf' {infixLeft \
    \= Name' (a :| []) (), infixOp = -> :| [], infixRight = Infix' (Inf' {infixLeft = \
    \Name' (b :| []) (), infixOp = -> :| [], infixRight = Name' (c :| []) (), annInf = \
    \()}) (), annInf = ()}) (), annInf = ()}) ()) ()), annS = ()} :| [S' {sumConstructor \
    \= B, sumValue = Just (Arrow' (Infix' (Inf' {infixLeft = Name' (d :| []) (), infixOp \
    \= -> :| [], infixRight = Name' (Foo :| []) (), annInf = ()}) ()) ()), annS = ()},S' \
    \{sumConstructor = C, sumValue = Just (Record' (Record''' {recordFields = NameType'' \
    \{nameTypeSignature = Name' (Int :| []) (), nameTypeName = Concrete' a (), annNameType' \
    \= ()} :| [NameType'' {nameTypeSignature = Name' (Int :| []) (), nameTypeName = Implicit' \
    \b (), annNameType' = ()}], recordFamilySignature = Nothing, annRecord'' = ()}) ()), \
    \annS = ()},S' {sumConstructor = D, sumValue = Just (Record' (Record''' {recordFields \
    \= NameType'' {nameTypeSignature = Name' (Int :| []) (), nameTypeName = Concrete' \
    \a (), annNameType' = ()} :| [NameType'' {nameTypeSignature = Name' (Int :| []) (), \
    \nameTypeName = Implicit' b (), annNameType' = ()}], recordFamilySignature = Just \
    \(Application' (App' {applicationName = Name' (Foo :| []) (), applicationArgs = Name' \
    \(Int :| []) () :| [Parened' (Infix' (Inf' {infixLeft = Name' (Fooy :| []) (), infixOp \
    \= -> :| [], infixRight = Name' (Nada :| []) (), annInf = ()}) ()) ()], annApp = ()}) \
    \()), annRecord'' = ()}) ()), annS = ()}]) (), annNonArrowed = ()}, annTyp = ()}"

--------------------------------------------------
-- Arrow Testing
--------------------------------------------------

superArrowCase :: T.TestTree
superArrowCase =
  shouldParseAs
    "superArrowCase"
    Parser.expression
    "( b : Bah -> \n c : B -o Foo) -> Foo a b -> a : Bah a c -o ( HAHAHHA -> foo )"
    "Infix' (Inf' {infixLeft = Parened' (Infix' (Inf' {infixLeft = Name' (b :| []) \
    \(), infixOp = : :| [], infixRight = Infix' (Inf' {infixLeft = Name' (Bah :| \
    \[]) (), infixOp = -> :| [], infixRight = Infix' (Inf' {infixLeft = Name' (c \
    \:| []) (), infixOp = : :| [], infixRight = Infix' (Inf' {infixLeft = Name' (B \
    \:| []) (), infixOp = -o :| [], infixRight = Name' (Foo :| []) (), annInf = ()}) \
    \(), annInf = ()}) (), annInf = ()}) (), annInf = ()}) ()) (), infixOp = -> :| \
    \[], infixRight = Infix' (Inf' {infixLeft = Application' (App' {applicationName \
    \= Name' (Foo :| []) (), applicationArgs = Name' (a :| []) () :| [Name' (b :| \
    \[]) ()], annApp = ()}) (), infixOp = -> :| [], infixRight = Infix' (Inf' {infixLeft \
    \= Name' (a :| []) (), infixOp = : :| [], infixRight = Infix' (Inf' {infixLeft \
    \= Application' (App' {applicationName = Name' (Bah :| []) (), applicationArgs \
    \= Name' (a :| []) () :| [Name' (c :| []) ()], annApp = ()}) (), infixOp = -o \
    \:| [], infixRight = Parened' (Infix' (Inf' {infixLeft = Name' (HAHAHHA :| []) \
    \(), infixOp = -> :| [], infixRight = Name' (foo :| []) (), annInf = ()}) ()) \
    \(), annInf = ()}) (), annInf = ()}) (), annInf = ()}) (), annInf = ()}) ()"

--------------------------------------------------
-- alias tests
--------------------------------------------------

typeTest :: T.TestTree
typeTest =
  shouldParseAs
    "typeTest"
    Parser.topLevel
    "type Foo a b c d = | Foo nah bah sad"
    "Type' (Typ' {typeUsage = Nothing, typeName' = Foo, typeArgs = [a,b,c,d], typeForm \
    \= NonArrowed' {dataAdt = Sum' (S' {sumConstructor = Foo, sumValue = Just (ADTLike' \
    \[Name' (nah :| []) (),Name' (bah :| []) (),Name' (sad :| []) ()] ()), annS = ()} \
    \:| []) (), annNonArrowed = ()}, annTyp = ()}) ()"

--------------------------------------------------------------------------------
-- Modules test
--------------------------------------------------------------------------------

moduleOpen :: T.TestTree
moduleOpen =
  shouldParseAs
    "moduleOpen"
    Parser.topLevel
    ( ""
        <> "mod Foo Int = \n"
        <> "  let T = Int.t \n"
        <> "  sig bah : T -> T \n"
        <> "  let bah t = Int.(t + 3) \n"
        <> "end"
    )
    "Module' (Mod' (Like' {functionLikedName = Foo, functionLikeArgs = [ConcreteA' \
    \(MatchLogic' {matchLogicContents = MatchCon' (Int :| []) [] (), matchLogicNamed = \
    \Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' (Function' (Func' (Like' \
    \{functionLikedName = T, functionLikeArgs = [], functionLikeBody = Body' (Name' (Int \
    \:| [t]) ()) (), annLike = ()}) ()) () :| [Signature' (Sig' {signatureName = bah, \
    \signatureUsage = Nothing, signatureArrowType = Infix' (Inf' {infixLeft = Name' (T \
    \:| []) (), infixOp = -> :| [], infixRight = Name' (T :| []) (), annInf = ()}) (), \
    \signatureConstraints = [], annSig = ()}) (),Function' (Func' (Like' {functionLikedName \
    \= bah, functionLikeArgs = [ConcreteA' (MatchLogic' {matchLogicContents = MatchName' \
    \t (), matchLogicNamed = Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' \
    \(OpenExpr' (OpenExpress' {moduleOpenExprModuleN = Int :| [], moduleOpenExprExpr = \
    \Infix' (Inf' {infixLeft = Name' (t :| []) (), infixOp = + :| [], infixRight = Constant' \
    \(Number' (Integer'' 3 ()) ()) (), annInf = ()}) (), annOpenExpress = ()}) ()) (), \
    \annLike = ()}) ()) ()]) (), annLike = ()}) ()) ()"

moduleOpen' :: T.TestTree
moduleOpen' =
  shouldParseAs
    "moduleOpen'"
    Parser.topLevel
    ( ""
        <> "mod Bah M = \n"
        <> "  open M"
        <> "  sig bah : Rec \n"
        <> "  let bah t = \n"
        <> "     { a = t + 3"
        <> "     , b = expr M.N.t}"
        <> "end"
    )
    "Module' (Mod' (Like' {functionLikedName = Bah, functionLikeArgs = [ConcreteA' \
    \(MatchLogic' {matchLogicContents = MatchCon' (M :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' (ModuleOpen' (Open' \
    \(M :| []) ()) () :| [Signature' (Sig' {signatureName = bah, signatureUsage = \
    \Nothing, signatureArrowType = Name' (Rec :| []) (), signatureConstraints = [], \
    \annSig = ()}) (),Function' (Func' (Like' {functionLikedName = bah, functionLikeArgs \
    \= [ConcreteA' (MatchLogic' {matchLogicContents = MatchName' t (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}) ()], functionLikeBody = Body' (ExpRecord' (ExpressionRecord' \
    \{expRecordFields = NonPunned' (a :| []) (Infix' (Inf' {infixLeft = Name' (t \
    \:| []) (), infixOp = + :| [], infixRight = Constant' (Number' (Integer'' 3 ()) \
    \()) (), annInf = ()}) ()) () :| [NonPunned' (b :| []) (Application' (App' {applicationName \
    \= Name' (expr :| []) (), applicationArgs = Name' (M :| [N,t]) () :| [], annApp \
    \= ()}) ()) ()], annExpressionRecord = ()}) ()) (), annLike = ()}) ()) ()]) (), \
    \annLike = ()}) ()) ()"

--------------------------------------------------
-- typeName tests
--------------------------------------------------

typeNameNoUniverse :: T.TestTree
typeNameNoUniverse =
  shouldParseAs
    "typeNameNoUniverse"
    Parser.expression
    "Foo a b c (b -o d) a c u"
    "Application' (App' {applicationName = Name' (Foo :| []) (), applicationArgs \
    \= Name' (a :| []) () :| [Name' (b :| []) (),Name' (c :| []) (),Parened' (Infix' \
    \(Inf' {infixLeft = Name' (b :| []) (), infixOp = -o :| [], infixRight = Name' \
    \(d :| []) (), annInf = ()}) ()) (),Name' (a :| []) (),Name' (c :| []) (),Name' \
    \(u :| []) ()], annApp = ()}) ()"

--------------------------------------------------------------------------------
-- Match tests
--------------------------------------------------------------------------------

simpleNamedCon :: T.TestTree
simpleNamedCon =
  shouldParseAs
    "simpleNamedCon"
    Parser.matchLogic
    "foo@( Hi a b c )"
    "MatchLogic' {matchLogicContents = MatchCon' (Hi :| []) [MatchLogic' {matchLogicContents \
    \= MatchName' a (), matchLogicNamed = Nothing, annMatchLogic = ()},MatchLogic' \
    \{matchLogicContents = MatchName' b (), matchLogicNamed = Nothing, annMatchLogic \
    \= ()},MatchLogic' {matchLogicContents = MatchName' c (), matchLogicNamed = Nothing, \
    \annMatchLogic = ()}] (), matchLogicNamed = Just foo, annMatchLogic = \
    \()}"

matchMoreComplex :: T.TestTree
matchMoreComplex =
  shouldParseAs
    "matchMoreComplex"
    Parser.matchLogic
    "foo@( Hi nah@{ a = nah , f } b 5 )"
    "MatchLogic' {matchLogicContents = MatchCon' (Hi :| []) [MatchLogic' {matchLogicContents \
    \= MatchRecord' (NonPunned' (a :| []) (MatchLogic' {matchLogicContents = MatchName' \
    \nah (), matchLogicNamed = Nothing, annMatchLogic = ()}) () :| [Punned' (f :| \
    \[]) ()]) (), matchLogicNamed = Just nah, annMatchLogic = ()},MatchLogic' \
    \{matchLogicContents = MatchName' b (), matchLogicNamed = Nothing, annMatchLogic \
    \= ()},MatchLogic' {matchLogicContents = MatchConst' (Number' (Integer'' 5 ()) \
    \()) (), matchLogicNamed = Nothing, annMatchLogic = ()}] (), matchLogicNamed \
    \= Just foo, annMatchLogic = ()}"

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
    "C' (CondExpression' {condLogicPred = Name' (foo :| []) (), condLogicBody = Name' \
    \(a :| []) (), annCondExpression = ()} :| [CondExpression' {condLogicPred = Name' \
    \(else :| []) (), condLogicBody = Name' (b :| []) (), annCondExpression = ()}]) ()"

--------------------------------------------------
-- Record
--------------------------------------------------

record1 :: T.TestTree
record1 =
  shouldParseAs
    "record1"
    Parser.expression
    "{a, b = 3+5}"
    "ExpRecord' (ExpressionRecord' {expRecordFields = Punned' (a :| []) () :| [NonPunned' \
    \(b :| []) (Infix' (Inf' {infixLeft = Constant' (Number' (Integer'' 3 ()) ()) \
    \(), infixOp = + :| [], infixRight = Constant' (Number' (Integer'' 5 ()) ()) \
    \(), annInf = ()}) ()) ()], annExpressionRecord = ()}) ()"

--------------------------------------------------
-- parens
--------------------------------------------------

parens1 :: T.TestTree
parens1 =
  shouldParseAs
    "parens1"
    Parser.expression
    "(       ( \n(({a, b = 3+5})))\n)"
    "Parened' (Parened' (Parened' (Parened' (ExpRecord' (ExpressionRecord' {expRecordFields \
    \= Punned' (a :| []) () :| [NonPunned' (b :| []) (Infix' (Inf' {infixLeft = Constant' \
    \(Number' (Integer'' 3 ()) ()) (), infixOp = + :| [], infixRight = Constant' \
    \(Number' (Integer'' 5 ()) ()) (), annInf = ()}) ()) ()], annExpressionRecord \
    \= ()}) ()) ()) ()) ()) ()"

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
