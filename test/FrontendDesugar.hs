module FrontendDesugar where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Char8
import qualified Juvix.Frontend.Parser as Parser
import Juvix.FrontendDesugar
import Juvix.Library hiding (show)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String, show)

allDesugar :: T.TestTree
allDesugar =
  T.testGroup
    "desugar Tests"
    [ guardTest
    ]

desugarTasty ::
  (Show a1, Show a2, Eq a1) => T.TestName -> Either a1 a2 -> String -> T.TestTree
desugarTasty name x y =
  T.testGroup
    "desugar Test"
    [ T.testCase (name <> " parser failed") (isRight x T.@=? True),
      T.testCase
        ("desugar: " <> name <> " " <> " should desugar to " <> y)
        (fmap show x T.@=? Right y)
    ]

shouldDesugar :: T.TestName -> ByteString -> String -> T.TestTree
shouldDesugar name parseString =
  desugarTasty name (fmap desugar (parseOnly (many Parser.topLevelSN) parseString))

guardTest :: T.TestTree
guardTest =
  shouldDesugar
    "guardTest"
    "let foo | x == 3 = 3 | else = 2"
    "[Function' (FunctionX (foo,FunctionLikeX {extFunctionLike = ([],Match' \
    \(Match''' {matchOn = Infix' (Inf' {infixLeft = Name' (x :| []) (), \
    \infixOp = == :| [], infixRight = Constant' (Number' (Integer'' 3 ()) \
    \()) (), annInf = ()}) (), matchBindigns = MatchL' {matchLPattern = \
    \MatchLogic' {matchLogicContents = MatchCon' (True :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' \
    \3 ()) ()) (), annMatchL = ()} :| [MatchL' {matchLPattern = MatchLogic' \
    \{matchLogicContents = MatchCon' (False :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Match' (Match''' {matchOn \
    \= Name' (else :| []) (), matchBindigns = MatchL' {matchLPattern = \
    \MatchLogic' {matchLogicContents = MatchCon' (True :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' \
    \2 ()) ()) (), annMatchL = ()} :| [MatchL' {matchLPattern = MatchLogic' \
    \{matchLogicContents = MatchCon' (False :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Match' (Match''' {matchOn \
    \= Name' (else :| []) (), matchBindigns = MatchL' {matchLPattern = \
    \MatchLogic' {matchLogicContents = MatchCon' (True :| []) [] (), matchLogicNamed \
    \= Nothing, annMatchLogic = ()}, matchLBody = Constant' (Number' (Integer'' \
    \2 ()) ()) (), annMatchL = ()} :| [], annMatch'' = ()}) (), annMatchL \
    \= ()}], annMatch'' = ()}) (), annMatchL = ()}], annMatch'' = ()}) \
    \())} :| [],Nothing)) ()]"
